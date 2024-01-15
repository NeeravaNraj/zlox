const std = @import("std");
const Chunk = @import("chunk.zig").Chunk;
const tokens = @import("tokens.zig");
const Object = @import("object.zig").Object;
const String = @import("string.zig").String;
const arg_parser = @import("arg_parser.zig");
const Opcodes = @import("opcodes.zig").Opcodes;
const Compiler = @import("compiler.zig").Compiler;
const Disassembler = @import("disassembler.zig").Disassembler;
const logger = @import("logger.zig");
const Logger = logger.Logger;
const Level = logger.Level;
const Span = tokens.Span;
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const Option = arg_parser.Option;
const Options = arg_parser.Options;
const HashMap = std.StringHashMap;

pub const Vm = struct {
    const Self = @This();
    const InterpreterError = error {
        Compile,
        Interpreter,
        AllocationError,
        Trace,
    };

    file: []const u8,
    ip: usize,
    chunk: Chunk,
    options: Options,
    stack: ArrayList(Object),
    globals: HashMap(Object),
    source_map: ArrayList(Span),
    disassembler: Disassembler,
    allocator: Allocator,

    pub fn init(file: []const u8, options: Options, allocator: Allocator) Self {
        return Self {
            .file = file,
            .chunk = Chunk.init(allocator),
            .ip = 0,
            .stack = ArrayList(Object).init(allocator),
            .globals = HashMap(Object).init(allocator),
            .source_map = undefined,
            .options = options,
            .allocator = allocator,
            .disassembler = Disassembler.init(),
        };
    }

    pub fn interpret(self: *Self, source: []const u8) InterpreterError!void {
        var compiler = Compiler.init(self.file, &self.chunk, self.options, self.allocator);
        self.source_map = compiler.compile(source) catch return InterpreterError.Compile;

        defer {
            self.deinit();
            compiler.deinit();
        }

        try self.run();
    }

    fn run(self: *Self) InterpreterError!void {
        while (true) {
            if (self.options.args & @intFromEnum(Option.DebugTrace) != 0) try self.log_trace();
            const instruction: Opcodes = @enumFromInt(self.chunk.code.items[self.ip]);
            switch (instruction) {
                Opcodes.Return => return,
                Opcodes.Print => {
                    const value = self.pop();
                    std.debug.print("{}\n", .{value});
                },
                Opcodes.Constant => {
                    var change: usize = 0;
                    const value = self.chunk.read_constant(self.ip, &change);
                    try self.push(value);
                    // jump over the value region
                    self.ip += change - 1;
                },
                Opcodes.Negate => {
                    const value = self.pop().negate() catch {
                        // TODO: log error using source mapping
                        return InterpreterError.Interpreter;
                    };
                    try self.push(value);
                },
                Opcodes.Add => try self.add(),
                Opcodes.Subtract => try self.sub(),
                Opcodes.Multiply => try self.mul(),
                Opcodes.Divide => try self.div(),
                Opcodes.Equals => try self.equals(false),
                Opcodes.NotEquals => try self.equals(true),
                Opcodes.Greater => try self.compare(true),
                Opcodes.Less => try self.compare(false),
                Opcodes.GreaterEquals => try self.compare_equal(true),
                Opcodes.LessEquals => try self.compare_equal(false),

                Opcodes.Not => try self.push(self.pop().not()),
                Opcodes.None => try self.push(.None),
                Opcodes.True => try self.push(Object{ .Bool = true }),
                Opcodes.False => try self.push(Object{ .Bool = false }),

                Opcodes.Pop => _ = self.pop(),

                Opcodes.DefGlobal => try self.def_global(),
                Opcodes.GetGlobal => try self.get_global(),

                else => return InterpreterError.Interpreter
            }
            self.ip += 1;
        }
    }

    fn def_global(self: *Self) InterpreterError!void {
        var change: usize = 0;
        var name = self.chunk.read_constant(self.ip, &change);
        self.globals.put(name.Str.as_slice(), self.peek(0)) catch 
            return InterpreterError.AllocationError;
        _ = self.pop();
        self.ip += change - 1;
    }

    fn get_global(self: *Self) InterpreterError!void {
        var change: usize = 0;
        const name = self.chunk.read_constant(self.ip, &change);
        if (self.globals.get(@constCast(&name.Str).as_slice())) |value| {
            try self.push(value);
        } else {
            return self.error_at("undefined variable '{s}'", .{@constCast(&name.Str).as_slice()});
        }
        self.ip += change - 1;
    }

    fn equals(self: *Self, not: bool) InterpreterError!void {
        const rhs = self.pop();
        const lhs = self.pop();

        const value = lhs.equals(rhs) catch {
            return self.error_at("cannot equate types '{s}' and '{s}'", .{lhs.type_name(), rhs.type_name()});
        };

        if (not) {
            try self.push(value.not());
        } else try self.push(value);
    }

    fn compare(self: *Self, greater: bool) InterpreterError!void {
        const rhs = self.pop();
        const lhs = self.pop();

        const value = if (greater) val: {
            break :val lhs.gt(rhs) catch {
                return self.error_at("cannot compare types '{s}' and '{s}'", .{lhs.type_name(), rhs.type_name()});
            };
        } else vale: {
            break :vale lhs.lt(rhs) catch {
                return self.error_at("cannot compare types '{s}' and '{s}'", .{lhs.type_name(), rhs.type_name()});
            };
        };

        try self.push(value);
    }

    fn compare_equal(self: *Self, greater: bool) InterpreterError!void {
        const rhs = self.pop();
        const lhs = self.pop();
        // TODO: log errors
        const value = if (greater) val: {
            break :val lhs.ge(rhs) catch {
                return self.error_at("cannot compare types '{s}' and '{s}'", .{lhs.type_name(), rhs.type_name()});
            };
        } else vale: {
            break :vale lhs.le(rhs) catch {
                return self.error_at("cannot compare types '{s}' and '{s}'", .{lhs.type_name(), rhs.type_name()});
            };
        };

        try self.push(value);
    }

    fn add(self: *Self) InterpreterError!void {
        const rhs = self.pop();
        const lhs = self.pop();
        const value = lhs.add(rhs) catch {
            return self.error_at("cannot add types '{s}' and '{s}'", .{lhs.type_name(), rhs.type_name()});
        };

        try self.push(value);
    }

    fn sub(self: *Self) InterpreterError!void {
        const rhs = self.pop();
        const lhs = self.pop();
        const value = lhs.sub(rhs) catch {
            return self.error_at("cannot add types '{s}' and '{s}'", .{lhs.type_name(), rhs.type_name()});
        };

        try self.push(value);
    }

    fn mul(self: *Self) InterpreterError!void {
        const rhs = self.pop();
        const lhs = self.pop();
        const value = lhs.mul(rhs) catch {
            return self.error_at("cannot multiply types '{s}' and '{s}'", .{lhs.type_name(), rhs.type_name()});
        };

        try self.push(value);
    }

    fn div(self: *Self) InterpreterError!void {
        const rhs = self.pop();
        const lhs = self.pop();
        const value = lhs.div(rhs) catch {
            return self.error_at("cannot divde types '{s}' and '{s}'", .{lhs.type_name(), rhs.type_name()});
        };

        try self.push(value);
    }

    fn log_trace(self: *Self) InterpreterError!void {
        std.debug.print("stack trace: [", .{});
        for (self.stack.items, 0..) |slot, i| {
            std.debug.print("{}", .{slot});
            if (self.stack.items.len > i)
                std.debug.print(", ", .{});
        }
        std.debug.print("]\n", .{});

        _ = self
            .disassembler
            .disassemble_instruction(&self.chunk, self.ip) catch 
            return InterpreterError.Trace;
    }

    fn pop(self: *Self) Object {
        return self.stack.pop();
    }

    fn push(self: *Self, value: Object) InterpreterError!void {
        self.stack.append(value) catch return InterpreterError.AllocationError;
    }

    fn peek(self: *Self, distance: usize) Object {
        return self.stack.items[self.stack.items.len - 1 - distance];
    }

    fn reset_stack(self: *Self) void {
        self.stack.clearRetainingCapacity();
    }

    fn deinit(self: *Self) void {
        self.chunk.deinit();
        self.stack.deinit();
    }

    fn error_at(self: *Self, comptime msg: []const u8, args: anytype) InterpreterError {
        if (self.chunk.get_line(self.ip)) |line| {
            if (self.get_source(line)) |span| {
                self.log_error(&span, msg, args);
                return InterpreterError.Interpreter;
            }
        }
        self.log_error_simple(msg, args);
        return InterpreterError.Interpreter;
    }

    fn get_source(self: *Self, line: usize) ?Span {
        for (self.source_map.items) |span| {
            if (span.line == line) return span;
        }
        return null;
    }

    fn log_error(self: *Self, span: *const Span, comptime fmt: []const u8, args: anytype) void {
        const message = std.fmt.allocPrint(self.allocator, fmt, args) catch return;
        Logger.interpreter_error(@constCast(span), message) catch return;
        self.allocator.free(message);
    }

    fn log_error_simple(self: *Self, comptime fmt: []const u8, args: anytype) void {
        const message = std.fmt.allocPrint(self.allocator, fmt, args) catch return;
        Logger.log(Level.Error, message) catch return;
        self.allocator.free(message);
    }
};
