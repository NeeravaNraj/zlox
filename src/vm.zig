const std = @import("std");
const Chunk = @import("chunk.zig").Chunk;
const tokens = @import("tokens.zig");
const Object = @import("object.zig").Object;
const String = @import("string.zig").String;
const arg_parser = @import("arg_parser.zig");
const Opcodes = @import("opcodes.zig").Opcodes;
const Compiler = @import("compiler.zig").Compiler;
const Disassembler = @import("disassembler.zig").Disassembler;
const Function = @import("function.zig").Function;
const logger = @import("logger.zig");
const Logger = logger.Logger;
const Level = logger.Level;
const Span = tokens.Span;
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const Option = arg_parser.Option;
const Options = arg_parser.Options;
const HashMap = std.StringHashMap;

const CallFrame = struct {
    function: *Function,
    ip: usize,
    // this value tells the callframe where its variables start from
    slot_start: usize,
};

pub const Vm = struct {
    const Self = @This();
    const InterpreterError = error {
        Compile,
        Interpreter,
        AllocationError,
        Trace,
    };

    options: Options,
    stack: ArrayList(Object),
    globals: HashMap(Object),
    frames: ArrayList(CallFrame),
    source_map: ArrayList(Span),
    disassembler: Disassembler,
    allocator: Allocator,

    pub fn init(options: Options, allocator: Allocator) Self {
        return Self {
            .stack = ArrayList(Object).init(allocator),
            .globals = HashMap(Object).init(allocator),
            .frames = ArrayList(CallFrame).init(allocator),
            .source_map = undefined,
            .options = options,
            .allocator = allocator,
            .disassembler = Disassembler.init(),
        };
    }

    pub fn interpret(self: *Self, source: []const u8) InterpreterError!void {
        var compiler = Compiler.init(self.options, self.allocator) catch 
            return InterpreterError.AllocationError;
        const function = compiler.compile(source) catch return InterpreterError.Compile;

        const frame = CallFrame {
            .function = @constCast(&function),
            .ip = 0,
            .slot_start = 1,
        };

        try self.push(Object{ .Fn = function });
        try self.push_frame(frame);
        defer {
            self.deinit();
        }

        try self.run();
    }

    fn run(self: *Self) InterpreterError!void {
        const frame = self.get_current_frame();
        const chunk = &frame.function.chunk;
        while (true) {
            if (self.options.args & @intFromEnum(Option.DebugTrace) != 0) 
                try self.log_trace();
            const instruction: Opcodes = @enumFromInt(chunk.code.items[frame.ip]);
            switch (instruction) {
                Opcodes.Return => return,
                Opcodes.Print => {
                    const value = self.pop();
                    std.debug.print("{}\n", .{value});
                },
                Opcodes.Constant => {
                    var change: usize = 0;
                    const value = chunk.read_constant(frame.ip, &change);
                    try self.push(value);
                    // jump over the value region
                    frame.ip += change - 1;
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
                Opcodes.SetGlobal => try self.set_global(),
                Opcodes.GetLocal => try self.get_local(),
                Opcodes.SetLocal => try self.set_local(),

                else => return InterpreterError.Interpreter
            }
            frame.ip += 1;
        }
    }

    fn get_local(self: *Self) InterpreterError!void {
        const frame = self.get_current_frame();
        var chunk = frame.function.chunk;
        var change: usize = 0;
        const index = chunk.read_index(frame.ip, &change);
        try self.push(self.stack.items[frame.slot_start + index]);
        frame.ip += change - 1;
    }

    fn set_local(self: *Self) InterpreterError!void {
        const frame = self.get_current_frame();
        var chunk = frame.function.chunk;
        var change: usize = 0;
        const index = chunk.read_index(frame.ip, &change);
        self.stack.items[frame.slot_start + index] = self.peek(0);
        frame.ip += change - 1;
    }

    fn def_global(self: *Self) InterpreterError!void {
        var frame = self.get_current_frame();
        var chunk = frame.function.chunk;
        var change: usize = 0;
        var name = chunk.read_constant(frame.ip, &change);
        self.globals.put(name.Str.as_slice(), self.peek(0)) catch 
            return InterpreterError.AllocationError;
        _ = self.pop();
        frame.ip += change - 1;
    }

    fn get_global(self: *Self) InterpreterError!void {
        var frame = self.get_current_frame();
        var chunk = frame.function.chunk;
        var change: usize = 0;
        const name = chunk.read_constant(frame.ip, &change);
        if (self.globals.get(@constCast(&name.Str).as_slice())) |value| {
            try self.push(value);
        } else {
            return self.error_at("undefined variable '{s}'", .{@constCast(&name.Str).as_slice()});
        }
        frame.ip += change - 1;
    }

    fn set_global(self: *Self) InterpreterError!void {
        var frame = self.get_current_frame();
        var chunk = frame.function.chunk;
        var change: usize = 0;
        const name = chunk.read_constant(frame.ip, &change);
        if (self.globals.contains(@constCast(&name.Str).as_slice())) {
            self.globals.put(@constCast(&name.Str).as_slice(), self.peek(0)) catch 
                return InterpreterError.AllocationError;

        } else {
            return self.error_at(
                "undefined variable '{s}'", 
                .{@constCast(&name.Str).as_slice()}
            );
        }
        frame.ip += change - 1;
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
                return self.error_at(
                    "cannot compare types '{s}' and '{s}'", 
                    .{lhs.type_name(), rhs.type_name()}
                );
            };
        } else vale: {
            break :vale lhs.lt(rhs) catch {
                return self.error_at(
                    "cannot compare types '{s}' and '{s}'", 
                    .{lhs.type_name(), rhs.type_name()}
                );
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
                return self.error_at(
                    "cannot compare types '{s}' and '{s}'",
                    .{lhs.type_name(), rhs.type_name()}
                );
            };
        } else vale: {
            break :vale lhs.le(rhs) catch {
                return self.error_at(
                    "cannot compare types '{s}' and '{s}'", 
                    .{lhs.type_name(), rhs.type_name()}
                );
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
        var frame = self.get_current_frame();
        std.debug.print("stack trace: [", .{});
        for (self.stack.items, frame.slot_start..) |slot, i| {
            std.debug.print("{}", .{slot});
            if (self.stack.items.len > i)
                std.debug.print(", ", .{});
        }
        std.debug.print("]\n", .{});

        _ = self
            .disassembler
            .disassemble_instruction(&frame.function.chunk, frame.ip) catch 
            return InterpreterError.Trace;
    }

    fn pop(self: *Self) Object {
        return self.stack.pop();
    }

    fn push(self: *Self, value: Object) InterpreterError!void {
        self.stack.append(value) catch return InterpreterError.AllocationError;
    }

    fn push_frame(self: *Self, value: CallFrame) InterpreterError!void {
        self.frames.append(value) catch return InterpreterError.AllocationError;
    }

    fn frame_len(self: *Self) usize {
        return self.frames.items.len;
    }

    fn peek(self: *Self, distance: usize) Object {
        return self.stack.items[self.stack.items.len - 1 - distance];
    }

    fn reset_stack(self: *Self) void {
        self.stack.clearRetainingCapacity();
    }

    fn deinit(self: *Self) void {
        self.frames.deinit();
        self.stack.deinit();
    }

    fn error_at(self: *Self, comptime msg: []const u8, args: anytype) InterpreterError {
        var frame = self.get_current_frame();
        if (frame.function.chunk.get_line(frame.ip)) |line| {
            if (self.get_source(line)) |span| {
                self.log_error(&span, msg, args);
                return InterpreterError.Interpreter;
            }
        }
        self.log_error_simple(msg, args);
        return InterpreterError.Interpreter;
    }

    fn get_source(self: *Self, line: usize) ?Span {
        const frame = self.get_current_frame();
        for (frame.function.source_map.items) |span| {
            if (span.line == line) return span;
        }
        return null;
    }

    fn get_current_frame(self: *Self) *CallFrame {
        return @constCast(&self.frames.items[self.frames.items.len - 1]);
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
