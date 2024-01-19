const std = @import("std");
const Chunk = @import("chunk.zig").Chunk;
const tokens = @import("tokens.zig");
const Object = @import("object.zig").Object;
const String = @import("string.zig").String;
const arg_parser = @import("arg_parser.zig");
const Opcodes = @import("opcodes.zig").Opcodes;
const Parser = @import("compiler.zig").Parser;
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

const FRAMES_MAX = 512;

const CallFrame = struct {
    function: Function,
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
        var compiler = Parser.init(self.options, self.allocator);
        const function = compiler.compile(source) catch return InterpreterError.Compile;

        const frame = CallFrame {
            .function = function,
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
        while (true) {
            const frame = self.get_current_frame();
            const chunk = &frame.function.chunk;
            if (self.options.args & @intFromEnum(Option.DebugTrace) != 0) 
                try self.log_trace();
            const instruction: Opcodes = @enumFromInt(chunk.code.items[frame.ip]);
            switch (instruction) {
                Opcodes.Return => {
                    const value = self.pop();
                    const f = self.frames.pop(); // TODO: deinit
                    if (self.frame_len() == 0) return;
                    self.stack.resize(f.slot_start - 1) catch 
                    return InterpreterError.AllocationError;
                    try self.push(value);
                },
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

                Opcodes.Call => {
                    const arg_count = self.read_byte();
                    try self.call_value(self.peek(arg_count), arg_count);
                },

                else => return InterpreterError.Interpreter
            }
            frame.ip += 1;
        }
    }

    fn call_value(self: *Self, callee: Object, count: u8) InterpreterError!void {
        if (!@constCast(&callee).is_fn()) 
            return self.error_at("'{s}' is not callable", .{callee.type_name()});

        switch (callee) {
            Object.Fn => |func| try self.call(func, count),
            else => return self.error_at("can only call objects", .{}),
        }
    }

    fn call(self: *Self, function: Function, count: u8) InterpreterError!void {
        if (function.arity != count)
            return self.error_at("expected {d} arguments got {}", .{function.arity, count});

        if (self.frames.items.len == FRAMES_MAX)
            return self.error_at("hit recursion limit", .{});

        const frame = CallFrame {
            .function = function,
            .ip = 0,
            .slot_start = self.stack_top() - count,
        };

        self.frames.append(frame) catch return InterpreterError.AllocationError;
    }

    fn get_local(self: *Self) InterpreterError!void {
        const frame = self.get_current_frame();
        const index = self.read_index();
        try self.push(self.stack.items[frame.slot_start + index]);
    }

    fn set_local(self: *Self) InterpreterError!void {
        const frame = self.get_current_frame();
        const index = self.read_index();
        self.stack.items[frame.slot_start + index] = self.peek(0);
    }

    fn def_global(self: *Self) InterpreterError!void {
        var name = self.read_object();
        self.globals.put(name.Str.as_slice(), self.peek(0)) catch 
            return InterpreterError.AllocationError;
        _ = self.pop();
    }

    fn get_global(self: *Self) InterpreterError!void {
        const name = self.read_object();
        if (self.globals.get(@constCast(&name.Str).as_slice())) |value| {
            try self.push(value);
        } else {
            return self.error_at("undefined variable '{s}'", .{@constCast(&name.Str).as_slice()});
        }
    }

    fn set_global(self: *Self) InterpreterError!void {
        const name = self.read_object();
        if (self.globals.contains(@constCast(&name.Str).as_slice())) {
            self.globals.put(@constCast(&name.Str).as_slice(), self.peek(0)) catch 
                return InterpreterError.AllocationError;

        } else {
            return self.error_at(
                "undefined variable '{s}'", 
                .{@constCast(&name.Str).as_slice()}
            );
        }
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
        std.debug.print("stack trace: ", .{});
        for (self.stack.items) |slot| {
            std.debug.print("[ {} ]", .{slot});
        }
        std.debug.print("\n", .{});

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
        var names = ArrayList([]const u8).init(self.allocator);
        var spans = ArrayList(Span).init(self.allocator);
        for (self.frames.items, 0..) |frame, i| {
            names.append(frame.function.name) catch return InterpreterError.AllocationError;
            if (@constCast(&frame.function.chunk).get_line(frame.ip - 1)) |line| {
                if (self.get_source(line, i)) |span| {
                    spans.append(span) catch return InterpreterError.AllocationError;
                }
            }
        }

        Logger.interpreter_traceback_call(spans.items, names.items, msg, args) catch 
        return InterpreterError.AllocationError;
        return InterpreterError.Interpreter;
    }

    fn get_source(self: *Self, line: usize, index: usize) ?Span {
        const frame = self.frames.items[index];
        for (frame.function.source_map.items) |span| {
            if (span.line == line) return span;
        }
        return null;
    }

    fn get_current_frame(self: *Self) *CallFrame {
        return @constCast(&self.frames.items[self.frames.items.len - 1]);
    }

    fn get_current_chunk(self: *Self) *Chunk {
        return @constCast(&self.frames.items[self.frames.items.len - 1].function.chunk);
    }

    fn read_byte(self: *Self) u8 {
        var frame = self.get_current_frame();
        const byte = self.get_current_chunk().code.items[frame.ip + 1];
        frame.ip += 1;
        return byte;
    }

    fn read_object(self: *Self) Object {
        var frame = self.get_current_frame();
        var change: usize = 0;
        const value = self.get_current_chunk().read_constant(frame.ip, &change);
        frame.ip += change - 1;
        return value;
    }

    fn read_index(self: *Self) usize {
        var frame = self.get_current_frame();
        var change: usize = 0;
        const value = self.get_current_chunk().read_index(frame.ip, &change);
        frame.ip += change - 1;
        return value;
    }

    fn log_error(self: *Self, span: *const Span, comptime fmt: []const u8, args: anytype) void {
        const message = std.fmt.allocPrint(self.allocator, fmt, args) catch return;
        Logger.interpreter_error(@constCast(span), message) catch return;
        self.allocator.free(message);
    }

    fn stack_top(self: *Self) usize {
        return self.stack.items.len;
    }

    fn log_error_simple(self: *Self, comptime fmt: []const u8, args: anytype) void {
        const message = std.fmt.allocPrint(self.allocator, fmt, args) catch return;
        Logger.log(Level.Error, message) catch return;
        self.allocator.free(message);
    }
};
