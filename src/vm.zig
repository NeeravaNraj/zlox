const std = @import("std");
const Chunk = @import("chunk.zig").Chunk;
const Object = @import("object.zig").Object;
const String = @import("string.zig").String;
const arg_parser = @import("arg_parser.zig");
const Opcodes = @import("opcodes.zig").Opcodes;
const Compiler = @import("compiler.zig").Compiler;
const Disassembler = @import("disassembler.zig").Disassembler;
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const Option = arg_parser.Option;
const Options = arg_parser.Options;

pub const Vm = struct {
    const Self = @This();
    const InterpreterError = error {
        Compile,
        Interpreter,
        AllocationError,
        Trace,
    };

    file: []const u8,
    chunk: Chunk,
    options: Options,
    stack: ArrayList(Object),
    ip: usize,
    disassembler: Disassembler,
    allocator: Allocator,

    pub fn init(file: []const u8, options: Options, allocator: Allocator) Self {
        return Self {
            .file = file,
            .chunk = Chunk.init(allocator),
            .ip = 0,
            .disassembler = Disassembler.init(),
            .stack = ArrayList(Object).init(allocator),
            .options = options,
            .allocator = allocator,
        };
    }

    pub fn interpret(self: *Self, source: []const u8) InterpreterError!void {
        defer self.deinit();

        var compiler = Compiler.init(self.file, &self.chunk, self.options, self.allocator);
        compiler.compile(source) catch return InterpreterError.Compile;

        try self.run();
    }

    fn run(self: *Self) InterpreterError!void {
        while (true) {
            if (self.options.args & @intFromEnum(Option.DebugTrace) != 0) try self.log_trace();
            const instruction: Opcodes = @enumFromInt(self.chunk.code.items[self.ip]);
            switch (instruction) {
                Opcodes.Return => {
                    const value = self.pop();
                    std.debug.print("{}\n", .{value});
                    return;
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

                else => return InterpreterError.Interpreter
            }
            self.ip += 1;
        }
    }

    fn equals(self: *Self, not: bool) InterpreterError!void {
        const rhs = self.pop();
        const lhs = self.pop();

        const value = lhs.equals(rhs) catch {
            // TODO: log error
            return InterpreterError.Interpreter;
        };

        if (not) {
            try self.push(value.not());
        } else try self.push(value);
    }

    fn compare(self: *Self, greater: bool) InterpreterError!void {
        const rhs = self.pop();
        const lhs = self.pop();

        // TODO: log errors
        const value = if (greater) val: {
            break :val lhs.gt(rhs) catch return InterpreterError.Interpreter;
        } else vale: {
            break :vale lhs.lt(rhs) catch return InterpreterError.Interpreter;
        };

        try self.push(value);
    }

    fn compare_equal(self: *Self, greater: bool) InterpreterError!void {
        const rhs = self.pop();
        const lhs = self.pop();
        // TODO: log errors
        const value = if (greater) val: {
            break :val lhs.ge(rhs) catch return InterpreterError.Interpreter;
        } else vale: {
            break :vale lhs.le(rhs) catch return InterpreterError.Interpreter;
        };

        try self.push(value);
    }

    fn add(self: *Self) InterpreterError!void {
        const rhs = self.pop();
        const lhs = self.pop();
        const value = lhs.add(rhs) catch {
            // TODO: log error
            return InterpreterError.Interpreter;
        };

        try self.push(value);
    }

    fn sub(self: *Self) InterpreterError!void {
        const rhs = self.pop();
        const lhs = self.pop();
        const value = lhs.sub(rhs) catch {
            // TODO: log error
            return InterpreterError.Interpreter;
        };

        try self.push(value);
    }

    fn mul(self: *Self) InterpreterError!void {
        const rhs = self.pop();
        const lhs = self.pop();
        const value = lhs.mul(rhs) catch {
            // TODO: log error
            return InterpreterError.Interpreter;
        };

        try self.push(value);
    }

    fn div(self: *Self) InterpreterError!void {
        const rhs = self.pop();
        const lhs = self.pop();
        const value = lhs.div(rhs) catch {
            // TODO: log error
            return InterpreterError.Interpreter;
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

    fn reset_stack(self: *Self) void {
        self.stack.clearRetainingCapacity();
    }

    fn deinit(self: *Self) void {
        self.chunk.deinit();
        self.stack.deinit();
    }
};
