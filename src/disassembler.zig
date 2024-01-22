const std = @import("std");
const Chunk = @import("chunk.zig").Chunk;
const Opcodes = @import("opcodes.zig").Opcodes;
const Allocator = std.mem.Allocator;

pub const Disassembler = struct {
    const Self = @This();

    pub fn init() Self {
        return Self {};
    }

    pub fn disassemble_chunk(self: *Self, name: []const u8, chunk: *Chunk) !void {
        try self.print(" {0s:=>23}{1s}{0s:=<22}\n", .{" ", name});
        try self.print("  CODE{0s: >7}LINE{0s: >11}OPCODES\n\n", .{""});

        var offset: usize = 0;
        while (offset < chunk.code_len()) {
            offset = try self.disassemble_instruction(chunk, offset);
        }
    }

    pub fn disassemble_instruction(self: *Self, chunk: *Chunk, offset: usize) !usize {
        try self.print("  {d:0>4}", .{offset});

        if (offset > 0 and chunk.check_previous(offset)) {
            try self.print("{s: >8}", .{"|"});
        } else {
            if (chunk.get_line(offset)) |line|
                try self.print("{: >8}", .{line});
        }

        const instruction: Opcodes = @enumFromInt(chunk.code.items[offset]);
        switch (instruction) {
            Opcodes.JumpFalse,
            Opcodes.Loop,
            Opcodes.Jump  => return self.jump_instruction(instruction, chunk, offset),
            Opcodes.Constant,
            Opcodes.GetGlobal,
            Opcodes.SetGlobal,
            Opcodes.GetLocal,
            Opcodes.SetLocal,
            Opcodes.DefGlobal => return self.constant_instruction(instruction, chunk, offset),
            Opcodes.Negate,
            Opcodes.Add,
            Opcodes.Subtract,
            Opcodes.Multiply,
            Opcodes.Divide,
            Opcodes.True,
            Opcodes.False,
            Opcodes.Not,
            Opcodes.Less,
            Opcodes.Greater,
            Opcodes.LessEquals,
            Opcodes.GreaterEquals,
            Opcodes.NotEquals,
            Opcodes.Equals,
            Opcodes.Ternary,
            Opcodes.Pop,
            Opcodes.None,
            Opcodes.Return => return try self.simple_instruction(instruction, offset),
            Opcodes.Call => return self.byte_instruction(instruction, chunk, offset),
        }

    }

    fn simple_instruction(self: *Self, instruction: Opcodes, offset: usize) !usize {
        try self.print("{s: >12}{s}\n", .{" ", instruction.to_string()});
        return offset + 1;
    }

    fn byte_instruction(self: *Self, instruction: Opcodes, chunk: *Chunk, offset: usize) !usize {
        const value = chunk.code.items[offset + 1];

        try self.print("{0s: >12}{1s: <24}'", .{" ", instruction.to_string()});
        try self.print("{d}\n", .{value});

        return offset + 2;
    }

    fn jump_instruction(self: *Self, instruction: Opcodes, chunk: *Chunk, offset: usize) !usize {
        var change: usize = 0;
        const value = chunk.read_index(offset, &change);

        try self.print("{0s: >12}{1s: <24}'", .{" ", instruction.to_string()});
        try self.print("{d}\n", .{value});

        return offset + change;
    }

    fn constant_instruction(self: *Self, instruction: Opcodes, chunk: *Chunk, offset: usize) !usize {
        var change: usize = 0;
        const value = chunk.read_constant(offset, &change);

        try self.print("{0s: >12}{1s: <24}{2d:0>4} '", .{" ", instruction.to_string(), offset});
        try self.print("{}\n", .{value});

        return offset + change;
    }

    fn print(self: *Self, comptime fmt: []const u8, args: anytype) !void {
        _ = self;
        try std.io.getStdOut().writer().print(fmt, args);
    }
};
