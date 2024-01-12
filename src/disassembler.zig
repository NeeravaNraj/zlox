const std = @import("std");
const Chunk = @import("chunk.zig").Chunk;
const Opcodes = @import("opcodes.zig").Opcodes;
const Allocator = std.mem.Allocator;

pub const Disassembler = struct {
    const Self = @This();

    pub fn init() Self {
        return Self {};
    }

    pub fn disassemble_chunk(self: *Self, chunk: *Chunk) !void {
        try self.print("==== Debug ====\n", .{});

        var offset: usize = 0;
        while (offset < chunk.code_len()) {
            offset = try self.disassemble_instruction(chunk, offset);
        }
    }

    pub fn disassemble_instruction(self: *Self, chunk: *Chunk, offset: usize) !usize {
        try self.print("{:0>4}", .{offset});

        if (offset > 0 and chunk.check_previous(offset)) {
            try self.print("   |  ", .{});
        } else {
            if (chunk.get_line(offset)) |line|
                try self.print("{:>4}  ", .{line});
        }

        const instruction: Opcodes = @enumFromInt(chunk.code.items[offset]);
        switch (instruction) {
            Opcodes.Constant => return self.constant_instruction(instruction, chunk, offset),
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
            Opcodes.Print,
            Opcodes.Pop,
            Opcodes.None,
            Opcodes.Return => return try self.simple_instruction(instruction, offset),
            else => {
                try self.print("unknown opcode {}\n", .{instruction});
                return offset + 1;
            },
        }

    }

    fn simple_instruction(self: *Self, instruction: Opcodes, offset: usize) !usize {
        try self.print("{}\n", .{instruction});
        return offset + 1;
    }

    fn constant_instruction(self: *Self, instruction: Opcodes, chunk: *Chunk, offset: usize) !usize {
        var change: usize = 0; // this is 9 as size(u8) + usize
        const index = chunk.read_constant(offset, &change);

        try self.print("{s: <16} {:04} '", .{instruction.to_string(),  offset});
        try self.print("{}\n", .{chunk.constants.items[index]});

        return offset + change;
    }

    fn print(self: *Self, comptime fmt: []const u8, args: anytype) !void {
        _ = self;
        try std.io.getStdOut().writer().print(fmt, args);
    }
};
