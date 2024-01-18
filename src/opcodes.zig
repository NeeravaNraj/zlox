const std = @import("std");

pub const Opcodes = enum(u8) {
    const Self = @This();
    Return,
    Constant,
    DefGlobal,
    GetGlobal,
    SetGlobal,
    SetLocal,
    GetLocal,
    JumpFalse,
    Call,
    Jump,
    Loop,
    Negate,
    Add,
    Subtract,
    Multiply,
    Divide,
    None,
    True,
    False,
    Not,
    Equals,
    NotEquals,
    Greater,
    GreaterEquals,
    Less,
    LessEquals,
    Ternary,
    Print,
    Pop,

    pub fn to_string(self: Self) []const u8 {
        const name = switch (self) {
            Self.Return => "OP_RETURN",
            Self.Constant => "OP_CONSTANT",
            Self.DefGlobal => "OP_DEF_GLOBAL",
            Self.GetGlobal => "OP_GET_GLOBAL",
            Self.SetGlobal => "OP_SET_GLOBAL",
            Self.Call => "OP_CALL",
            Self.SetLocal => "OP_SET_LOCAL",
            Self.GetLocal => "OP_GET_LOCAL",
            Self.JumpFalse => "OP_JUMP_FALSE",
            Self.Jump => "OP_JUMP",
            Self.Loop => "OP_LOOP",
            Self.Negate => "OP_NEGATE",
            Self.Add => "OP_ADD",
            Self.Subtract => "OP_SUBTRACT",
            Self.Multiply => "OP_MULTIPLY",
            Self.Divide => "OP_DIVIDE",
            Self.None => "OP_NONE",
            Self.True => "OP_TRUE",
            Self.False => "OP_FALSE",
            Self.Not => "OP_NOT",
            Self.Equals => "OP_EQAULS",
            Self.NotEquals => "OP_NOT_EQUAL",
            Self.Greater => "OP_GREATER",
            Self.GreaterEquals => "OP_GREATER_EQUAL",
            Self.Less => "OP_LESS",
            Self.LessEquals => "OP_LESS_EQUAL",
            Self.Ternary => "OP_TERNARY",
            Self.Print => "OP_PRINT",
            Self.Pop => "OP_POP",
        };

        return name;
    }
    
    pub fn format(self: Self, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;
        try writer.print("{s}", .{self.to_string()});
    }
};
