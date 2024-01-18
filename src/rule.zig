const tokens = @import("tokens.zig");
const TokenKind = tokens.TokenKind;

pub const PRECEDENCE_MASK: u16 = 0x000F;
pub const PREFIX_MASK: u16 = 0x00F0;
pub const INFIX_MASK: u16 = 0x0F00;
pub const POSTFIX_MASK: u16 = 0xF000;

pub const Precedence = enum(u16) {
    None,
    Assignment,
    Ternary,
    Logical,
    Equality,
    Comparison,
    Term,
    Factor,
    Unary,
    Call,
    Primary,
};

pub const RuleFn = enum(u16) {
    const Self = @This();
    const RuleError = error{
        Unknown,
    };

    None = 0,

    // prefix
    Number = 0x10,
    Literal = 0x20,
    String = 0x30,
    Grouping = 0x40,
    Unary = 0x50,
    Variable = 0x60,

    // infix
    Binary = 0x100,
    Ternary = 0x200,
    And = 0x300,
    Or = 0x400,
    Call = 0x500,

    pub fn parse(kind: TokenKind) u16 {
        //  0 - none
        //  1 - number
        //  2 - grouping
        //  3 - unary
        //  4 - binary
        return switch (kind) {
            TokenKind.LeftParen => @intFromEnum(Precedence.Call) | @intFromEnum(RuleFn.Grouping) | @intFromEnum(RuleFn.Call),
            TokenKind.Minus => @intFromEnum(Precedence.Term) | @intFromEnum(RuleFn.Binary) | @intFromEnum(RuleFn.Unary),
            TokenKind.Plus => @intFromEnum(Precedence.Term) | @intFromEnum(RuleFn.Binary),
            TokenKind.Slash => @intFromEnum(Precedence.Factor) | @intFromEnum(RuleFn.Binary),
            TokenKind.Star => @intFromEnum(Precedence.Factor) | @intFromEnum(RuleFn.Binary),
            TokenKind.Int => @intFromEnum(Precedence.None) | @intFromEnum(RuleFn.Number),
            TokenKind.Float => @intFromEnum(Precedence.None) | @intFromEnum(RuleFn.Number),
            TokenKind.None => @intFromEnum(Precedence.None) | @intFromEnum(RuleFn.Literal),
            TokenKind.True => @intFromEnum(Precedence.None) | @intFromEnum(RuleFn.Literal),
            TokenKind.False => @intFromEnum(Precedence.None) | @intFromEnum(RuleFn.Literal),
            TokenKind.Bang => @intFromEnum(Precedence.None) | @intFromEnum(RuleFn.Unary),
            TokenKind.BangEquals => @intFromEnum(Precedence.Equality) | @intFromEnum(RuleFn.Binary),
            TokenKind.Equals => @intFromEnum(Precedence.Equality) | @intFromEnum(RuleFn.Binary),
            TokenKind.Greater => @intFromEnum(Precedence.Comparison) | @intFromEnum(RuleFn.Binary),
            TokenKind.GreaterEquals => @intFromEnum(Precedence.Comparison) | @intFromEnum(RuleFn.Binary),
            TokenKind.Lesser => @intFromEnum(Precedence.Comparison) | @intFromEnum(RuleFn.Binary),
            TokenKind.LesserEquals => @intFromEnum(Precedence.Comparison) | @intFromEnum(RuleFn.Binary),
            TokenKind.QuestionMark => @intFromEnum(Precedence.Ternary) | @intFromEnum(RuleFn.Ternary),
            TokenKind.String => @intFromEnum(Precedence.None) | @intFromEnum(RuleFn.String),
            TokenKind.Identifier => @intFromEnum(Precedence.None) | @intFromEnum(RuleFn.Variable),
            TokenKind.And => @intFromEnum(Precedence.Logical) | @intFromEnum(RuleFn.And),
            TokenKind.Or => @intFromEnum(Precedence.Logical) | @intFromEnum(RuleFn.Or),
            else => @intFromEnum(Precedence.None),
        };
    }
    pub fn get_precedence(rule: u16) Precedence {
        return @enumFromInt(rule & PRECEDENCE_MASK);
    }

    pub fn get_prefix(rule: u16) RuleFn {
        return @enumFromInt(rule & PREFIX_MASK);
    }

    pub fn get_infix(rule: u16) RuleFn {
        return @enumFromInt(rule & INFIX_MASK);
    }

    pub fn get_postfix(rule: u16) RuleFn {
        return @enumFromInt(rule & POSTFIX_MASK);
    }
};
