const std = @import("std");
const Object = @import("object.zig").Object;

pub const TokenKind = enum {
    // Single character
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    LeftBracket,
    RightBracket,
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Dollar,
    Caret,
    Ampersand,
    Pound,
    At,
    Bang,
    BackTick,
    Underscore,
    Colon,
    SemiColon,
    Quote,
    DoubleQuote,
    QuestionMark,
    BackSlash,
    Pipe,
    Assign,
    Dot,
    Comma,

    // Two or more characters
    Equals,
    BangEquals,
    Lesser,
    Greater,
    LesserEquals,
    GreaterEquals,
    And,
    Or,

    // Literals
    Int,
    Float,
    String,
    Identifier,

    // Keywords
    If,
    Else,
    ElseIf,
    Fn,
    Class,
    Self,
    For,
    While,
    Let,
    None,
    True,
    False,
    In,
    Return,
    Lm,
    Break,
    Continue,
    Eof,
};

pub const Span = struct {
    const Self = @This();
    line: usize,
    start: usize,
    end: usize,
    file_path: []const u8,

    pub fn init(file: []const u8, line: usize, start: usize, end: usize) Self {
        return Self{
            .line = line,
            .start = start,
            .end = end,
            .file_path = file,
        };
    }

    pub fn copy(self: *Self) Self {
        return Self{
            .line = self.line,
            .start = self.start,
            .end = self.end,
            .file_path = self.file_path
        };
    }

    pub fn format(self: Self, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;
        try writer.print("{}:{}-{}", .{ self.line, self.start, self.end });
    }
};

pub const Token = struct {
    const Self = @This();
    kind: TokenKind,
    lexeme: []const u8,
    span: Span,
    literal: ?Object,

    pub fn init(kind: TokenKind, lexeme: []const u8, span: Span, object: ?Object) Self {
        return Self{
            .kind = kind,
            .lexeme = lexeme,
            .span = span,
            .literal = object
        };
    }

    pub fn eof(span: Span) Self {
        return Self {
            .kind = TokenKind.Eof,
            .lexeme = "",
            .span = span,
            .literal = null
        };
    }

    pub fn format(self: Self, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;
        try writer.print(
            "Token {{\n  kind: {any},\n  lexeme: '{s}',\n  span: {},\n  literal: {any}\n}}\n",
            .{ self.kind, self.lexeme, self.span, self.literal}
        );
    }
};
