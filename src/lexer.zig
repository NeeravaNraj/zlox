const std = @import("std");
const tokens = @import("tokens.zig");
const Object = @import("object.zig").Object;
const Logger = @import("logger.zig").Logger;
const String = @import("string.zig").String;
const arg_parser = @import("arg_parser.zig");
const Options = arg_parser.Options;
const Option = arg_parser.Option;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const HashMap = std.StringHashMap;
const Token = tokens.Token;
const TokenKind = tokens.TokenKind;
const Span = tokens.Span;

pub const Lexer = struct {
    pub const LexerError = error{
        UnknownCharater,
        AbruptEof,
        UnterminatedString,
        ParseNumerical,
        AllocationError,
        UnterminatedComment,
    };

    const Self = @This();
    source: []const u8,
    start: usize,
    curr: usize,
    span: Span,
    options: Options,
    error_occurred: bool,
    allocator: Allocator,

    pub fn init(source: []const u8, options: Options, allocator: Allocator) Self {
        return Self{ 
            .source = source, 
            .start = 0, 
            .curr = 0, 
            .span = Span.init(options.file.?, 1, 0, 0), 
            .options = options, 
            .error_occurred = false, 
            .allocator = allocator 
        };
    }

    pub fn tokenize(self: *Self) !ArrayList(Token) {
        var list = ArrayList(Token).init(self.allocator);

        while (!self.is_at_end()) {
            const token = self.read_token() catch |err| {
                if (
                    err == LexerError.UnterminatedComment or 
                    err == LexerError.UnterminatedString
                ) return err;
                continue;
            };

            try list.append(token);
        }

        try list.append(Token.eof(self.span));

        if (self.options.args & @intFromEnum(Option.DebugTokens) != 0)
            try self.print_tokens(list.items);

        return list;
    }

    fn read_token(self: *Self) LexerError!Token {
        try self.whitespace();

        self.start = self.curr;
        self.span.start = self.span.end;

        if (self.is_at_end()) return LexerError.AbruptEof;
        const char = self.bump();
        const token: LexerError!Token = switch (char) {
            '(' => self.make_token(TokenKind.LeftParen),
            ')' => self.make_token(TokenKind.RightParen),
            '{' => self.make_token(TokenKind.LeftBrace),
            '}' => self.make_token(TokenKind.RightBrace),
            '[' => self.make_token(TokenKind.LeftBracket),
            ']' => self.make_token(TokenKind.RightBracket),
            '.' => self.make_token(TokenKind.Dot),
            ':' => self.make_token(TokenKind.Colon),
            ';' => self.make_token(TokenKind.SemiColon),
            '+' => plus: {
                const token = TokenKind.Plus;
                break :plus self.make_token(token);
            },
            '-' => self.make_token(TokenKind.Minus),
            '*' => self.make_token(TokenKind.Star),
            // TODO: Comments
            '/' => self.make_token(TokenKind.Slash),
            '`' => self.make_token(TokenKind.BackTick),
            '%' => self.make_token(TokenKind.Percent),
            '=' => equal: {
                var token = TokenKind.Assign;
                if (self.is_match('=')) token = TokenKind.Equals;

                break :equal self.make_token(token);
            },
            '>' => greater: {
                var token = TokenKind.Greater;
                if (self.is_match('=')) token = TokenKind.GreaterEquals;
                break :greater self.make_token(token);
            },
            '<' => lesser: {
                var token = TokenKind.Lesser;
                if (self.is_match('=')) token = TokenKind.LesserEquals;
                break :lesser self.make_token(token);
            },
            '!' => bang: {
                var token = TokenKind.Bang;
                if (self.is_match('=')) token = TokenKind.BangEquals;

                break :bang self.make_token(token);
            },
            '@' => self.make_token(TokenKind.At),
            '$' => self.make_token(TokenKind.Dollar),
            // TODO: Strings
            '\'', '"' => self.string(char),

            ',' => self.make_token(TokenKind.Comma),
            '?' => self.make_token(TokenKind.QuestionMark),
            '#' => self.make_token(TokenKind.Pound),
            '&' => l_and: {
                var token = TokenKind.Ampersand;
                if (self.is_match('&')) token = TokenKind.And;
                break :l_and self.make_token(token);
            },
            '|' => l_or: {
                var token = TokenKind.Pipe;
                if (self.is_match('|')) token = TokenKind.Or;
                break :l_or self.make_token(token);
            },
            '^' => self.make_token(TokenKind.Caret),
            '\\' => self.make_token(TokenKind.BackSlash),
            '0'...'9' => self.numerical(),
            'a'...'z', 'A'...'Z', '_' => self.identifier(),
            else => {
                self.log_error("unknown character '{c}'", .{char});
                return LexerError.UnknownCharater;
            },
        };

        return token;
    }

    fn whitespace(self: *Self) LexerError!void {
        while (true) {
            if (self.peek()) |char| {
                switch (char) {
                    '\n' => {
                        _ = self.bump();
                        self.span.line += 1;
                        self.span.start = 0;
                        self.span.end = 0;
                    },
                    ' ', '\r', '\t' => _ = self.bump(),
                    '/' => {
                        const next = self.peek_next();
                        if (next) |ch| {
                            switch (ch) {
                                '/' => {
                                    while (!self.is_at_end()) {
                                        if (self.check('\n')) {
                                            _ = self.bump();
                                            break;
                                        }
                                        _ = self.bump();
                                    }
                                },
                                '*' => try self.block_comment(),
                                else => break,
                            }
                        } else break;
                    },
                    else => break,
                }
            } else break;
        }
    }

    fn block_comment(self: *Self) LexerError!void {
        _ = self.bump();
        _ = self.bump();
        const line = self.span.line;
        while (!self.is_at_end()) {
            switch (self.peek().?) {
                '/' => {
                    if (self.peek_next()) |star| {
                        if (star == '*') try self.block_comment();
                    }
                },
                '*' => {
                    _ = self.bump();
                    if (self.is_match('/')) return;
                },
                '\n' => {
                    self.span.line += 1;
                    _ = self.bump();
                },
                else => _ = self.bump(),
            }
        }
        var span = self.span;
        span.line = line;
        span.end = span.start + 2;
        self.error_occurred = true;
        Logger.interpreter_error(&span, "comment block was not terminated") 
        catch return LexerError.UnterminatedComment;
        return LexerError.UnterminatedComment;
    }

    fn numerical(self: *Self) LexerError!Token {
        // TODO: Handle Hex and Bin (optionally octal)
        while (self.peek()) |char| {
            if (!std.ascii.isDigit(char) or self.is_at_end()) break;
            _ = self.bump();
        }

        const point = self.peek();
        if (point != null and point.? == '.') {
            _ = self.bump();
            while (self.peek()) |char| {
                // Handle scientific notation
                if (char == 'e' or char == 'E') {
                    _ = self.bump();
                    if (self.peek()) |sign| {
                        if (sign == '+' or sign == '-') _ = self.bump();
                    }
                    continue;
                }
                if (!std.ascii.isDigit(char) or self.is_at_end()) break;
                _ = self.bump();
            }

            if (std.fmt.parseFloat(f64, self.source[self.start..self.curr])) |value| {
                return self.make_literal(TokenKind.Float, Object{ .Float = value });
            } else |_| {
                self.log_error("invalid floating point integer", .{});
                return LexerError.ParseNumerical;
            }
        }

        if (self.peek()) |char| {
            if (std.ascii.isAlphabetic(char)) {
                _ = self.bump();
                while (self.peek()) |c| {
                    if (!std.ascii.isAlphabetic(c)) break;
                    _ = self.bump();
                }

                self.log_error("invalid integer", .{});
                return LexerError.ParseNumerical;
            }
        }

        // Parse base 10
        if (std.fmt.parseInt(i32, self.source[self.start..self.curr], 10)) |value| {
            return self.make_literal(TokenKind.Int, Object{ .Int = value });
        } else |_| {
            self.log_error("invalid integer", .{});
            return LexerError.ParseNumerical;
        }
    }

    fn string(self: *Self, closing: u8) LexerError!Token {
        var terminated = false;
        var literal = String.init("", self.allocator) catch return LexerError.AllocationError;
        while (!self.is_at_end()) {
            if (self.peek() != null and self.peek().? == '\n') break;
            if (self.peek() != null and self.peek().? == closing) {
                terminated = true;
                break;
            }
            if (self.peek() != null and self.peek() == '\\') {
                _ = self.bump();
                if (self.peek()) |escaped| {
                    const c: u8 = switch (escaped) {
                        'n' => '\n',
                        '\'' => '\'',
                        '"' => '\"',
                        'r' => '\r',
                        't' => '\t',
                        '\\' => '\\',
                        else => continue,
                    };
                    literal.push_char(c) catch return LexerError.AllocationError;
                    _ = self.bump();
                    continue;
                }
            }
            literal.push_char(self.current()) catch return LexerError.AllocationError;
            _ = self.bump();
        }

        if (!terminated) {
            self.log_error("unterminated string, expected {c} at end", .{closing});
            literal.deinit();
            return LexerError.UnterminatedString;
        }

        _ = self.bump();
        return self.make_literal(TokenKind.String, Object{ .Str = literal });
    }

    fn identifier(self: *Self) LexerError!Token {
        while (self.peek()) |char| {
            if (std.ascii.isAlphanumeric(char) or char == '_') {
                _ = self.bump();
            } else break;
        }

        const value = self.source[self.start..self.curr];

        const keyword = self.is_keyword(value) catch return LexerError.AllocationError;
        if (keyword) |token| return self.make_token(token);

        return self.make_token(TokenKind.Identifier);
    }

    fn is_keyword(self: *Self, value: []const u8) !?TokenKind {
        var keywords = HashMap(TokenKind).init(self.allocator);
        try keywords.put("if", TokenKind.If);
        try keywords.put("else", TokenKind.Else);
        try keywords.put("fn", TokenKind.Fn);
        try keywords.put("lm", TokenKind.Lm);
        try keywords.put("for", TokenKind.For);
        try keywords.put("while", TokenKind.While);
        try keywords.put("let", TokenKind.Let);
        try keywords.put("class", TokenKind.Class);
        try keywords.put("self", TokenKind.Self);
        try keywords.put("elif", TokenKind.ElseIf);
        try keywords.put("true", TokenKind.True);
        try keywords.put("false", TokenKind.False);
        try keywords.put("none", TokenKind.None);
        try keywords.put("in", TokenKind.In);
        try keywords.put("break", TokenKind.Break);
        try keywords.put("continue", TokenKind.Continue);
        try keywords.put("return", TokenKind.Return);
        return keywords.get(value);
    }

    inline fn is_at_end(self: *Self) bool {
        return self.curr >= self.source.len - 1;
    }

    fn is_match(self: *Self, expected: u8) bool {
        if (self.is_at_end()) return false;

        if (self.current() != expected) return false;

        self.curr += 1;
        self.span.end += 1;
        return true;
    }

    inline fn current(self: *Self) u8 {
        return self.source[self.curr];
    }

    inline fn bump(self: *Self) u8 {
        self.curr += 1;
        self.span.end += 1;
        return self.source[self.curr - 1];
    }

    inline fn peek(self: *Self) ?u8 {
        if (self.is_at_end()) return null;
        return self.source[self.curr];
    }

    inline fn peek_next(self: *Self) ?u8 {
        if (self.is_at_end()) return null;
        return self.source[self.curr + 1];
    }

    inline fn check(self: *Self, expected: u8) bool {
        if (self.is_at_end()) return false;
        return self.source[self.curr + 1] == expected;
    }

    inline fn make_literal(self: *Self, kind: TokenKind, literal: Object) Token {
        return Token.init(kind, self.source[self.start..self.curr], self.span.copy(), literal);
    }

    inline fn make_token(self: *Self, kind: TokenKind) Token {
        return Token.init(kind, self.source[self.start..self.curr], self.span.copy(), null);
    }

    fn log_error(self: *Self, comptime fmt: []const u8, args: anytype) void {
        const message = std.fmt.allocPrint(self.allocator, fmt, args) catch return;
        Logger.interpreter_error(&self.span, message) catch return;
        self.error_occurred = true;
        self.allocator.free(message);
    }

    fn print_tokens(self: *Self, t: []Token) !void {
        _ = self;
        for (t) |token| {
            std.debug.print("{}", .{token});
        }
    }
};
