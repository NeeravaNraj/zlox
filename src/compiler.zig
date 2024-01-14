const std = @import("std");
const Object = @import("object.zig").Object;
const Lexer = @import("lexer.zig").Lexer;
const Chunk = @import("chunk.zig").Chunk;
const tokens = @import("tokens.zig");
const Logger = @import("logger.zig").Logger;
const arg_parser = @import("arg_parser.zig");
const Opcodes = @import("opcodes.zig").Opcodes;
const rulefn = @import("rule.zig");
const Disasm = @import("disassembler.zig").Disassembler;
const Precedence = rulefn.Precedence;
const RuleFn = rulefn.RuleFn;
const ArrayList = std.ArrayList;
const Span = tokens.Span;
const Token = tokens.Token;
const TokenKind = tokens.TokenKind;
const Allocator = std.mem.Allocator;
const Options = arg_parser.Options;
const Option = arg_parser.Option;

pub const Compiler = struct {
    const Self = @This();
    const CompilerError = error {
        Error,
        Lexer,
        Disassembler,
    };

    file: []const u8,
    tokens: ArrayList(Token),
    allocator: Allocator,
    curr: usize,
    chunk: *Chunk,
    error_occurred: bool,
    panic_mode: bool,
    options: Options,

    pub fn init(file: []const u8, chunk: *Chunk, options: Options, allocator: Allocator) Self {
        return Self {
            .file = file,
            .tokens = undefined,
            .chunk = chunk,
            .curr = 0,
            .error_occurred = false,
            .panic_mode = false,
            .options = options,
            .allocator = allocator,
        };
    }

    pub fn compile(self: *Self, source: []const u8) CompilerError!void {
        var lexer = Lexer.init(self.file, source, self.options, self.allocator);
        self.tokens = lexer.tokenize() catch return CompilerError.Lexer;
        defer self.tokens.deinit();
        self.expression();
        self.consume(TokenKind.Eof, "expected end of expression");
        self.emit_byte(Opcodes.Return);

        if (self.options.args & @intFromEnum(Option.DebugOpcodes) != 0) {
            var disasm = Disasm.init();
            disasm.disassemble_chunk(self.chunk) catch return CompilerError.Disassembler;
        }

        if (self.error_occurred) return CompilerError.Error;

    }

    fn rule_fn(self: *Self, f: RuleFn, can_assign: bool) void {
        _ = can_assign;
        switch (f) {
            // prefix
            RuleFn.Number,
            RuleFn.String => self.constant_value(),
            RuleFn.Grouping => self.grouping(),
            RuleFn.Unary => self.unary(),
            RuleFn.Literal => self.literal(),
            // RuleFn.Variable => self.variable(can_assign),
            //
            // infix
            RuleFn.Binary => self.binary(),
            // RuleFn.Ternary => self.ternary(),
            // RuleFn.And => self.and(),
            // RuleFn.Or => self.or(),
            else => unreachable,
        }
    }

    fn parse_precedence(self: *Self, precedence: Precedence) void {
        self.advance();

        var kind = self.previous().kind;
        const prefix_ruleu = self.get_rule(kind);
        const prefix_rule = RuleFn.get_prefix(prefix_ruleu);
        const can_assign = @intFromEnum(precedence) <= @intFromEnum(Precedence.Assignment);

        switch (prefix_rule) {
            RuleFn.None => {
                const token = self.previous();
                self.log_error(&token.span, "expected expression", .{});
                return;
            },
            else => self.rule_fn(prefix_rule, can_assign),
        }

        while (!self.is_at_end()) {
            kind = self.current().kind;
            const rule = self.get_rule(kind);
            const next_prece = RuleFn.get_precedence(rule);
            if (@intFromEnum(precedence) <= @intFromEnum(next_prece)) {
                self.advance();
                kind = self.previous().kind;
                const infix_rule = RuleFn.get_infix(self.get_rule(kind));
                self.rule_fn(infix_rule, can_assign);
            } else break;
        }

    }

    fn grouping(self: *Self) void {
        self.expression();
        self.consume(TokenKind.RightParen, "expected ')' after expression");
    }

    fn constant_value(self: *Self) void {
        const value = self.previous().literal.?;
        self.emit_constant(value);
    }

    fn literal(self: *Self) void {
        switch (self.previous().kind) {
            TokenKind.False => self.emit_byte(Opcodes.False),
            TokenKind.True => self.emit_byte(Opcodes.True),
            TokenKind.None => self.emit_byte(Opcodes.None),
            else => unreachable,
        }
    }

    fn unary(self: *Self) void {
        const op = self.previous().kind;
        self.parse_precedence(Precedence.Unary);

        switch (op) {
            TokenKind.Minus => self.emit_byte(Opcodes.Negate),
            TokenKind.Bang => self.emit_byte(Opcodes.Not),
            else => unreachable,
        }
    }

    fn binary(self: *Self) void {
        const op = self.previous().kind;
        const rule = self.get_rule(op);
        self.parse_precedence(RuleFn.get_precedence(rule));
        
        switch (op) {
            TokenKind.Plus => self.emit_byte(Opcodes.Add),
            TokenKind.Minus => self.emit_byte(Opcodes.Subtract),
            TokenKind.Star => self.emit_byte(Opcodes.Multiply),
            TokenKind.Slash => self.emit_byte(Opcodes.Divide),
            TokenKind.Equals => self.emit_byte(Opcodes.Equals),
            TokenKind.BangEquals => self.emit_byte(Opcodes.NotEquals),
            TokenKind.Greater => self.emit_byte(Opcodes.Greater),
            TokenKind.Lesser => self.emit_byte(Opcodes.Less),
            TokenKind.GreaterEquals => self.emit_byte(Opcodes.GreaterEquals),
            TokenKind.LesserEquals => self.emit_byte(Opcodes.LessEquals),
            else => unreachable,
        }
    }

    fn expression(self: *Self) void {
        self.parse_precedence(Precedence.Assignment);
    }

    fn get_rule(self: *Self, kind: TokenKind) u16 {
        _ = self;
        return RuleFn.parse(kind);
    }

    fn emit_byte(self: *Self, op_code: Opcodes) void {
        // TODO: handle error
        self.chunk.write(op_code, self.previous().span.line) catch return;
    }

    fn emit_constant(self: *Self, constant: Object) void {
        // TODO: handle error
        self.chunk.write_constant(constant, self.previous().span.line) catch return;
    }

    fn advance(self: *Self) void {
        self.curr += 1;
    }

    fn consume(self: *Self, kind: TokenKind, message: []const u8) void {
        const token = self.current();
        if (token.kind == kind) {
            self.advance();
            return;
        }

        self.log_error(&token.span, "{s}", .{message});
    }

    fn current(self: *Self) *Token {
        return &self.tokens.items[self.curr];
    }

    fn previous(self: *Self) *Token {
        return &self.tokens.items[self.curr - 1];
    }

    fn set_error(self: *Self) void {
        self.error_occurred = true;
        self.panic_mode = true;
    }

    fn is_at_end(self: *Self) bool {
        return self.curr >= self.tokens.items.len;
    }

    fn log_error(self: *Self, span: *Span, comptime fmt: []const u8, args: anytype) void {
        self.set_error();
        const message = std.fmt.allocPrint(self.allocator, fmt, args) catch return;
        Logger.interpreter_error(span, message) catch return;
        self.allocator.free(message);
    }
};

