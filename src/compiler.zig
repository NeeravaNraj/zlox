const std = @import("std");
const Object = @import("object.zig").Object;
const Lexer = @import("lexer.zig").Lexer;
const Chunk = @import("chunk.zig").Chunk;
const tokens = @import("tokens.zig");
const logger = @import("logger.zig");
const arg_parser = @import("arg_parser.zig");
const Opcodes = @import("opcodes.zig").Opcodes;
const rulefn = @import("rule.zig");
const String = @import("string.zig").String;
const Disasm = @import("disassembler.zig").Disassembler;
const Function = @import("function.zig").Function;
const Logger = logger.Logger;
const Level = logger.Level;
const Precedence = rulefn.Precedence;
const RuleFn = rulefn.RuleFn;
const ArrayList = std.ArrayList;
const Span = tokens.Span;
const Token = tokens.Token;
const TokenKind = tokens.TokenKind;
const Allocator = std.mem.Allocator;
const Options = arg_parser.Options;
const Option = arg_parser.Option;

pub const FunctionType = enum {
    Script,
    Fn
};

pub const Local = struct {
    name: []const u8,
    depth: usize,
    initialized: bool
};

const Compiler = struct {
    const Self = @This();
    function: Function,
    fn_type: FunctionType,
    enclosing: ?*Self,
    locals: ArrayList(Local),

    pub fn init(fn_type: FunctionType, enclosing: ?*Self, allocator: Allocator) !Self {
        const locals = ArrayList(Local).init(allocator);
        return Self {
            .fn_type = fn_type,
            .function = try Function.default(allocator),
            .enclosing = enclosing,
            .locals = locals,
        };
    }

    pub fn deinit(self: *Self) void {
        self.locals.deinit();
    }
};

pub const Parser = struct {
    const Self = @This();
    const ParserError = error {
        Error,
        Lexer,
        Disassembler,
        Allocation,
    };

    tokens: ArrayList(Token),
    compiler: *Compiler,

    curr: usize,
    depth: usize,

    error_occurred: bool,
    panic_mode: bool,

    options: Options,
    allocator: Allocator,

    pub fn init(options: Options, allocator: Allocator) Self {
        return Self {
            .tokens = undefined,
            .curr = 0,
            .depth = 0,
            .error_occurred = false,
            .compiler = undefined,
            .panic_mode = false,
            .options = options,
            .allocator = allocator,
        };
    }

    pub fn compile(self: *Self, source: []const u8) ParserError!Function {
        var compiler = Compiler.init(FunctionType.Script, null, self.allocator) catch return ParserError.Allocation;
        compiler.function.name = "script";
        self.compiler = @constCast(&compiler);
        var lexer = Lexer.init(source, self.options, self.allocator);
        self.tokens = lexer.tokenize() catch return ParserError.Lexer;
        defer self.tokens.deinit();
        while (!self.is_match(TokenKind.Eof)) {
            self.statement();
        }
        self.emit_return();

        try self.log_code();

        if (self.error_occurred) return ParserError.Error;
        return self.compiler.function;
    }

    fn end_compiler(self: *Self) Function {
        self.emit_return();

        const function = self.compiler.function;
        if (self.compiler.enclosing) |compiler|
            self.compiler = compiler;

        return function;
    }

    fn statement(self: *Self) void {
        switch (self.current().kind) {
            TokenKind.Print => self.print_statement(),
            TokenKind.Let => self.let_statement(),
            TokenKind.LeftBrace => {
                self.begin_scope();
                self.block();
                self.end_scope();
            },
            TokenKind.Return => self.return_statement(),
            TokenKind.Fn => self.function_statement(),
            else => self.expression_statement(),
        }

        if (self.panic_mode) self.synchronize();
    }

    fn function_statement(self: *Self) void {
        self.advance();
        const global = self.parse_variable("expected function name");
        self.mark_initialized();
        self.function_decl(FunctionType.Fn);
        self.define_variable(global);
    }

    fn function_decl(self: *Self, fn_type: FunctionType) void {
        var compiler = Compiler.init(fn_type, self.compiler, self.allocator) catch {
            self.log_error(
                @constCast(&self.current().span), 
                "allocation failed at `function_decl`", .{}
            );
            return;
        };
        defer compiler.deinit();
        self.compiler = &compiler;
        self.compiler.function.name = self.previous().lexeme;
        self.begin_scope();

        self.consume(TokenKind.LeftParen, "expected '(' after function name");
        if (!self.check(TokenKind.RightParen)) {
            while (true) {
                if (self.compiler.function.arity >= 255) {
                    self.log_error(
                        @constCast(&self.previous().span), 
                        "function cannot have more than 255 parameters", 
                        .{}
                    );
                    // synchronize
                    while (!self.check(TokenKind.RightParen)) self.advance();
                    break;
                }
                self.compiler.function.arity += 1;
                const constant = self.parse_variable("expected parameter name");
                self.define_variable(constant);
                if (self.check(TokenKind.Comma)) {
                    self.advance();
                    if (self.check(TokenKind.RightParen)) break;
                    continue;
                }

                break;
            }
        }
        self.consume(TokenKind.RightParen, "expected ')' after parameters");
        if (!self.check(TokenKind.LeftBrace)) {
            self.log_error(
                @constCast(&self.current().span), 
                "expected '{{' before function body", .{}
            );
            return;
        }
        self.block();
        self.log_code() catch return;
        const function = self.end_compiler();

        self.end_scope();
        self.emit_constant(Object{ .Fn = function });
    }

    fn return_statement(self: *Self) void {
        if (self.compiler.fn_type == FunctionType.Script) {
            self.log_error(
                @constCast(&self.current().span),
                "return at top level is not allowed", .{}
            );
        }
        self.advance();
        if (self.is_match(TokenKind.SemiColon)) {
            self.emit_return();
        } else {
            self.expression();
            self.consume(TokenKind.SemiColon, "expected ';' afer return value");
            self.emit_byte(Opcodes.Return);
        }
    }

    fn block(self: *Self,) void {
        self.advance();
        while (!self.check(TokenKind.RightBrace) and !self.check(TokenKind.Eof))
            self.statement();
        self.consume(TokenKind.RightBrace, "expected '}}' after block");
    }

    fn let_statement(self: *Self) void {
        self.advance();
        const global = self.parse_variable("expected variable name");

        if (self.is_match(TokenKind.Assign)) {
            self.expression();
        } else self.emit_byte(Opcodes.None);

        self.consume(TokenKind.SemiColon, "expected ';' after expression");
        self.define_variable(global);
    }

    fn define_variable(self: *Self, index: usize) void {
        if (self.depth > 0) {
            self.mark_initialized();
            return;
        }

        self.emit_byte(Opcodes.DefGlobal);
        self.current_chunk().write_index(index) catch {
            self.log_error(&self.previous().span, "allocation failed at `define_variable`", .{});
            return;
        };
    }

    fn parse_variable(self: *Self, err_msg: []const u8) usize {
        self.consume(TokenKind.Identifier, err_msg);

        self.declare_variable();
        if (self.depth > 0) return 0;

        return self.identifier_constant(self.previous());
    }

    fn identifier_constant(self: *Self, token: *Token) usize {
        // Failure is very rare
        const var_name = String.init(token.lexeme, self.allocator) catch {
            Logger.log(Level.Fatal, "allocation failed at `identifier_constant`") 
            catch return 0;
            return 0;
        };
        return self.current_chunk().add_constant_manual(
            Object { .Str = var_name }
        ) catch {
            self.log_error(&token.span, "allocation failed at `identifier_constant`", .{});
            return 0;
        };
    }

    fn declare_variable(self: *Self) void {
        if (self.depth == 0) return;
        const name = self.previous();
        if (self.locals().items.len > 0) {
            var i = self.locals().items.len - 1;
            while (i >= 0) {
                const local = self.locals().items[i];
                if (local.initialized and local.depth < self.depth) break;
                if (std.mem.eql(u8, name.lexeme, local.name))
                    self.log_error(
                        &name.span, 
                        "variable with name '{s}' already exists in current scope", 
                        .{name.lexeme}
                    );
                if (i == 0) break;
                i -= 1;
            }
        }

        self.add_local(name);
    }

    fn add_local(self: *Self, name: *Token) void {
        const local = Local {
            .name = name.lexeme,
            .depth = self.depth,
            .initialized = false,
        };
        self.locals().append(local) catch {
            self.log_error(&name.span, "allocation failed at `add_local`", .{});
            return;
        };
    }

    fn mark_initialized(self: *Self) void {
        if (self.depth == 0) return;
        self.locals().items[self.locals().items.len - 1].initialized = true;
    }

    fn print_statement(self: *Self) void {
        self.advance();
        self.expression();
        self.consume(TokenKind.SemiColon, "expected ';' after expresion");
        self.emit_byte(Opcodes.Print);
    }

    fn expression_statement(self: *Self) void {
        self.expression();
        self.consume(TokenKind.SemiColon, "expected ';' after expresion");
    }

    fn synchronize(self: *Self) void {
        self.panic_mode = false;

        while (self.current().kind != TokenKind.Eof) {
            if (self.previous().kind == TokenKind.SemiColon) return;
            switch (self.current().kind) {
                TokenKind.Class,
                TokenKind.Fn,
                TokenKind.Let,
                TokenKind.For,
                TokenKind.While,
                TokenKind.If,
                TokenKind.Print,
                TokenKind.Return => return,
                else => {},
            }
            self.advance();
        }
    }

    fn rule_fn(self: *Self, f: RuleFn, can_assign: bool) void {
        switch (f) {
            // prefix
            RuleFn.Number,
            RuleFn.String => self.constant_value(),
            RuleFn.Grouping => self.grouping(),
            RuleFn.Unary => self.unary(),
            RuleFn.Literal => self.literal(),
            RuleFn.Variable => self.variable(can_assign),
            RuleFn.Call => self.call(),

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

        if (can_assign and self.is_match(TokenKind.Assign)) 
            self.log_error(&self.current().span, "invalid assignment target", .{});
    }

    fn argument_count(self: *Self) u8 {
        var arg_count: u8 = 0;
        
        if (!self.check(TokenKind.RightParen)) {
            while (true) {
                if (arg_count >= 255) {
                    self.log_error(
                        @constCast(&self.previous().span), 
                        "cannot pass more than 255 parameters to function", 
                        .{}
                    );
                    // synchronize
                    while (!self.is_match(TokenKind.RightParen)) self.advance();
                    break;
                }
                self.expression();
                arg_count += 1;
                if (self.check(TokenKind.Comma)) {
                    self.advance();
                    if (self.check(TokenKind.RightParen)) break;
                    continue;
                }
                break;
            }
        }
        self.consume(TokenKind.RightParen, "expected ')' after function arguments");

        return arg_count;
    }

    fn call(self: *Self) void {
        const arg_count: u8 = self.argument_count();
        self.emit_byte(Opcodes.Call);
        self.emit_value(arg_count);
    }

    fn variable(self: *Self, can_assign: bool) void {
        self.named_variable(self.previous(), can_assign);
    }

    fn named_variable(self: *Self, token: *Token, can_assign: bool) void {
        var get_op = Opcodes.GetGlobal;
        var set_op = Opcodes.SetGlobal;
        var arg = self.identifier_constant(token);

        if (self.resolve_local(token)) |local| {
            get_op = Opcodes.GetLocal;
            set_op = Opcodes.SetLocal;
            arg = local;
        }

        if (can_assign and self.is_match(TokenKind.Assign)) {
            self.expression();
            self.emit_byte(set_op);
            self.current_chunk().write_index(arg) catch {
                self.log_error(&token.span, "allocation failed at `named_variable`", .{});
                return;
            };
        } else {
            self.emit_byte(get_op);
            self.current_chunk().write_index(arg) catch {
                self.log_error(&token.span, "allocation failed at `named_variable`", .{});
                return;
            };
        }
    }

    fn resolve_local(self: *Self, name: *Token) ?usize {
        if (self.locals().items.len == 0) return null;
        var i = self.locals().items.len - 1;
        while (i >= 0) {
            const local = self.locals().items[i];
            if (std.mem.eql(u8, name.lexeme, local.name)) {
                if (self.depth == 1 and !local.initialized) return null;
                return if (local.initialized) i else i - 1;
            }
            if (i == 0) break;
            i -= 1;
        }

        return null;
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
        var start = self.current().span;
        self.parse_precedence(Precedence.Assignment);
        const end = self.previous().span.end;
        start.end = end;
        self.map_source(start);
    }

    fn map_source(self: *Self, span: Span) void {
        self.current_function().source_map.append(span) catch {
            self.log_error(@constCast(&span), "allocation failed at `map_source`", .{});
            return;
        };
    }

    fn get_rule(self: *Self, kind: TokenKind) u16 {
        _ = self;
        return RuleFn.parse(kind);
    }

    fn emit_byte(self: *Self, op_code: Opcodes) void {
        // TODO: handle error
        self.current_chunk().write(op_code, self.previous().span.line) catch return;
    }

    fn emit_value(self: *Self, byte: u8) void {
        self.current_chunk().code.append(byte) catch return;
    }

    fn emit_constant(self: *Self, constant: Object) void {
        // TODO: handle error
        self.current_chunk().write_constant(constant, self.previous().span.line) catch return;
    }

    fn emit_return(self: *Self) void {
        // TODO: handle error
        self.current_chunk().write(Opcodes.None, self.previous().span.line) catch return;
        self.current_chunk().write(Opcodes.Return, self.previous().span.line) catch return;
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

    fn check(self: *Self, kind: TokenKind) bool {
        return self.current().kind == kind;
    }

    fn is_match(self: *Self, kind: TokenKind) bool {
        if (!self.check(kind)) return false;
        self.advance();
        return true;
    }

    fn current_chunk(self: *Self) *Chunk {
        return &self.compiler.function.chunk;
    }

    fn current_function(self: *Self) *Function {
        return &self.compiler.function;
    }

    fn begin_scope(self: *Self) void {
        self.depth += 1;
    }

    fn end_scope(self: *Self) void {
        self.depth -= 1;
        while (
            self.locals().items.len > 0 and 
            self.locals().items[self.locals().items.len - 1].depth > self.depth
        ) {
            self.emit_byte(Opcodes.Pop);
            _ = self.locals().pop();
        }
    }

    fn locals(self: *Self) *ArrayList(Local) {
        return &self.compiler.locals;
    }

    fn log_code(self: *Self) ParserError!void {
        if (self.options.args & @intFromEnum(Option.DebugOpcodes) != 0) {
            var disasm = Disasm.init();
            disasm.disassemble_chunk(
                self.current_function().name, 
                self.current_chunk()
            ) catch return ParserError.Disassembler;
        }
    }

    fn log_error(self: *Self, span: *Span, comptime fmt: []const u8, args: anytype) void {
        self.set_error();
        const message = std.fmt.allocPrint(self.allocator, fmt, args) catch return;
        Logger.interpreter_error(span, message) catch return;
        self.allocator.free(message);
    }

};
