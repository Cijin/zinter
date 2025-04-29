const std = @import("std");
const lexer = @import("lexer.zig");
const token = @import("token.zig");
const ast = @import("ast.zig");
const print = std.debug.print;
const mem = std.mem;
const fmt = std.fmt;
const testing = std.testing;
const assert = std.debug.assert;

const MAX_STMTS_ARRAY_SIZE = 4096;
const infix_parse_fn = *const fn (p: *Parser, left: ast.Expression) ParserError!ast.Expression;
const prefix_parse_fn = *const fn (p: *Parser) ParserError!ast.Expression;

const precedence = enum(u4) {
    lowest = 1,
    equals,
    less_greater,
    sum,
    product,
    prefix,
    call,
};

const ParserError = error{
    UnexpectedToken,
    UnexpectedPrefix,
    UnexpectedInfixOperator,
    ParseIntError,
    OutOfMemory,
};

const Parser = struct {
    l: *lexer.lexer,
    cur_token: token.Token,
    peek_token: token.Token,
    allocator: mem.Allocator,
    prefix_parse_fns: std.AutoHashMap(token.TokenType, prefix_parse_fn),
    infix_parse_fns: std.AutoHashMap(token.TokenType, infix_parse_fn),

    fn next_token(self: *Parser) void {
        assert(self.cur_token.token_type != token.TokenType.Eof);

        self.cur_token = self.peek_token;
        self.peek_token = self.l.next_token();
    }

    fn peek_token_is(self: *Parser, expected: token.TokenType) bool {
        return self.peek_token.token_type == expected;
    }

    fn expect_peek(self: *Parser, expected: token.TokenType) ParserError!void {
        if (self.peek_token_is(expected)) {
            self.next_token();
            return;
        }

        print("expected next token to be {s} but found {s}\n", .{ @tagName(expected), @tagName(self.peek_token.token_type) });
        return ParserError.UnexpectedToken;
    }

    pub fn parse_program(self: *Parser) ParserError!ast.Program {
        var stmts: [MAX_STMTS_ARRAY_SIZE]ast.Statement = undefined;
        var idx: usize = 0;
        while (self.cur_token.token_type != token.TokenType.Eof) {
            assert(idx < self.l.input.len);
            assert(idx < MAX_STMTS_ARRAY_SIZE);

            const stmt = try self.parse_statement();
            if (stmt) |s| {
                stmts[idx] = s;
                idx += 1;
            }
            self.next_token();
        }

        return ast.Program{
            .statements = stmts[0..idx],
        };
    }

    fn parse_statement(self: *Parser) ParserError!?ast.Statement {
        switch (self.cur_token.token_type) {
            token.TokenType.Let => {
                const let_stmt = try self.parse_let_statement();
                return ast.Statement{ .let_statement = let_stmt };
            },
            token.TokenType.Return => {
                const return_stmt = try self.parse_return_statement();
                if (return_stmt) |stmt| {
                    return ast.Statement{ .return_statement = stmt };
                }

                return null;
            },
            else => return null,
        }
    }

    fn parse_return_statement(self: *Parser) ParserError!?ast.ReturnStatement {
        var return_stmt = ast.ReturnStatement{
            .token = self.cur_token,
            .return_value = undefined,
        };

        self.next_token();

        const p = self.cur_precedence();
        return_stmt.return_value = try self.parse_expression(p);

        return return_stmt;
    }

    fn parse_let_statement(self: *Parser) ParserError!ast.LetStatement {
        var let_stmt = ast.LetStatement{
            .token = self.cur_token,
            .name = undefined,
            .value = undefined,
        };

        try self.expect_peek(token.TokenType.Ident);
        const ident = ast.Identifier{
            .token = self.cur_token,
            .value = self.cur_token.literal,
        };
        let_stmt.name = ident;

        try self.expect_peek(token.TokenType.Assign);
        self.next_token();

        const p = self.cur_precedence();
        let_stmt.value = try self.parse_expression(p);

        return let_stmt;
    }

    fn register_prefix(self: *Parser, k: token.TokenType, v: prefix_parse_fn) !void {
        try self.prefix_parse_fns.put(k, v);
    }

    fn register_infix(self: *Parser, k: token.TokenType, v: infix_parse_fn) !void {
        try self.infix_parse_fns.put(k, v);
    }

    fn cur_precedence(self: *Parser) precedence {
        return precedence_look_up.get(self.cur_token.token_type) orelse precedence.lowest;
    }

    fn peek_precedence(self: *Parser) precedence {
        return precedence_look_up.get(self.peek_token.token_type) orelse precedence.lowest;
    }

    fn parse_identifier(self: *Parser) ParserError!ast.Expression {
        return ast.Expression{ .identifier = ast.Identifier{ .token = self.cur_token, .value = self.cur_token.literal } };
    }

    fn parse_integer(self: *Parser) ParserError!ast.Expression {
        const parsed_int = fmt.parseInt(i64, self.cur_token.literal, 10) catch {
            print("failed to parse integer literal", .{});
            return ParserError.ParseIntError;
        };

        return ast.Expression{ .integer = ast.Integer{ .token = self.cur_token, .value = parsed_int } };
    }

    fn parse_prefix_expression(self: *Parser) ParserError!ast.Expression {
        var prefix_expression = self.allocator.create(ast.PrefixExpression) catch unreachable;
        prefix_expression.* = ast.PrefixExpression{
            .token = self.cur_token,
            .operator = self.cur_token.literal,
            .right = undefined,
        };

        const p = self.cur_precedence();
        self.next_token();
        prefix_expression.right = try self.parse_expression(p);

        return ast.Expression{ .prefix_expression = prefix_expression };
    }

    fn parse_infix_expression(self: *Parser, left: ast.Expression) ParserError!ast.Expression {
        const infix_expression = self.allocator.create(ast.InfixExpression) catch {
            return ParserError.OutOfMemory;
        };
        infix_expression.* = ast.InfixExpression{
            .token = self.cur_token,
            .operator = self.cur_token.literal,
            .left = left,
            .right = undefined,
        };

        const p = self.cur_precedence();
        self.next_token();
        infix_expression.right = try self.parse_expression(p);

        return ast.Expression{ .infix_expression = infix_expression };
    }

    fn parse_expression(self: *Parser, p: precedence) ParserError!ast.Expression {
        const prefix_fn = self.prefix_parse_fns.get(self.cur_token.token_type) orelse {
            print("no prefix parse fn found for: {s}", .{self.cur_token.literal});
            return ParserError.UnexpectedPrefix;
        };
        var left_exp = try prefix_fn(self);

        // Todo: does the below statement get parsed as expected?
        // 1 + 2 * 3
        while (!self.peek_token_is(token.TokenType.Semicolon) and @intFromEnum(p) < @intFromEnum(self.peek_precedence())) {
            const infix = self.infix_parse_fns.get(self.peek_token.token_type) orelse {
                print("no infix parse fn found for: {s}", .{self.cur_token.literal});
                return ParserError.UnexpectedInfixOperator;
            };

            self.next_token();
            left_exp = try infix(self, left_exp);
        }

        return left_exp;
    }
};

var precedence_look_up: std.AutoHashMap(token.TokenType, precedence) = undefined;
pub fn New(allocator: mem.Allocator, l: *lexer.lexer) !*Parser {
    precedence_look_up = std.AutoHashMap(token.TokenType, precedence).init(allocator);
    try precedence_look_up.put(token.TokenType.Equal, precedence.equals);
    try precedence_look_up.put(token.TokenType.NotEqual, precedence.equals);
    try precedence_look_up.put(token.TokenType.Lt, precedence.less_greater);
    try precedence_look_up.put(token.TokenType.Gt, precedence.less_greater);
    try precedence_look_up.put(token.TokenType.Minus, precedence.sum);
    try precedence_look_up.put(token.TokenType.Plus, precedence.sum);
    try precedence_look_up.put(token.TokenType.Slash, precedence.product);
    try precedence_look_up.put(token.TokenType.Asterix, precedence.product);

    var p = try allocator.create(Parser);
    p.* = Parser{
        .l = l,
        .cur_token = undefined,
        .peek_token = undefined,
        .allocator = allocator,
        .prefix_parse_fns = std.AutoHashMap(token.TokenType, prefix_parse_fn).init(allocator),
        .infix_parse_fns = std.AutoHashMap(token.TokenType, infix_parse_fn).init(allocator),
    };

    try p.register_prefix(token.TokenType.Ident, Parser.parse_identifier);
    try p.register_prefix(token.TokenType.Int, Parser.parse_integer);
    try p.register_prefix(token.TokenType.Bang, Parser.parse_prefix_expression);
    try p.register_prefix(token.TokenType.Minus, Parser.parse_prefix_expression);

    try p.register_infix(token.TokenType.Plus, Parser.parse_infix_expression);
    try p.register_infix(token.TokenType.Minus, Parser.parse_infix_expression);
    try p.register_infix(token.TokenType.Lt, Parser.parse_infix_expression);
    try p.register_infix(token.TokenType.Gt, Parser.parse_infix_expression);
    try p.register_infix(token.TokenType.Asterix, Parser.parse_infix_expression);
    try p.register_infix(token.TokenType.Slash, Parser.parse_infix_expression);
    try p.register_infix(token.TokenType.Equal, Parser.parse_infix_expression);
    try p.register_infix(token.TokenType.NotEqual, Parser.parse_infix_expression);

    p.next_token();
    p.next_token();

    return p;
}

test "let statement parser" {
    const input =
        \\ let x = 5;
        \\ let y = 10;
        \\ let foo = 69;
    ;

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const l = try lexer.New(allocator, input);
    const p = try New(allocator, l);
    const program = try p.parse_program();

    const tests = [_]struct { expected_name: []const u8 }{
        .{ .expected_name = "x" },
        .{ .expected_name = "y" },
        .{ .expected_name = "foo" },
    };
    assert(program.statements.len == tests.len);
    for (program.statements, 0..) |stmt, i| {
        try test_let_statement(allocator, stmt, tests[i].expected_name, null);
    }
}

test "return statement parser" {
    const input =
        \\ return 5;
        \\ return 10;
        \\ return 69;
    ;

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const l = try lexer.New(allocator, input);
    const p = try New(allocator, l);
    const program = try p.parse_program();

    const tests = [_]struct { expected_return_value: []const u8 }{
        .{ .expected_return_value = "5" },
        .{ .expected_return_value = "10" },
        .{ .expected_return_value = "69" },
    };
    assert(program.statements.len == tests.len);
    for (program.statements, 0..) |stmt, i| {
        try test_return_statement(allocator, stmt, tests[i].expected_return_value);
    }
}

fn test_return_statement(allocator: mem.Allocator, stmt: ast.Statement, expected_return_value: []const u8) !void {
    assert(@as(std.meta.Tag(ast.Statement), stmt) == .return_statement);

    try testing.expectEqualStrings(stmt.return_statement.token_literal(), "return");
    try testing.expectEqualStrings(stmt.return_statement.return_value.token_literal(allocator), expected_return_value);
}

fn test_let_statement(allocator: mem.Allocator, stmt: ast.Statement, expected_name: []const u8, expected_value: ?[]const u8) !void {
    assert(@as(std.meta.Tag(ast.Statement), stmt) == .let_statement);

    try testing.expectEqualStrings(expected_name, stmt.let_statement.name.value);
    try testing.expectEqualStrings(expected_name, stmt.let_statement.name.token_literal(allocator));

    if (expected_value) |v| {
        try testing.expectEqualStrings(v, stmt.let_statement.value.token_literal(allocator));
    }
}

test "prefix expression parsing" {
    const input =
        \\ let x = -5;
        \\ let y = !5;
    ;

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const l = try lexer.New(allocator, input);
    const p = try New(allocator, l);
    const program = try p.parse_program();

    const tests = [_]struct { expected_name: []const u8, expected_value: []const u8 }{
        .{ .expected_name = "x", .expected_value = "-5" },
        .{ .expected_name = "y", .expected_value = "!5" },
    };
    assert(program.statements.len == tests.len);
    for (program.statements, 0..) |stmt, i| {
        try test_let_statement(allocator, stmt, tests[i].expected_name, tests[i].expected_value);
    }
}

test "infix expression parsing" {
    const input =
        \\ let a = 5 - 5;
        \\ let b = 5 + 5;
        \\ let c = 5 * 5;
        \\ let d = 5 / 5;
        \\ let e = 5 == 5;
        \\ let f = 5 != 5;
        \\ let g = 5 > 5;
        \\ let h = 5 < 5;
    ;

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const l = try lexer.New(allocator, input);
    const p = try New(allocator, l);
    const program = try p.parse_program();

    const tests = [_]struct { expected_name: []const u8, expected_value: []const u8 }{
        .{ .expected_name = "a", .expected_value = "5-5" },
        .{ .expected_name = "b", .expected_value = "5+5" },
        .{ .expected_name = "c", .expected_value = "5*5" },
        .{ .expected_name = "d", .expected_value = "5/5" },
        .{ .expected_name = "e", .expected_value = "5==5" },
        .{ .expected_name = "f", .expected_value = "5!=5" },
        .{ .expected_name = "g", .expected_value = "5>5" },
        .{ .expected_name = "h", .expected_value = "5<5" },
    };
    assert(program.statements.len == tests.len);
    for (program.statements, 0..) |stmt, i| {
        try test_let_statement(allocator, stmt, tests[i].expected_name, tests[i].expected_value);
    }
}
