const std = @import("std");
const lexer = @import("lexer.zig");
const token = @import("token.zig");
const ast = @import("ast.zig");
const mem = std.mem;
const fmt = std.fmt;
const testing = std.testing;
const assert = std.debug.assert;

const infix_parse_fn = *const fn (p: *Parser, left: ast.Expression) ast.Expression;
const prefix_parse_fn = *const fn (p: *Parser) ast.Expression;

const precedence = enum(u4) {
    lowest = 1,
    equals,
    less_greater,
    sum,
    product,
    prefix,
    call,
};

const Parser = struct {
    l: *lexer.lexer,
    cur_token: token.Token,
    peek_token: token.Token,
    allocator: mem.Allocator,
    // Todo: fix this at some point
    errors: [32][]const u8,
    error_count: u32,
    prefix_parse_fns: std.AutoHashMap(token.TokenType, prefix_parse_fn),
    infix_parse_fns: std.AutoHashMap(token.TokenType, infix_parse_fn),

    // Todo: create a method to handle errors better, i.e. keep the actual error instead of just a string
    fn next_token(self: *Parser) void {
        assert(self.cur_token.token_type != token.TokenType.Eof);

        self.cur_token = self.peek_token;
        self.peek_token = self.l.next_token();
    }

    fn peek_token_is(self: *Parser, expected: token.TokenType) bool {
        return self.peek_token.token_type == expected;
    }

    fn peek_error(self: *Parser, expected: token.TokenType) !void {
        self.errors[self.error_count] = try fmt.allocPrint(self.allocator, "expected next token to be {s} but found {s}\n", .{ @tagName(expected), @tagName(self.peek_token.token_type) });
        self.error_count += 1;
    }

    fn expect_peek(self: *Parser, expected: token.TokenType) bool {
        if (self.peek_token_is(expected)) {
            self.next_token();
            return true;
        }

        self.peek_error(expected) catch unreachable;
        return false;
    }

    pub fn parse_program(self: *Parser) ast.Program {
        var stmts: [4096]ast.Statement = undefined;
        var idx: usize = 0;
        while (self.cur_token.token_type != token.TokenType.Eof) {
            assert(idx < self.l.input.len);
            const stmt = self.parse_statement();
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

    fn parse_statement(self: *Parser) ?ast.Statement {
        switch (self.cur_token.token_type) {
            token.TokenType.Let => {
                const let_stmt = self.parse_let_statement();
                return ast.Statement{ .let_statement = let_stmt };
            },
            token.TokenType.Return => {
                const return_stmt = self.parse_return_statement();
                if (return_stmt) |stmt| {
                    return ast.Statement{ .return_statement = stmt };
                }

                return null;
            },
            // Todo: update to handle expressions
            else => return null,
        }
    }

    fn parse_return_statement(self: *Parser) ?ast.ReturnStatement {
        var return_stmt = ast.ReturnStatement{
            .token = self.cur_token,
            .return_value = undefined,
        };

        self.next_token();

        const p = self.cur_precedence();
        return_stmt.return_value = self.parse_expression(p);

        return return_stmt;
    }

    fn parse_let_statement(self: *Parser) ast.LetStatement {
        var let_stmt = ast.LetStatement{
            .token = self.cur_token,
            .name = undefined,
            .value = undefined,
        };

        if (!self.expect_peek(token.TokenType.Ident)) {
            return let_stmt;
        }

        const ident = ast.Identifier{
            .token = self.cur_token,
            .value = self.cur_token.literal,
        };
        let_stmt.name = ident;

        if (!self.expect_peek(token.TokenType.Assign)) {
            return let_stmt;
        }
        self.next_token();

        const p = self.cur_precedence();
        let_stmt.value = self.parse_expression(p);

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

    fn parse_identifier(self: *Parser) ast.Expression {
        return ast.Expression{ .identifier = ast.Identifier{ .token = self.cur_token, .value = self.cur_token.literal } };
    }

    fn parse_integer(self: *Parser) ast.Expression {
        const parsed_int = fmt.parseInt(i64, self.cur_token.literal, 10) catch {
            self.errors[self.error_count] = "failed to parse integer literal";
            self.error_count += 1;

            // Todo: this can be done better
            return ast.Expression{ .integer = ast.Integer{ .token = self.cur_token, .value = -1 } };
        };

        return ast.Expression{ .integer = ast.Integer{ .token = self.cur_token, .value = parsed_int } };
    }

    fn parse_prefix_expression(self: *Parser) ast.Expression {
        var prefix_expression = self.allocator.create(ast.PrefixExpression) catch unreachable;
        prefix_expression.* = ast.PrefixExpression{
            .token = self.cur_token,
            .operator = self.cur_token.literal,
            .right = undefined,
        };

        const p = self.cur_precedence();
        self.next_token();
        prefix_expression.right = self.parse_expression(p);

        return ast.Expression{ .prefix_expression = prefix_expression };
    }

    fn parse_infix_expression(self: *Parser, left: ast.Expression) ast.Expression {
        // Todo: handle this error
        const infix_expression = self.allocator.create(ast.InfixExpression) catch unreachable;
        infix_expression.* = ast.InfixExpression{
            .token = self.cur_token,
            .operator = self.cur_token.literal,
            .left = left,
            .right = undefined,
        };

        const p = self.cur_precedence();
        self.next_token();
        infix_expression.right = self.parse_expression(p);

        return ast.Expression{ .infix_expression = infix_expression };
    }

    fn parse_expression(self: *Parser, p: precedence) ast.Expression {
        const prefix_fn = self.prefix_parse_fns.get(self.cur_token.token_type) orelse {
            self.errors[self.error_count] = fmt.allocPrint(self.allocator, "no prefix parse fn found for: {s}", .{self.cur_token.literal}) catch unreachable;
            self.error_count += 1;

            return ast.Expression{ .nil_expression = ast.NilExpression{ .token = token.Token{ .token_type = token.TokenType.Illegal, .literal = self.cur_token.literal } } };
        };
        var left_exp = prefix_fn(self);

        // Todo: does the below statement get parsed as expected?
        // 1 + 2 * 3
        while (!self.peek_token_is(token.TokenType.Semicolon) and @intFromEnum(p) < @intFromEnum(self.peek_precedence())) {
            const infix = self.infix_parse_fns.get(self.peek_token.token_type) orelse {
                self.errors[self.error_count] = fmt.allocPrint(self.allocator, "no infix parse fn found for: {s}", .{self.cur_token.literal}) catch unreachable;
                self.error_count += 1;

                return ast.Expression{ .nil_expression = ast.NilExpression{ .token = token.Token{ .token_type = token.TokenType.Illegal, .literal = self.cur_token.literal } } };
            };

            self.next_token();
            left_exp = infix(self, left_exp);
        }

        return left_exp;
    }
};

var precedence_look_up: std.AutoHashMap(token.TokenType, precedence) = undefined;
// Todo:
// 1. Test expressions with integers
// 2. Test prefix expressions
pub fn New(allocator: mem.Allocator, l: *lexer.lexer) !*Parser {
    // Todo: what about product & prefix?
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
        .errors = undefined,
        .error_count = 0,
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
    const program = p.parse_program();
    assert(p.error_count == 0);

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
    const program = p.parse_program();
    assert(p.error_count == 0);

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
    const program = p.parse_program();
    assert(p.error_count == 0);

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
    const program = p.parse_program();
    assert(p.error_count == 0);

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
