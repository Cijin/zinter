const std = @import("std");
const mem = std.mem;
const fmt = std.fmt;
const testing = std.testing;
const assert = std.debug.assert;

const lexer = @import("lexer.zig");
const token = @import("token.zig");
const ast = @import("ast.zig");

const parser = struct {
    l: *lexer.lexer,
    cur_token: token.Token,
    peek_token: token.Token,
    allocator: mem.Allocator,
    errors: [32][]const u8,
    error_count: u32,

    fn next_token(self: *parser) void {
        assert(self.cur_token.token_type != token.TokenType.Eof);

        self.cur_token = self.peek_token;
        self.peek_token = self.l.next_token();
    }

    fn peek_token_is(self: *parser, expected: token.TokenType) bool {
        return self.peek_token.token_type == expected;
    }

    fn peek_error(self: *parser, expected: token.TokenType) !void {
        self.errors[self.error_count] = try fmt.allocPrint(self.allocator, "expected next token to be {s} but found {s}\n", .{ @tagName(expected), @tagName(self.peek_token.token_type) });
        self.error_count += 1;
    }

    fn expect_peek(self: *parser, expected: token.TokenType) bool {
        if (self.peek_token_is(expected)) {
            self.next_token();
            return true;
        }

        self.peek_error(expected) catch unreachable;
        return false;
    }

    fn parse_program(self: *parser) ast.Program {
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

    fn parse_statement(self: *parser) ?ast.Statement {
        switch (self.cur_token.token_type) {
            token.TokenType.Let => {
                const let_stmt = self.parse_let_statement();
                if (let_stmt) |stmt| {
                    return ast.Statement{ .let_statement = stmt };
                }

                return null;
            },
            token.TokenType.Return => {
                const return_stmt = self.parse_return_statement();
                if (return_stmt) |stmt| {
                    return ast.Statement{ .return_statement = stmt };
                }

                return null;
            },
            else => return null,
        }
    }

    fn parse_return_statement(self: *parser) ?ast.ReturnStatement {
        const return_stmt = ast.ReturnStatement{
            .token = self.cur_token,
            .return_value = undefined,
        };

        self.next_token();

        // Todo: implement expression evaluation
        while (!self.peek_token_is(token.TokenType.Semicolon)) {
            self.next_token();
        }

        return return_stmt;
    }

    fn parse_let_statement(self: *parser) ?ast.LetStatement {
        var let_stmt = ast.LetStatement{
            .token = self.cur_token,
            .name = undefined,
            .value = undefined,
        };

        if (!self.expect_peek(token.TokenType.Ident)) {
            return null;
        }

        const ident = ast.Identifier{
            .token = self.cur_token,
            .value = self.cur_token.literal,
        };
        let_stmt.name = ident;

        // Todo: implement expression evaluation
        while (!self.peek_token_is(token.TokenType.Semicolon)) {
            self.next_token();
        }

        return let_stmt;
    }
};

fn New(allocator: mem.Allocator, l: *lexer.lexer) !*parser {
    var p = try allocator.create(parser);
    p.* = parser{
        .l = l,
        .cur_token = undefined,
        .peek_token = undefined,
        .allocator = allocator,
        .errors = undefined,
        .error_count = 0,
    };

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

    const l = try lexer.New(testing.allocator, input);
    defer testing.allocator.destroy(l);
    const p = try New(testing.allocator, l);
    defer testing.allocator.destroy(p);

    const program = p.parse_program();
    assert(p.error_count == 0);

    const tests = [_]struct { expected_name: []const u8 }{
        .{ .expected_name = "x" },
        .{ .expected_name = "y" },
        .{ .expected_name = "foo" },
    };
    assert(program.statements.len == tests.len);
    for (program.statements, 0..) |stmt, i| {
        try test_let_statement(stmt, tests[i].expected_name);
    }
}

test "return statement parser" {
    const input =
        \\ return 5;
        \\ return 10;
        \\ return 69;
    ;

    const l = try lexer.New(testing.allocator, input);
    defer testing.allocator.destroy(l);
    const p = try New(testing.allocator, l);
    defer testing.allocator.destroy(p);

    const program = p.parse_program();
    assert(p.error_count == 0);

    const tests = [_]struct { expected_return_value: []const u8 }{
        .{ .expected_return_value = "5" },
        .{ .expected_return_value = "10" },
        .{ .expected_return_value = "69" },
    };
    assert(program.statements.len == tests.len);
    for (program.statements) |stmt| {
        try test_return_statement(stmt);
    }
}

fn test_return_statement(stmt: ast.Statement) !void {
    assert(@as(std.meta.Tag(ast.Statement), stmt) == .return_statement);

    // Todo: test return_value.
    try testing.expectEqualStrings(stmt.return_statement.token_literal(), "return");
}

fn test_let_statement(stmt: ast.Statement, expected_name: []const u8) !void {
    assert(@as(std.meta.Tag(ast.Statement), stmt) == .let_statement);

    try testing.expectEqualStrings(stmt.let_statement.name.value, expected_name);
    try testing.expectEqualStrings(stmt.let_statement.name.token_literal(), expected_name);
}
