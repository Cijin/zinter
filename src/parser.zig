const std = @import("std");
const mem = std.mem;
const testing = std.testing;
const assert = std.debug.assert;

const lexer = @import("lexer.zig");
const token = @import("token.zig");
const ast = @import("ast.zig");

const parser = struct {
    l: *lexer.lexer,
    cur_token: token.Token,
    peek_token: token.Token,

    fn next_token(self: *parser) void {
        self.cur_token = self.peek_token;
        self.peek_token = self.l.next_token();
    }

    fn peek_token_is(self: *parser, expected: token.TokenType) bool {
        return self.peek_token.token_type == expected;
    }

    fn expect_peek(self: *parser, expected: token.TokenType) bool {
        if (self.peek_token_is(expected)) {
            self.next_token();
            return true;
        }

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
            else => return null,
        }
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

fn test_let_statement(stmt: ast.Statement, expected_name: []const u8) !void {
    assert(@TypeOf(stmt) == ast.Statement);
    assert(@as(std.meta.Tag(ast.Statement), stmt) == .let_statement);

    try testing.expectEqualStrings(stmt.let_statement.name.value, expected_name);
    try testing.expectEqualStrings(stmt.let_statement.name.token_literal(), expected_name);
}
