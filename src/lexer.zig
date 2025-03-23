const std = @import("std");
const mem = std.mem;
const testing = std.testing;
const expect = testing.expect;
const token = @import("token.zig");

const lexer = struct {
    input: []const u8,
    position: usize,
    read_position: usize,
    ch: u8,

    fn read_char(l: *lexer) void {
        if (l.read_position >= l.input.len) {
            l.ch = 0;
        } else {
            l.ch = l.input[l.read_position];
        }
        l.position = l.read_position;
        l.read_position += 1;
    }

    fn next_token(l: *lexer) token.token {
        var t: token.token = undefined;
        t = switch (l.ch) {
            '=' => token.token{ .token_type = token.TokenType.Assign, .literal = l.ch },
            ';' => token.token{ .token_type = token.TokenType.Semicolon, .literal = l.ch },
            ',' => token.token{ .token_type = token.TokenType.Comma, .literal = l.ch },
            '(' => token.token{ .token_type = token.TokenType.Lparen, .literal = l.ch },
            ')' => token.token{ .token_type = token.TokenType.Rparen, .literal = l.ch },
            '{' => token.token{ .token_type = token.TokenType.Lbrace, .literal = l.ch },
            '}' => token.token{ .token_type = token.TokenType.Rbrace, .literal = l.ch },
            '+' => token.token{ .token_type = token.TokenType.Plus, .literal = l.ch },
            '0' => token.token{ .token_type = token.TokenType.Eof, .literal = undefined },
            else => unreachable,
        };
        l.read_char();
        return t;
    }
};

fn New(allocator: mem.Allocator, input: []const u8) !*lexer {
    // Todo: use arena allocator, ensure arena.deinit() is called when the
    // interpreter is done
    // https://ziglang.org/documentation/0.14.0/#Choosing-an-Allocator
    var l = try allocator.create(lexer);
    l.* = lexer{
        .input = input,
        .position = 0,
        .read_position = 0,
        .ch = 0,
    };
    l.read_char();
    return l;
}

test "next token method" {
    const input = "=+(){},;";
    const tests = [_]struct { expected_type: token.TokenType, expected_literal: u8 }{
        .{ .expected_type = token.TokenType.Assign, .expected_literal = '=' },
        .{ .expected_type = token.TokenType.Plus, .expected_literal = '+' },
        .{ .expected_type = token.TokenType.Lparen, .expected_literal = '(' },
        .{ .expected_type = token.TokenType.Rparen, .expected_literal = ')' },
        .{ .expected_type = token.TokenType.Lbrace, .expected_literal = '{' },
        .{ .expected_type = token.TokenType.Rbrace, .expected_literal = '}' },
        .{ .expected_type = token.TokenType.Comma, .expected_literal = ',' },
        .{ .expected_type = token.TokenType.Semicolon, .expected_literal = ';' },
    };

    const l = try New(testing.allocator, input);
    defer testing.allocator.destroy(l);
    for (0..tests.len) |i| {
        const t = l.next_token();

        try expect(tests[i].expected_type == t.token_type);
        try expect(tests[i].expected_literal == t.literal);
    }
}
