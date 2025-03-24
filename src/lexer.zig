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
            '=' => token.token{ .token_type = token.TokenType.Assign, .literal = "=" },
            ';' => token.token{ .token_type = token.TokenType.Semicolon, .literal = ";" },
            ',' => token.token{ .token_type = token.TokenType.Comma, .literal = "," },
            '(' => token.token{ .token_type = token.TokenType.Lparen, .literal = "(" },
            ')' => token.token{ .token_type = token.TokenType.Rparen, .literal = ")" },
            '{' => token.token{ .token_type = token.TokenType.Lbrace, .literal = "{" },
            '}' => token.token{ .token_type = token.TokenType.Rbrace, .literal = "}" },
            '+' => token.token{ .token_type = token.TokenType.Plus, .literal = "+" },
            '0' => token.token{ .token_type = token.TokenType.Eof, .literal = undefined },
            // Todo: handle let, ident, ints, return and fn
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
    const tests = [_]struct { expected_type: token.TokenType, expected_literal: []const u8 }{
        .{ .expected_type = token.TokenType.Assign, .expected_literal = "=" },
        .{ .expected_type = token.TokenType.Plus, .expected_literal = "+" },
        .{ .expected_type = token.TokenType.Lparen, .expected_literal = "(" },
        .{ .expected_type = token.TokenType.Rparen, .expected_literal = ")" },
        .{ .expected_type = token.TokenType.Lbrace, .expected_literal = "{" },
        .{ .expected_type = token.TokenType.Rbrace, .expected_literal = "}" },
        .{ .expected_type = token.TokenType.Comma, .expected_literal = "," },
        .{ .expected_type = token.TokenType.Semicolon, .expected_literal = ";" },
    };

    const l = try New(testing.allocator, input);
    defer testing.allocator.destroy(l);
    for (0..tests.len) |i| {
        const t = l.next_token();

        try expect(tests[i].expected_type == t.token_type);
        try expect(mem.eql(u8, tests[i].expected_literal, t.literal));
    }
}

test "next token with source code" {
    const input =
        \\ let five = 5;
        \\ let ten = 10;
        \\  
        \\ let add = fn(x, y) {
        \\  return x + y;
        \\ };
        \\
        \\ let result = add(five, ten);
    ;
    const tests = [_]struct { expected_type: token.TokenType, expected_literal: []const u8 }{
        .{ .expected_type = token.TokenType.Let, .expected_literal = "let" },
        .{ .expected_type = token.TokenType.Ident, .expected_literal = "five" },
        .{ .expected_type = token.TokenType.Assign, .expected_literal = "=" },
        .{ .expected_type = token.TokenType.Int, .expected_literal = "5" },
        .{ .expected_type = token.TokenType.Semicolon, .expected_literal = ";" },
        .{ .expected_type = token.TokenType.Let, .expected_literal = "let" },
        .{ .expected_type = token.TokenType.Ident, .expected_literal = "ten" },
        .{ .expected_type = token.TokenType.Assign, .expected_literal = "=" },
        .{ .expected_type = token.TokenType.Int, .expected_literal = "10" },
        .{ .expected_type = token.TokenType.Semicolon, .expected_literal = ";" },
        .{ .expected_type = token.TokenType.Let, .expected_literal = "let" },
        .{ .expected_type = token.TokenType.Ident, .expected_literal = "add" },
        .{ .expected_type = token.TokenType.Assign, .expected_literal = "=" },
        .{ .expected_type = token.TokenType.Function, .expected_literal = "fn" },
        .{ .expected_type = token.TokenType.Lparen, .expected_literal = "(" },
        .{ .expected_type = token.TokenType.Ident, .expected_literal = "x" },
        .{ .expected_type = token.TokenType.Comma, .expected_literal = "," },
        .{ .expected_type = token.TokenType.Ident, .expected_literal = "y" },
        .{ .expected_type = token.TokenType.Rparen, .expected_literal = ")" },
        .{ .expected_type = token.TokenType.Lbrace, .expected_literal = "{" },
        .{ .expected_type = token.TokenType.Return, .expected_literal = "return" },
        .{ .expected_type = token.TokenType.Ident, .expected_literal = "x" },
        .{ .expected_type = token.TokenType.Plus, .expected_literal = "+" },
        .{ .expected_type = token.TokenType.Ident, .expected_literal = "y" },
        .{ .expected_type = token.TokenType.Semicolon, .expected_literal = ";" },
        .{ .expected_type = token.TokenType.Rbrace, .expected_literal = "}" },
        .{ .expected_type = token.TokenType.Semicolon, .expected_literal = ";" },
        .{ .expected_type = token.TokenType.Let, .expected_literal = "let" },
        .{ .expected_type = token.TokenType.Ident, .expected_literal = "result" },
        .{ .expected_type = token.TokenType.Assign, .expected_literal = "=" },
        .{ .expected_type = token.TokenType.Ident, .expected_literal = "add" },
        .{ .expected_type = token.TokenType.Lparen, .expected_literal = "(" },
        .{ .expected_type = token.TokenType.Ident, .expected_literal = "five" },
        .{ .expected_type = token.TokenType.Comma, .expected_literal = "," },
        .{ .expected_type = token.TokenType.Ident, .expected_literal = "ten" },
        .{ .expected_type = token.TokenType.Rparen, .expected_literal = ")" },
        .{ .expected_type = token.TokenType.Semicolon, .expected_literal = ";" },
    };

    const l = try New(testing.allocator, input);
    defer testing.allocator.destroy(l);
    for (0..tests.len) |i| {
        const t = l.next_token();

        try expect(tests[i].expected_type == t.token_type);
        try expect(mem.eql(u8, tests[i].expected_literal, t.literal));
    }
}
