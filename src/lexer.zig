const std = @import("std");
const mem = std.mem;
const assert = std.debug.assert;
const testing = std.testing;
const token = @import("token.zig");

const lexer = struct {
    input: []const u8,
    position: i64,
    read_position: i64,
    ch: u8,

    fn read_char(l: *lexer) void {
        assert(l.position <= l.input.len);

        if (l.read_position >= l.input.len) {
            l.ch = 0;
        } else {
            l.ch = l.input[@intCast(l.read_position)];
        }
        l.position = l.read_position;
        l.read_position += 1;
    }

    fn peek_char(l: *lexer) u8 {
        assert(l.position <= l.input.len);

        if (l.read_position >= l.input.len) {
            return 0;
        }

        return l.input[@intCast(l.read_position)];
    }

    pub fn next_token(l: *lexer) token.token {
        var t: token.token = undefined;

        l.skip_white_space();

        t = switch (l.ch) {
            '=' => blk: {
                if (l.peek_char() == '=') {
                    l.read_char();
                    break :blk token.token{ .token_type = token.TokenType.Equal, .literal = "==" };
                }

                break :blk token.token{ .token_type = token.TokenType.Assign, .literal = "=" };
            },
            '!' => blk: {
                if (l.peek_char() == '=') {
                    l.read_char();
                    break :blk token.token{ .token_type = token.TokenType.NotEqual, .literal = "!=" };
                }

                break :blk token.token{ .token_type = token.TokenType.Bang, .literal = "!" };
            },
            ',' => token.token{ .token_type = token.TokenType.Comma, .literal = "," },
            '(' => token.token{ .token_type = token.TokenType.Lparen, .literal = "(" },
            ')' => token.token{ .token_type = token.TokenType.Rparen, .literal = ")" },
            '{' => token.token{ .token_type = token.TokenType.Lbrace, .literal = "{" },
            '}' => token.token{ .token_type = token.TokenType.Rbrace, .literal = "}" },
            '+' => token.token{ .token_type = token.TokenType.Plus, .literal = "+" },
            '-' => token.token{ .token_type = token.TokenType.Minus, .literal = "-" },
            '/' => token.token{ .token_type = token.TokenType.Slash, .literal = "/" },
            '*' => token.token{ .token_type = token.TokenType.Asterix, .literal = "*" },
            '<' => token.token{ .token_type = token.TokenType.Lt, .literal = "<" },
            '>' => token.token{ .token_type = token.TokenType.Gt, .literal = ">" },
            ';' => token.token{ .token_type = token.TokenType.Semicolon, .literal = ";" },
            0 => token.token{ .token_type = token.TokenType.Eof, .literal = undefined },
            else => {
                const start_position: usize = @intCast(l.position);
                var end_position: usize = 0;
                if (is_letter(l.ch)) {
                    l.read_identifier();
                    end_position = @intCast(l.position);
                    if (token.lookup_keyword(l.input[start_position..end_position])) |keyword_token| {
                        t = keyword_token;
                        return t;
                    }
                    return token.token{ .token_type = token.TokenType.Ident, .literal = l.input[start_position..end_position] };
                } else if (is_integer(l.ch)) {
                    l.read_integer();
                    end_position = @intCast(l.position);
                    return token.token{ .token_type = token.TokenType.Int, .literal = l.input[start_position..end_position] };
                } else {
                    return token.token{ .token_type = token.TokenType.Illegal, .literal = l.input[start_position .. start_position + 1] };
                }
            },
        };
        l.read_char();
        return t;
    }

    fn read_identifier(l: *lexer) void {
        while (is_letter(l.ch)) {
            l.read_char();
        }
    }

    fn read_integer(l: *lexer) void {
        while (is_integer(l.ch)) {
            l.read_char();
        }
    }

    fn skip_white_space(l: *lexer) void {
        while (is_white_space(l.ch)) {
            l.read_char();
        }
    }
};

pub fn New(allocator: mem.Allocator, input: []const u8) !*lexer {
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

fn is_integer(ch: u8) bool {
    return ch >= '0' and ch <= '9';
}

fn is_letter(ch: u8) bool {
    return (ch >= 'a' and ch <= 'z') or (ch >= 'A' and ch <= 'Z') or ch == '_';
}

fn is_white_space(c: u8) bool {
    return switch (c) {
        ' ', '\t'...'\r' => true,
        else => false,
    };
}

test "is_letter method" {
    const tests = [_]struct { input: u8, expected: bool }{
        .{ .input = 'a', .expected = true },
        .{ .input = 'z', .expected = true },
        .{ .input = 'A', .expected = true },
        .{ .input = 'Z', .expected = true },
        .{ .input = '0', .expected = false },
        .{ .input = '9', .expected = false },
        .{ .input = ' ', .expected = false },
        .{ .input = '_', .expected = true },
        .{ .input = '.', .expected = false },
        .{ .input = '@', .expected = false },
        .{ .input = '[', .expected = false },
        .{ .input = '`', .expected = false },
        .{ .input = 127, .expected = false },
    };

    for (0..tests.len) |i| {
        try testing.expectEqual(tests[i].expected, is_letter(tests[i].input));
    }
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

        try testing.expectEqual(tests[i].expected_type, t.token_type);
        try testing.expectEqualSlices(u8, tests[i].expected_literal, t.literal);
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
        \\ !/*-
        \\ <>
        \\ if (5 > 10) {
        \\  return true;
        \\ } else {
        \\  return false;
        \\ }
        \\ 10 == 10;
        \\ 10 != 9;
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
        .{ .expected_type = token.TokenType.Bang, .expected_literal = "!" },
        .{ .expected_type = token.TokenType.Slash, .expected_literal = "/" },
        .{ .expected_type = token.TokenType.Asterix, .expected_literal = "*" },
        .{ .expected_type = token.TokenType.Minus, .expected_literal = "-" },
        .{ .expected_type = token.TokenType.Lt, .expected_literal = "<" },
        .{ .expected_type = token.TokenType.Gt, .expected_literal = ">" },
        .{ .expected_type = token.TokenType.If, .expected_literal = "if" },
        .{ .expected_type = token.TokenType.Lparen, .expected_literal = "(" },
        .{ .expected_type = token.TokenType.Int, .expected_literal = "5" },
        .{ .expected_type = token.TokenType.Gt, .expected_literal = ">" },
        .{ .expected_type = token.TokenType.Int, .expected_literal = "10" },
        .{ .expected_type = token.TokenType.Rparen, .expected_literal = ")" },
        .{ .expected_type = token.TokenType.Lbrace, .expected_literal = "{" },
        .{ .expected_type = token.TokenType.Return, .expected_literal = "return" },
        .{ .expected_type = token.TokenType.True, .expected_literal = "true" },
        .{ .expected_type = token.TokenType.Semicolon, .expected_literal = ";" },
        .{ .expected_type = token.TokenType.Rbrace, .expected_literal = "}" },
        .{ .expected_type = token.TokenType.Else, .expected_literal = "else" },
        .{ .expected_type = token.TokenType.Lbrace, .expected_literal = "{" },
        .{ .expected_type = token.TokenType.Return, .expected_literal = "return" },
        .{ .expected_type = token.TokenType.False, .expected_literal = "false" },
        .{ .expected_type = token.TokenType.Semicolon, .expected_literal = ";" },
        .{ .expected_type = token.TokenType.Rbrace, .expected_literal = "}" },
        .{ .expected_type = token.TokenType.Int, .expected_literal = "10" },
        .{ .expected_type = token.TokenType.Equal, .expected_literal = "==" },
        .{ .expected_type = token.TokenType.Int, .expected_literal = "10" },
        .{ .expected_type = token.TokenType.Semicolon, .expected_literal = ";" },
        .{ .expected_type = token.TokenType.Int, .expected_literal = "10" },
        .{ .expected_type = token.TokenType.NotEqual, .expected_literal = "!=" },
        .{ .expected_type = token.TokenType.Int, .expected_literal = "9" },
        .{ .expected_type = token.TokenType.Semicolon, .expected_literal = ";" },
    };

    const l = try New(testing.allocator, input);
    defer testing.allocator.destroy(l);
    for (0..tests.len) |i| {
        const t = l.next_token();

        try testing.expectEqual(tests[i].expected_type, t.token_type);
        try testing.expectEqualSlices(u8, tests[i].expected_literal, t.literal);
    }
}
