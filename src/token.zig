const std = @import("std");

pub const TokenType = enum {
    Illegal,
    Eof,
    Ident,
    Int,
    Bang,
    Assign,
    Equal,
    NotEqual,
    Plus,
    Minus,
    Asterix,
    Slash,
    Comma,
    Lt,
    Gt,
    Semicolon,
    Lparen,
    Rparen,
    Lbrace,
    Rbrace,
    Let,
    True,
    False,
    If,
    Else,
    Function,
    Return,
};

pub const Token = struct {
    token_type: TokenType,
    literal: []const u8,
};

pub fn new_token(t: TokenType, l: []const u8) Token {
    return Token{ .token_type = t, .literal = l };
}

pub const keywords = [_]struct { key: []const u8, token: Token }{
    .{ .key = "let", .token = Token{ .token_type = TokenType.Let, .literal = "let" } },
    .{ .key = "fn", .token = Token{ .token_type = TokenType.Function, .literal = "fn" } },
    .{ .key = "return", .token = Token{ .token_type = TokenType.Return, .literal = "return" } },
    .{ .key = "if", .token = Token{ .token_type = TokenType.If, .literal = "if" } },
    .{ .key = "else", .token = Token{ .token_type = TokenType.Else, .literal = "else" } },
    .{ .key = "true", .token = Token{ .token_type = TokenType.True, .literal = "true" } },
    .{ .key = "false", .token = Token{ .token_type = TokenType.False, .literal = "false" } },
};

pub fn lookup_keyword(k: []const u8) ?Token {
    for (keywords) |keyword| {
        if (std.mem.eql(u8, k, keyword.key)) {
            return keyword.token;
        }
    }

    return null;
}
