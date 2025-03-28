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

pub const token = struct {
    token_type: TokenType,
    literal: []const u8,
};

pub fn new_token(t: TokenType, l: []const u8) token {
    return token{ .token_type = t, .literal = l };
}

pub const keywords = [_]struct { key: []const u8, token: token }{
    .{ .key = "let", .token = token{ .token_type = TokenType.Let, .literal = "let" } },
    .{ .key = "fn", .token = token{ .token_type = TokenType.Function, .literal = "fn" } },
    .{ .key = "return", .token = token{ .token_type = TokenType.Return, .literal = "return" } },
    .{ .key = "if", .token = token{ .token_type = TokenType.If, .literal = "if" } },
    .{ .key = "else", .token = token{ .token_type = TokenType.Else, .literal = "else" } },
    .{ .key = "true", .token = token{ .token_type = TokenType.True, .literal = "true" } },
    .{ .key = "false", .token = token{ .token_type = TokenType.False, .literal = "false" } },
};

pub fn lookup_keyword(k: []const u8) ?token {
    for (keywords) |keyword| {
        if (std.mem.eql(u8, k, keyword.key)) {
            return keyword.token;
        }
    }

    return null;
}
