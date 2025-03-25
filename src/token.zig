const std = @import("std");

pub const TokenType = enum {
    Illegal,
    Eof,
    Ident,
    Int,
    Assign,
    Plus,
    Comma,
    Semicolon,
    Lparen,
    Rparen,
    Lbrace,
    Rbrace,
    Let,
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
};

pub fn lookup_keyword(k: []const u8) ?token {
    for (keywords) |keyword| {
        if (std.mem.eql(u8, k, keyword.key)) {
            return keyword.token;
        }
    }

    return null;
}
