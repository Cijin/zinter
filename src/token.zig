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
