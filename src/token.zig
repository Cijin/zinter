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
    Function,
    Let,
};

pub const token = struct {
    token_type: TokenType,
    literal: u8,
};

pub fn new_token(t: TokenType, l: u8) token {
    return token{ .token_type = t, .literal = l };
}
