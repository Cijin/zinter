const TokenType = enum {
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

const Token = struct {
    Type: TokenType,
    //Literal: not sure what this should be yet
};
