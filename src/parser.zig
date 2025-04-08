const std = @import("std");
const mem = std.mem;
const lexer = @import("lexer");
const token = @import("token");

const parser = struct {
    l: *lexer.lexer,
    cur_token: token.Token,
    peek_token: token.Token,

    fn next_token(self: *parser) void {
        self.cur_token = self.peek_token();
        self.peek_token = self.l.next_token();
    }
};

fn New(l: *lexer.lexer, _: mem.Allocator) *parser {
    var p = &parser{
        .l = l,
        .cur_token = undefined,
        .peek_token = undefined,
    };

    p.next_token();
    p.next_token();

    return p;
}
