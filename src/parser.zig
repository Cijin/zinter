const std = @import("std");
const mem = std.mem;
const testing = std.testing;
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

test "let statement parser" {
    const input =
        \\ let x = 5;
        \\ let y = 10;
        \\ let foo = 69
    ;

    const l = try lexer.New(testing.allocator, input);
    defer testing.destroy(l);
    const p = New(l, testing.allocator);
    defer testing.destroy(p);

    const program = p.parse_program();

    const tests = [_][3]u8{ "x", "y", "foo" };
    for (tests) |t| {
        // Todo: create test let statement fn
        _ = t;
        _ = program;
    }
}
