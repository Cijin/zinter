const std = @import("std");
const io = std.io;
const lexer = @import("lexer.zig");
const token = @import("token.zig");

const prompt = ">>";

pub fn start() !void {
    const stdout_file = io.getStdOut().writer();
    var bw = io.bufferedWriter(stdout_file);
    const stdout = bw.writer();

    try stdout.print("Welcome to zinter v0.0.1.\n{s} ", .{prompt});
    try bw.flush();

    const stdin = io.getStdIn().reader();
    var buffer: [1024]u8 = undefined;
    const result = try stdin.readUntilDelimiter(&buffer, '\n');

    var program_buffer: [4096]u8 = undefined;
    var fba = std.heap.FixedBufferAllocator.init(&program_buffer);
    const allocator = fba.allocator();
    // Todo: write lexer errors to stderr
    const l = try lexer.New(allocator, result);
    defer allocator.destroy(l);

    var tok = l.next_token();
    while (tok.token_type != token.TokenType.Eof) {
        try stdout.print("TokenType = {s} | Literal = '{s}'\n", .{ @tagName(tok.token_type), tok.literal });
        tok = l.next_token();
    }

    try bw.flush();
}
