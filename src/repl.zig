const std = @import("std");
const io = std.io;
const lexer = @import("lexer.zig");
const parser = @import("parser.zig");
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
    const l = lexer.New(allocator, result) catch |err| {
        std.debug.print("Unable to parse input, lexer failed with error={any}\n", .{err});
        return;
    };
    const p = parser.New(allocator, l) catch |err| {
        std.debug.print("Unable to parse input, parser failed with error={any}\n", .{err});
        return;
    };
    // Todo: when using arena allocator, this is not required
    // refer to parser test allocations
    defer {
        allocator.destroy(p);
        allocator.destroy(l);
    }

    const program = p.parse_program();
    for (program.statements) |stmt| {
        try stdout.print("{s}\n", .{stmt.token_literal()});
    }

    try bw.flush();
}
