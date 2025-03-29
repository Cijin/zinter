const std = @import("std");
const repl = @import("repl.zig");
const lexer = @import("lexer.zig");

pub fn main() !void {
    repl.start() catch |err| {
        std.debug.print("Unable to start repl: {any}\n", .{err});
        return;
    };
}
