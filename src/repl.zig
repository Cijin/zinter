const std = @import("std");
const io = std.io;
const lexer = @import("lexer.zig");
const parser = @import("parser.zig");
const token = @import("token.zig");
const compiler = @import("compiler.zig");
const vm = @import("vm.zig");
const ast = @import("ast.zig");

const prompt = ">>";

pub fn start() !void {
    const stdout_file = io.getStdOut().writer();
    var bw = io.bufferedWriter(stdout_file);
    const stdout = bw.writer();

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var c = try compiler.New(allocator);
    var virtual_machine = try vm.New(c.byte_code(), allocator);

    while (true) {
        try stdout.print("Welcome to zinter v0.0.1.\nCTRL + c to exit.\n{s} ", .{prompt});
        try bw.flush();

        const stdin = io.getStdIn().reader();
        var buffer: [1024]u8 = undefined;
        const result = try stdin.readUntilDelimiter(&buffer, '\n');

        const l = lexer.New(allocator, result) catch |err| {
            std.debug.print("Unable to parse input, lexer failed with error={any}\n", .{err});
            return;
        };
        const p = parser.New(allocator, l) catch |err| {
            std.debug.print("Unable to parse input, parser failed with error={any}\n", .{err});
            return;
        };

        const program = p.parse_program() catch |err| {
            std.debug.print("Unable to parse program, parser failed with error={any}\n", .{err});
            return;
        };

        // Todo: fix OOM error
        c = try compiler.NewWithState(allocator, c.constants, c.global_var);
        c.compile(ast.Node{ .program = program }) catch |err| {
            std.debug.print("Unable to compile program, compiler failed with error={any}\n", .{err});
            return;
        };
        virtual_machine = try vm.NewWithState(c.byte_code(), allocator, virtual_machine.stack, virtual_machine.globals);
        virtual_machine.run() catch |err| {
            std.debug.print("Unable to execute program,runtime error={any}\n", .{err});
            return;
        };

        try stdout.print("{s}\n", .{virtual_machine.last_popped().inspect(allocator)});

        try bw.flush();
    }
}
