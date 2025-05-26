const std = @import("std");
const mem = std.mem;
const testing = std.testing;
const assert = std.debug.assert;

const lexer = @import("lexer.zig");
const parser = @import("parser.zig");
const ast = @import("ast.zig");
const object = @import("object.zig");
const compiler = @import("compiler.zig");
const code = @import("code.zig");
// Todo:
// 2. loop through instructions

const stack_size = 2048;

const RuntimeError = error{
    StackOverflow,
};

const VM = struct {
    constants: []object.Object,
    instructions: []u8,
    stack: [stack_size]object.Object,
    sp: u32,

    pub fn run(self: *VM) RuntimeError!void {
        for (self.instructions, 0..) |instr, i| {
            const opcode: code.Opcode = @enumFromInt(instr);
            switch (opcode) {
                .opConstant => {
                    // [00000001, 10000000]
                    // [00000000  00000000]
                    // [00000000  00000001]
                    // << 8 [00000001  00000000]
                    // || [0000000  10000000]
                    var const_idx: u16 = @intCast(self.instructions[i + 1]);
                    const_idx <<= 8;
                    const_idx |= @intCast(self.instructions[i + 2]);
                    i += 2;

                    assert(const_idx < self.constants.len);
                    self.push(self.constants[const_idx]);
                },
                else => unreachable,
            }
        }
    }

    fn push(self: *VM, obj: object.Object) RuntimeError!void {
        if (self.sp >= stack_size) {
            return RuntimeError.StackOverflow;
        }

        self.stack[self.sp].* = obj;
        self.sp += 1;
    }

    fn stack_top(self: *VM) object.Object {
        return self.stack[self.sp];
    }
};

fn New(b: compiler.ByteCode, allocator: mem.Allocator) !*VM {
    const vm = try allocator.create(VM);
    vm.* = VM{
        .constants = b.constants,
        .instructions = b.instructions,
        .stack = undefined,
        .sp = 0,
    };

    return vm;
}

test "virtual machine run" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const tests = [_]struct {
        input: []const u8,
        expectedInt: i64,
    }{
        .{
            .input = "1;",
            .expectedInt = 1,
        },
        .{
            .input = "2;",
            .expectedInt = 2,
        },
        .{
            .input = "1+2;",
            .expectedInt = 2,
        },
    };

    for (tests) |t| {
        const l = try lexer.New(allocator, t.input);
        const p = try parser.New(allocator, l);
        const program = try p.parse_program();

        var c = try compiler.New(allocator);
        try c.compile(ast.Node{ .program = program });
        const vm = try New(c.byte_code(), allocator);

        const expected = object.Object{
            .integer = object.Integer{
                .value = t.expectedInt,
            },
        };

        try testing.expectEqual(expected, vm.stack_top());
    }
}
