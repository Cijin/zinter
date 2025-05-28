const std = @import("std");
const mem = std.mem;
const testing = std.testing;
const assert = std.debug.assert;

pub const Opcode = enum {
    opConstant,
    opAdd,

    pub fn lookup_definition(self: Opcode) definition {
        return switch (self) {
            Opcode.opConstant => definition{
                .name = "opconstant",
                .operandWidth = &.{2},
            },
            Opcode.opAdd => definition{
                .name = "opAdd",
                .operandWidth = &.{},
            },
        };
    }
};

const definition = struct {
    name: []const u8,
    operandWidth: []const u4,
};

pub fn make(op: Opcode, operands: []const u64, allocator: mem.Allocator) []u8 {
    var instruction = std.ArrayList(u8).init(allocator);
    const width = op.lookup_definition().operandWidth;

    var expected_ins_size: u16 = 1;
    for (width) |w| {
        expected_ins_size += w;
    }

    instruction.append(@intFromEnum(op)) catch unreachable;

    for (operands, width) |o, w| {
        switch (w) {
            2 => {
                const high_byte: u8 = @intCast(o >> 8);
                const low_byte: u8 = @intCast(o & 0xFF);

                instruction.append(high_byte) catch unreachable;
                instruction.append(low_byte) catch unreachable;
            },
            else => unreachable,
        }
    }

    return instruction.items;
}

test "make instructions methods" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const tests = [_]struct { opcode: Opcode, operand: []const u64, expected_bytes: []const u8 }{
        .{ .opcode = Opcode.opConstant, .operand = &.{65534}, .expected_bytes = &.{ @intFromEnum(Opcode.opConstant), 255, 254 } },
        .{ .opcode = Opcode.opConstant, .operand = &.{1}, .expected_bytes = &.{ @intFromEnum(Opcode.opConstant), 0, 1 } },
        .{ .opcode = Opcode.opConstant, .operand = &.{0}, .expected_bytes = &.{ @intFromEnum(Opcode.opConstant), 0, 0 } },
        .{ .opcode = Opcode.opAdd, .operand = &.{}, .expected_bytes = &.{@intFromEnum(Opcode.opAdd)} },
    };

    for (tests) |t| {
        const instruction = make(t.opcode, t.operand, allocator);
        assert(t.expected_bytes.len == instruction.len);

        try testing.expectEqualSlices(u8, t.expected_bytes, instruction);
    }
}
