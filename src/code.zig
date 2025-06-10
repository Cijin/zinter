const std = @import("std");
const mem = std.mem;
const testing = std.testing;
const assert = std.debug.assert;

pub const Opcode = enum {
    opConstant,
    opAdd,
    opSub,
    opMul,
    opDiv,
    opFalse,
    opTrue,
    opNotEqual,
    opEqual,
    opLt,
    opGt,
    opMinus,
    opNot,
    opJumpNtTrue,
    opJump,
    opPop,
    opNull,

    pub fn lookup_definition(self: Opcode) definition {
        return switch (self) {
            .opConstant => definition{
                .name = "opconstant",
                .operandWidth = &.{2},
            },
            .opAdd => definition{
                .name = "opAdd",
                .operandWidth = &.{},
            },
            .opSub => definition{
                .name = "opSub",
                .operandWidth = &.{},
            },
            .opMul => definition{
                .name = "opMul",
                .operandWidth = &.{},
            },
            .opDiv => definition{
                .name = "opDiv",
                .operandWidth = &.{},
            },
            .opTrue => definition{
                .name = "opTrue",
                .operandWidth = &.{},
            },
            .opFalse => definition{
                .name = "opFalse",
                .operandWidth = &.{},
            },
            .opEqual => definition{
                .name = "opEqual",
                .operandWidth = &.{},
            },
            .opNotEqual => definition{
                .name = "opNotEqual",
                .operandWidth = &.{},
            },
            .opGt => definition{
                .name = "opGt",
                .operandWidth = &.{},
            },
            .opLt => definition{
                .name = "opLt",
                .operandWidth = &.{},
            },
            .opNot => definition{
                .name = "opNot",
                .operandWidth = &.{},
            },
            .opMinus => definition{
                .name = "opMinus",
                .operandWidth = &.{},
            },
            .opJumpNtTrue => definition{
                .name = "opJumpIfTrue",
                .operandWidth = &.{2},
            },
            .opJump => definition{
                .name = "opJump",
                .operandWidth = &.{2},
            },
            .opPop => definition{
                .name = "opPop",
                .operandWidth = &.{},
            },
            .opNull => definition{
                .name = "opNull",
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
        .{ .opcode = Opcode.opJumpNtTrue, .operand = &.{7}, .expected_bytes = &.{ @intFromEnum(Opcode.opJumpNtTrue), 0, 7 } },
        .{ .opcode = Opcode.opJump, .operand = &.{0}, .expected_bytes = &.{ @intFromEnum(Opcode.opJump), 0, 0 } },
        .{ .opcode = Opcode.opAdd, .operand = &.{}, .expected_bytes = &.{@intFromEnum(Opcode.opAdd)} },
        .{ .opcode = Opcode.opSub, .operand = &.{}, .expected_bytes = &.{@intFromEnum(Opcode.opSub)} },
        .{ .opcode = Opcode.opMul, .operand = &.{}, .expected_bytes = &.{@intFromEnum(Opcode.opMul)} },
        .{ .opcode = Opcode.opDiv, .operand = &.{}, .expected_bytes = &.{@intFromEnum(Opcode.opDiv)} },
        .{ .opcode = Opcode.opTrue, .operand = &.{}, .expected_bytes = &.{@intFromEnum(Opcode.opTrue)} },
        .{ .opcode = Opcode.opEqual, .operand = &.{}, .expected_bytes = &.{@intFromEnum(Opcode.opEqual)} },
        .{ .opcode = Opcode.opNotEqual, .operand = &.{}, .expected_bytes = &.{@intFromEnum(Opcode.opNotEqual)} },
        .{ .opcode = Opcode.opGt, .operand = &.{}, .expected_bytes = &.{@intFromEnum(Opcode.opGt)} },
        .{ .opcode = Opcode.opLt, .operand = &.{}, .expected_bytes = &.{@intFromEnum(Opcode.opLt)} },
        .{ .opcode = Opcode.opMinus, .operand = &.{}, .expected_bytes = &.{@intFromEnum(Opcode.opMinus)} },
        .{ .opcode = Opcode.opNot, .operand = &.{}, .expected_bytes = &.{@intFromEnum(Opcode.opNot)} },
    };

    for (tests) |t| {
        const instruction = make(t.opcode, t.operand, allocator);
        assert(t.expected_bytes.len == instruction.len);

        try testing.expectEqualSlices(u8, t.expected_bytes, instruction);
    }
}
