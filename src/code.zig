const std = @import("std");
const mem = std.mem;
const testing = std.testing;
const assert = std.debug.assert;

const Opcode = enum(u8) {
    opConstant,

    pub fn lookup_definition(self: Opcode) definition {
        return switch (self) {
            Opcode.opConstant => definition{ .name = "opconstant", .width = &.{2} },
        };
    }
};

const definition = struct {
    name: []const u8,
    width: []const u4,
};

fn make(op: Opcode, operands: []const u64) []u8 {
    const width = op.lookup_definition().width;

    // Todo: limit array size to instruction width
    var expected_ins_size: u16 = 1;
    for (width) |w| {
        expected_ins_size += w;
    }

    assert(expected_ins_size < 128);
    // Todo: fix this at some point
    var instruction: [128]u8 = undefined;
    instruction[0] = @intFromEnum(op);
    // Todo: copmtime thingy error
    // Todo: also fix the usize type
    var ins_idx: usize = 1;

    for (operands, width) |o, w| {
        assert(ins_idx < expected_ins_size);

        switch (w) {
            2 => {
                const high_byte: u8 = @intCast(o >> 8);
                const low_byte: u8 = @intCast(o & 0xFF);
                instruction[ins_idx] = high_byte;
                ins_idx += 1;
                instruction[ins_idx] = low_byte;
                ins_idx += 1;
            },
            else => unreachable,
        }
    }

    return instruction[0..ins_idx];
}

test "make instructions methods" {
    const tests = [_]struct { opcode: Opcode, operand: []const u64, expected_bytes: []const u8 }{
        .{ .opcode = Opcode.opConstant, .operand = &.{65534}, .expected_bytes = &.{ 255, 254 } },
        .{ .opcode = Opcode.opConstant, .operand = &.{1}, .expected_bytes = &.{ 0, 1 } },
        .{ .opcode = Opcode.opConstant, .operand = &.{0}, .expected_bytes = &.{ 0, 0 } },
    };

    for (tests) |t| {
        const instruction = make(t.opcode, t.operand);
        assert(1 + t.expected_bytes.len == instruction.len);

        assert(@intFromEnum(t.opcode) == instruction[0]);
        for (t.expected_bytes, 0..) |b, i| {
            try testing.expectEqual(b, instruction[i + 1]);
        }
    }
}
