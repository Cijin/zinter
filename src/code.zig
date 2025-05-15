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

    var instruction_size = 1;
    for (width) |w| {
        instruction_size += w;
    }

    var instruction: [instruction_size]u8 = undefined;
    instruction[0] = op;
    // Todo: copmtime thingy error
    var ins_idx = 1;

    for (operands) |o| {
        switch (width) {
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
}

test "make instructions methods" {
    const tests = [_]struct { opcode: Opcode, operand: []const u64, expected_bytes: []const u8 }{
        .{ .opcode = Opcode.opConstant, .operand = &.{65534}, .expected_bytes = &.{ 255, 254 } },
    };

    for (tests) |t| {
        const instruction = make(t.opcode, t.operand);
        assert(1 + t.expected_bytes.len == instruction.len);

        assert(t.opcode == instruction[0]);
        for (t.expected_bytes, 0..) |b, i| {
            try testing.expectEqual(b, instruction[i + 1]);
        }
    }
}
