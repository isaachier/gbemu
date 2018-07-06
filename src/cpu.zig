const std = @import("std");
const builtin = @import("builtin");

pub const Registers = struct {
    af: u16,
    bc: u16,
    de: u16,
    hl: u16,
    sp: u16,
    pc: u16,

    pub fn a(self: *const Registers) u8 {
        return @truncate(u8, self.af >> 8);
    }

    pub fn setA(self: *Registers, value: u8) void {
        return self.af = (u16(value) << 8) | self.f();
    }

    pub fn b(self: *const Registers) u8 {
        return @truncate(u8, self.bc >> 8);
    }

    pub fn setB(self: *Registers, value: u8) void {
        return self.bc = (u16(value) << 8) | self.c();
    }

    pub fn c(self: *const Registers) u8 {
        return @truncate(u8, self.bc);
    }

    pub fn setC(self: *Registers, value: u8) void {
        return self.bc = (u16(self.b()) << 8) | value;
    }

    pub fn d(self: *const Registers) u8 {
        return @truncate(u8, self.de >> 8);
    }

    pub fn setD(self: *Registers, value: u8) void {
        return self.de = (u16(value) << 8) | self.e();
    }

    pub fn e(self: *const Registers) u8 {
        return @truncate(u8, self.de);
    }

    pub fn setE(self: *Registers, value: u8) void {
        return self.de = (u16(self.d()) << 8) | value;
    }

    pub fn f(self: *const Registers) u8 {
        return @truncate(u8, self.af);
    }

    pub fn setF(self: *Registers, value: u8) void {
        return self.af = (u16(self.a()) << 8) | value;
    }

    pub fn h(self: *const Registers) u8 {
        return @truncate(u8, self.hl >> 8);
    }

    pub fn setH(self: *Registers, value: u8) void {
        return self.hl = (u16(value) << 8) | self.l();
    }

    pub fn l(self: *const Registers) u8 {
        return @truncate(u8, self.hl);
    }

    pub fn setL(self: *Registers, value: u8) void {
        return self.hl = (u16(self.h()) << 8) | value;
    }

    const zero_flag_mask : u8 = 0x80;
    const subtract_flag_mask : u8 = 0x40;
    const half_carry_flag_mask : u8 = 0x20;
    const carry_flag_mask : u8 = 0x10;

    pub fn zeroFlag(self: *const Registers) bool {
        return (self.f() & zero_flag_mask) != 0;
    }

    pub fn setZeroFlag(self: *Registers, value: bool) void {
        if (value == true) {
            self.setF(self.f() | zero_flag_mask);
        } else {
            self.setF(self.f() & (~zero_flag_mask));
        }
    }

    pub fn subtractFlag(self: *const Registers) bool {
        return (self.f() & subtract_flag_mask) != 0;
    }

    pub fn setSubtractFlag(self: *Registers, value: bool) void {
        if (value == true) {
            self.setF(self.f() | subtract_flag_mask);
        } else {
            self.setF(self.f() & (~subtract_flag_mask));
        }
    }

    pub fn halfCarryFlag(self: *const Registers) bool {
        return (self.f() & half_carry_flag_mask) != 0;
    }

    pub fn setHalfCarryFlag(self: *Registers, value: bool) void {
        if (value == true) {
            self.setF(self.f() | half_carry_flag_mask);
        } else {
            self.setF(self.f() & (~half_carry_flag_mask));
        }
    }

    pub fn carryFlag(self: *const Registers) bool {
        return (self.f() & carry_flag_mask) != 0;
    }

    pub fn setCarryFlag(self: *Registers, value: bool) void {
        if (value == true) {
            self.setF(self.f() | carry_flag_mask);
        } else {
            self.setF(self.f() & (~carry_flag_mask));
        }
    }
};

test "Registers" {
    var registers : Registers = undefined;
    registers.af = 0xFF11;
    std.debug.assert(registers.a() == 0xFF);
    std.debug.assert(registers.f() == 0x11);
    registers.setA(0x11);
    registers.setF(0x55);
    std.debug.assert(registers.a() == 0x11);
    std.debug.assert(registers.f() == 0x55);

    registers.bc = 0xFF11;
    std.debug.assert(registers.b() == 0xFF);
    std.debug.assert(registers.c() == 0x11);
    registers.setB(0x11);
    registers.setC(0x55);
    std.debug.assert(registers.b() == 0x11);
    std.debug.assert(registers.c() == 0x55);

    registers.de = 0xFF11;
    std.debug.assert(registers.d() == 0xFF);
    std.debug.assert(registers.e() == 0x11);
    registers.setD(0x11);
    registers.setE(0x55);
    std.debug.assert(registers.d() == 0x11);
    std.debug.assert(registers.e() == 0x55);

    registers.hl = 0xFF11;
    std.debug.assert(registers.h() == 0xFF);
    std.debug.assert(registers.l() == 0x11);
    registers.setH(0x11);
    registers.setL(0x55);
    std.debug.assert(registers.h() == 0x11);
    std.debug.assert(registers.l() == 0x55);
}

pub const Memory = struct {
    const memory_len = 0xFFFF - 0x2000;

    allocator: *std.mem.Allocator,
    memory: []u8,

    pub fn init(allocator: *std.mem.Allocator) !Memory {
        return Memory{
            .allocator = allocator,
            .memory = try allocator.alloc(u8, memory_len),
        };
    }

    pub fn deinit(self: *Memory) void {
        self.allocator.free(self.memory);
    }

    fn internalIndex(index: u16) u16 {
        if (index < 0xE000) {
            return index;
        }
        return index - 0x2000;
    }

    pub fn get(self: *const Memory, index: u16) u8 {
        return self.memory[internalIndex(index)];
    }

    pub fn set(self: *Memory, index: u16, value: u8) void {
        self.memory[internalIndex(index)] = value;
    }

    pub fn sliceConst(self: *const Memory, index: u16, len: usize) []const u8 {
        const offset = internalIndex(index);
        return self.memory[offset..offset + len];
    }

    pub fn slice(self: *Memory, index: u16, len: usize) []u8 {
        const offset = internalIndex(index);
        return self.memory[offset..offset + len];
    }
};

pub const CPU = struct {
    pub const ErrorSet = error{ InvalidInstruction, };
    pub const Stream = std.io.InStream(error{});
    pub const EmptyErrorSet = error{};
    pub const Mode = enum {
        Default,
        Halt,
        Stop,
        DisableInterrupts,
        EnableInterrupts,
    };

    registers: Registers,
    memory: Memory,
    stream: Stream,

    pub fn init(allocator: *std.mem.Allocator) !CPU {
        return CPU{
            .registers = undefined,
            .memory = try Memory.init(allocator),
            .stream = Stream{ .readFn = CPU.readFn },
        };
    }

    pub fn deinit(self: *CPU) void {
        self.memory.deinit();
    }

    fn readFn(in_stream: *Stream, buffer: []u8) EmptyErrorSet!usize {
        const self = @fieldParentPtr(CPU, "stream", in_stream);
        var len : usize = undefined;
        if (usize(self.registers.pc) + buffer.len > 0xFFFF) {
            len = 0xFFFF - self.registers.pc;
        } else {
            len = buffer.len;
        }
        std.mem.copy(u8, buffer, self.memory.sliceConst(self.registers.pc, len));
        self.registers.pc +%= @truncate(u16, len);
        return len;
    }

    fn push(self: *CPU, value: var) void {
        const len = @sizeOf(@typeOf(value));
        self.registers.sp -%= len;
        std.mem.writeInt(self.memory.slice(self.registers.sp, len), value, builtin.Endian.Little);
    }

    fn pop(self: *CPU, comptime T: type) T {
        const value = std.mem.readIntLE(T, self.memory.sliceConst(self.registers.sp, @sizeOf(T)));
        self.registers.sp +%= @sizeOf(T);
        return value;
    }

    fn add(self: *CPU, comptime T: type, a: T, b: T) T {
        const B = @IntType(false, @sizeOf(T) * 16);
        const high_test_bit_num_trailing_zeros = @sizeOf(T) * 8;
        const high_test_bit : B = 1 << high_test_bit_num_trailing_zeros;
        const high_operand_mask = high_test_bit - 1;
        const low_test_bit = high_test_bit >> 4;
        const low_operand_mask = (low_test_bit) - 1;
        const result = a + b;
        self.registers.setHalfCarryFlag(
            (((a & low_operand_mask) + (b & low_operand_mask)) & low_test_bit) == low_test_bit);
        self.registers.setCarryFlag(
            (((a & high_operand_mask) + (b & high_operand_mask)) & high_test_bit) == high_test_bit);
        self.registers.setZeroFlag(result == 0);
        self.registers.setSubtractFlag(false);
        return result;
    }

    fn sub(self: *CPU, a: u8, b: u8) u8 {
        const result = a - b;
        self.registers.setHalfCarryFlag((((a & 0xF) - (b & 0xF)) & 0x10) == 0x10);
        self.registers.setCarryFlag(((((a >> 4) & 0xF) - ((b >> 4) & 0xF)) & 0x10) == 0x10);
        self.registers.setZeroFlag(result == 0);
        self.registers.setSubtractFlag(true);
        return @bitCast(u8, result);
    }

    fn bitwiseAnd(self: *CPU, a: u8, b: u8) u8 {
        const result = a & b;
        self.registers.setHalfCarryFlag(true);
        self.registers.setCarryFlag(false);
        self.registers.setZeroFlag(result == 0);
        self.registers.setSubtractFlag(false);
        return result;
    }

    fn bitwiseOr(self: *CPU, a: u8, b: u8) u8 {
        const result = a | b;
        self.registers.setHalfCarryFlag(false);
        self.registers.setCarryFlag(false);
        self.registers.setZeroFlag(result == 0);
        self.registers.setSubtractFlag(false);
        return result;
    }

    fn bitwiseXor(self: *CPU, a: u8, b: u8) u8 {
        const result = a ^ b;
        self.registers.setHalfCarryFlag(false);
        self.registers.setCarryFlag(false);
        self.registers.setZeroFlag(result == 0);
        self.registers.setSubtractFlag(false);
        return result;
    }

    fn swap(self: *CPU, x: u8) u8 {
        const high_nibble = x & 0xF0;
        const low_nibble = x & 0x0F;
        const result = low_nibble << 4 | (high_nibble >> 4);
        self.registers.setHalfCarryFlag(false);
        self.registers.setCarryFlag(false);
        self.registers.setZeroFlag(result == 0);
        self.registers.setSubtractFlag(false);
        return result;
    }

    fn rlc(self: *CPU, x: u8) u8 {
        const result = (x << 1) | (x >> 7);
        self.registers.setCarryFlag((x & 0x80) != 0);
        self.registers.setHalfCarryFlag(false);
        self.registers.setSubtractFlag(false);
        self.registers.setZeroFlag(result == 0);
        return result;
    }

    fn rl(self: *CPU, x: u8) u8 {
        const carry_flag = @boolToInt(self.registers.carryFlag());
        const result = x << 1 | carry_flag;
        self.registers.setCarryFlag((x & 0x80) != 0);
        self.registers.setHalfCarryFlag(false);
        self.registers.setSubtractFlag(false);
        self.registers.setZeroFlag(result == 0);
        return result;
    }

    fn rrc(self: *CPU, x: u8) u8 {
        const result = (x << 7) | (x >> 1);
        self.registers.setCarryFlag((x & 0x01) != 0);
        self.registers.setHalfCarryFlag(false);
        self.registers.setSubtractFlag(false);
        self.registers.setZeroFlag(result == 0);
        return result;
    }

    fn rr(self: *CPU, x: u8) u8 {
        const result = (u8(@boolToInt(self.registers.carryFlag())) << 7) | (x >> 1);
        self.registers.setCarryFlag((x & 0x01) != 0);
        self.registers.setHalfCarryFlag(false);
        self.registers.setSubtractFlag(false);
        self.registers.setZeroFlag(result == 0);
        return result;
    }

    fn sla(self: *CPU, x: u8) u8 {
        const result = x << 1;
        self.registers.setCarryFlag((x & 0x80) != 0);
        self.registers.setHalfCarryFlag(false);
        self.registers.setSubtractFlag(false);
        self.registers.setZeroFlag(result == 0);
        std.debug.assert((result & 0x01) == 0);
        return result;
    }

    fn sra(self: *CPU, x: u8) u8 {
        const result = (x & 0x80) | x >> 1;
        self.registers.setCarryFlag((x & 0x01) != 0);
        self.registers.setHalfCarryFlag(false);
        self.registers.setSubtractFlag(false);
        self.registers.setZeroFlag(result == 0);
        std.debug.assert((result & 0x80) == (x & 0x80));
        return result;
    }

    fn srl(self: *CPU, x: u8) u8 {
        const result = x >> 1;
        self.registers.setCarryFlag((x & 0x01) != 0);
        self.registers.setHalfCarryFlag(false);
        self.registers.setSubtractFlag(false);
        self.registers.setZeroFlag(result == 0);
        std.debug.assert((result & 0x80) == 0);
        return result;
    }

    fn testBit(self: *CPU, x: u8, pos: u8) void {
        const n = @truncate(u3, pos);
        const result = (x & (u8(1) << n)) != 0;
        self.registers.setZeroFlag(!result);
        self.registers.setSubtractFlag(false);
        self.registers.setHalfCarryFlag(true);
    }

    pub fn execute(self: *CPU) !Mode {
        switch (try self.stream.readByte()) {
            0x00 => {
                // NOP
            },
            0x01 => {
                // LD BC,nn
                self.registers.bc = try self.stream.readIntLe(u16);
            },
            0x02 => {
                // LD (BC),A
                self.memory.set(self.registers.bc, self.registers.a());
            },
            0x03 => {
                // INC BC
                self.registers.bc = self.add(u16, self.registers.bc, 1);
            },
            0x04 => {
                // INC B
                self.registers.setB(self.add(u8, self.registers.b(), 1));
            },
            0x05 => {
                // DEC B
                self.registers.setB(self.sub(self.registers.b(), 1));
            },
            0x06 => {
                // LD B,n
                self.registers.setB(try self.stream.readByte());
            },
            0x07 => {
                // RLCA
                self.registers.setA(self.rlc(self.registers.a()));
            },
            0x08 => {
                // LD (nn),SP
                const value = try self.stream.readIntLe(u16);
                self.memory.set(value, @truncate(u8, (self.registers.sp & 0xFF00) >> 8));
                self.memory.set(value + 1, @truncate(u8, self.registers.sp));
            },
            0x09 => {
                // ADD HL,BC
                self.registers.hl = self.add(u16, self.registers.hl, self.registers.bc);
            },
            0x0A => {
                // LD A,(BC)
                self.registers.setA(self.memory.get(self.registers.bc));
            },
            0x0B => {
                // DEC BC
                self.registers.bc -%= 1;
            },
            0x0C => {
                // INC C
                self.registers.setC(self.add(u8, self.registers.c(), 1));
            },
            0x0D => {
                // DEC D
                self.registers.setD(self.sub(self.registers.d(), 1));
            },
            0x0E => {
                // LD C,n
                self.registers.setC(try self.stream.readByte());
            },
            0x0F => {
                // RRCA
                self.registers.setA(self.rrc(self.registers.a()));
            },
            0x10 => {
                // STOP
                switch (try self.stream.readByte()) {
                    0x00 => {
                        return Mode.Stop;
                    },
                    else => {
                        return ErrorSet.InvalidInstruction;
                    }
                }
            },
            0x11 => {
                // LD DE,nn
                self.registers.de = try self.stream.readIntLe(u16);
            },
            0x12 => {
                // LD (DE),A
                self.memory.set(self.registers.de, self.registers.a());
            },
            0x13 => {
                // INC DE
                self.registers.de = self.add(u16, self.registers.de, 1);
            },
            0x14 => {
                // INC D
                self.registers.setD(self.add(u8, self.registers.d(), 1));
            },
            0x15 => {
                // DEC D
                self.registers.setD(self.sub(self.registers.d(), 1));
            },
            0x16 => {
                // LD D,n
                self.registers.setD(try self.stream.readByte());
            },
            0x17 => {
                // RLA
                self.registers.setA(self.rl(self.registers.a()));
            },
            0x19 => {
                // ADD HL,DE
                self.registers.hl = self.add(u16, self.registers.hl, self.registers.de);
            },
            0x1A => {
                // LD A,(DE)
                self.registers.setA(self.memory.get(self.registers.de));
            },
            0x1B => {
                // DEC DE
                self.registers.de -%= 1;
            },
            0x1C => {
                // INC E
                self.registers.setE(self.add(u8, self.registers.e(), 1));
            },
            0x1D => {
                // DEC E
                self.registers.setE(self.sub(self.registers.e(), 1));
            },
            0x1E => {
                // LD E,n
                self.registers.setE(try self.stream.readByte());
            },
            0x1F => {
                // RRA
                self.registers.setA(self.rr(self.registers.a()));
            },
            0x21 => {
                // LD HL,nn
                self.registers.hl = try self.stream.readIntLe(u16);
            },
            0x22 => {
                // LDI (HL),A
                self.memory.set(self.registers.hl, self.registers.a());
                self.registers.hl +%= 1;
            },
            0x23 => {
                // INC HL
                self.registers.hl = self.add(u16, self.registers.hl, 1);
            },
            0x24 => {
                // INC H
                self.registers.setH(self.add(u8, self.registers.h(), 1));
            },
            0x25 => {
                // DEC H
                self.registers.setH(self.sub(self.registers.h(), 1));
            },
            0x26 => {
                // LD H,n
                self.registers.setH(try self.stream.readByte());
            },
            0x27 => {
                // DAA
                var carry = false;
                if (!self.registers.subtractFlag()) {
                    if (self.registers.carryFlag() or self.registers.a() > 0x99) {
                        self.registers.setA(self.registers.a() +% 0x60);
                        carry = true;
                    }
                    if (self.registers.halfCarryFlag() or (self.registers.a() & 0x0F) > 0x09) {
                        self.registers.setA(self.registers.a() +% 0x06);
                    }
                } else if (self.registers.carryFlag()) {
                    carry = true;
                    const adjustment = if (self.registers.halfCarryFlag()) u8(0x9A) else u8(0xA0);
                    self.registers.setA(self.registers.a() +% adjustment);
                } else if (self.registers.halfCarryFlag()) {
                    self.registers.setA(self.registers.a() +% 0xFA);
                }

                self.registers.setZeroFlag(self.registers.a() == 0);
                self.registers.setCarryFlag(carry);
                self.registers.setHalfCarryFlag(false);
            },
            0x29 => {
                // ADD HL,HL
                self.registers.hl = self.add(u16, self.registers.hl, self.registers.hl);
            },
            0x2A => {
                // LDI A,(HL)
                self.registers.setA(self.memory.get(self.registers.hl));
                self.registers.hl +%= 1;
            },
            0x2B => {
                // DEC HL
                self.registers.hl -%= 1;
            },
            0x2C => {
                // INC L
                self.registers.setL(self.add(u8, self.registers.l(), 1));
            },
            0x2D => {
                // DEC L
                self.registers.setL(self.sub(self.registers.l(), 1));
            },
            0x2E => {
                // LD L,n
                self.registers.setL(try self.stream.readByte());
            },
            0x2F => {
                // CPL
                self.registers.setA(~self.registers.a());
                self.registers.setSubtractFlag(true);
                self.registers.setHalfCarryFlag(true);
            },
            0x31 => {
                // LD SP,nn
                self.registers.sp = try self.stream.readIntLe(u16);
            },
            0x32 => {
                // LDD (HL),A
                self.memory.set(self.registers.hl, self.registers.a());
                self.registers.hl -%= 1;
            },
            0x33 => {
                // INC SP
                self.registers.sp = self.add(u16, self.registers.sp, 1);
            },
            0x34 => {
                // INC (HL)
                self.memory.set(self.registers.hl,
                                self.add(u8, self.memory.get(self.registers.hl), 1));
            },
            0x35 => {
                // DEC (HL)
                self.memory.set(self.registers.hl,
                                self.sub(self.memory.get(self.registers.hl), 1));
            },
            0x36 => {
                // LD (HL),n
                self.memory.set(self.registers.hl, try self.stream.readByte());
            },
            0x37 => {
                // SCF
                self.registers.setSubtractFlag(false);
                self.registers.setHalfCarryFlag(false);
                self.registers.setCarryFlag(true);
            },
            0x39 => {
                // ADD HL,SP
                self.registers.hl = self.add(u16, self.registers.hl, self.registers.sp);
            },
            0x3A => {
                // LDD A,(HL)
                self.registers.setA(self.memory.get(self.registers.hl));
                self.registers.hl -%= 1;
            },
            0x3B => {
                // DEC HL
                self.registers.hl -%= 1;
            },
            0x3C => {
                // INC A
                self.registers.setA(self.add(u8, self.registers.a(), 1));
            },
            0x3D => {
                // DEC A
                self.registers.setA(self.sub(self.registers.a(), 1));
            },
            0x3E => {
                // LD A,n
                self.registers.setA(try self.stream.readByte());
            },
            0x3F => {
                // CCF
                self.registers.setSubtractFlag(false);
                self.registers.setHalfCarryFlag(false);
                self.registers.setCarryFlag(!self.registers.carryFlag());
            },
            0x40 => {
                // LD B,B
                self.registers.setB(self.registers.b());
            },
            0x41 => {
                // LD B,C
                self.registers.setB(self.registers.c());
            },
            0x42 => {
                // LD B,D
                self.registers.setB(self.registers.d());
            },
            0x43 => {
                // LD B,E
                self.registers.setB(self.registers.e());
            },
            0x44 => {
                // LD B,H
                self.registers.setB(self.registers.h());
            },
            0x45 => {
                // LD B,L
                self.registers.setB(self.registers.l());
            },
            0x46 => {
                // LD B,(HL)
                self.registers.setB(self.memory.get(self.registers.hl));
            },
            0x47 => {
                // LD B,A
                self.registers.setB(self.registers.a());
            },
            0x48 => {
                // LD C,B
                self.registers.setC(self.registers.b());
            },
            0x49 => {
                // LD C,C
                self.registers.setC(self.registers.c());
            },
            0x4A => {
                // LD C,D
                self.registers.setC(self.registers.d());
            },
            0x4B => {
                // LD C,E
                self.registers.setC(self.registers.e());
            },
            0x4C => {
                // LD C,H
                self.registers.setC(self.registers.h());
            },
            0x4D => {
                // LD C,L
                self.registers.setC(self.registers.l());
            },
            0x4E => {
                // LD C,(HL)
                self.registers.setC(self.memory.get(self.registers.hl));
            },
            0x4F => {
                // LD C,A
                self.registers.setC(self.registers.a());
            },
            0x50 => {
                // LD D,B
                self.registers.setD(self.registers.b());
            },
            0x51 => {
                // LD D,C
                self.registers.setD(self.registers.c());
            },
            0x52 => {
                // LD D,D
                self.registers.setD(self.registers.d());
            },
            0x53 => {
                // LD D,E
                self.registers.setD(self.registers.e());
            },
            0x54 => {
                // LD D,H
                self.registers.setD(self.registers.h());
            },
            0x55 => {
                // LD D,L
                self.registers.setD(self.registers.l());
            },
            0x56 => {
                // LD D,(HL)
                self.registers.setD(self.memory.get(self.registers.hl));
            },
            0x57 => {
                // LD D,A
                self.registers.setD(self.registers.a());
            },
            0x58 => {
                // LD E,B
                self.registers.setE(self.registers.b());
            },
            0x59 => {
                // LD E,C
                self.registers.setE(self.registers.c());
            },
            0x5A => {
                // LD E,D
                self.registers.setE(self.registers.d());
            },
            0x5B => {
                // LD E,E
                self.registers.setE(self.registers.e());
            },
            0x5C => {
                // LD E,H
                self.registers.setE(self.registers.h());
            },
            0x5D => {
                // LD E,L
                self.registers.setE(self.registers.l());
            },
            0x5E => {
                // LD E,(HL)
                self.registers.setE(self.memory.get(self.registers.hl));
            },
            0x5F => {
                // LD E,A
                self.registers.setE(self.registers.a());
            },
            0x60 => {
                // LD H,B
                self.registers.setH(self.registers.b());
            },
            0x61 => {
                // LD H,C
                self.registers.setH(self.registers.c());
            },
            0x62 => {
                // LD H,D
                self.registers.setH(self.registers.d());
            },
            0x63 => {
                // LD H,E
                self.registers.setH(self.registers.e());
            },
            0x64 => {
                // LD H,H
                self.registers.setH(self.registers.h());
            },
            0x65 => {
                // LD H,L
                self.registers.setH(self.registers.l());
            },
            0x66 => {
                // LD H,(HL)
                self.registers.setH(self.memory.get(self.registers.hl));
            },
            0x67 => {
                // LD H,A
                self.registers.setH(self.registers.a());
            },
            0x68 => {
                // LD L,B
                self.registers.setL(self.registers.b());
            },
            0x69 => {
                // LD L,C
                self.registers.setL(self.registers.c());
            },
            0x6A => {
                // LD L,D
                self.registers.setL(self.registers.d());
            },
            0x6B => {
                // LD L,E
                self.registers.setL(self.registers.e());
            },
            0x6C => {
                // LD L,H
                self.registers.setL(self.registers.h());
            },
            0x6D => {
                // LD L,L
                self.registers.setL(self.registers.l());
            },
            0x6E => {
                // LD L,(HL)
                self.registers.setL(self.memory.get(self.registers.hl));
            },
            0x6F => {
                // LD L,A
                self.registers.setL(self.registers.a());
            },
            0x70 => {
                // LD (HL),B
                self.memory.set(self.registers.hl, self.registers.b());
            },
            0x71 => {
                // LD (HL),C
                self.memory.set(self.registers.hl, self.registers.c());
            },
            0x72 => {
                // LD (HL),D
                self.memory.set(self.registers.hl, self.registers.d());
            },
            0x73 => {
                // LD (HL),E
                self.memory.set(self.registers.hl, self.registers.e());
            },
            0x74 => {
                // LD (HL),H
                self.memory.set(self.registers.hl, self.registers.h());
            },
            0x75 => {
                // LD (HL),L
                self.memory.set(self.registers.hl, self.registers.l());
            },
            0x76 => {
                // HALT
                return Mode.Halt;
            },
            0x77 => {
                // LD (HL),A
                self.memory.set(self.registers.hl, self.registers.a());
            },
            0x78 => {
                // LD A,B
                self.registers.setA(self.registers.b());
            },
            0x79 => {
                // LD A,C
                self.registers.setA(self.registers.c());
            },
            0x7A => {
                // LD A,D
                self.registers.setA(self.registers.d());
            },
            0x7B => {
                // LD A,E
                self.registers.setA(self.registers.e());
            },
            0x7C => {
                // LD A,H
                self.registers.setA(self.registers.h());
            },
            0x7D => {
                // LD A,L
                self.registers.setA(self.registers.l());
            },
            0x7E => {
                // LD A,(HL)
                self.registers.setA(self.memory.get(self.registers.hl));
            },
            0x80 => {
                // ADD A,B
                self.registers.setA(self.add(u8, self.registers.a(), self.registers.b()));
            },
            0x81 => {
                // ADD A,C
                self.registers.setA(self.add(u8, self.registers.a(), self.registers.c()));
            },
            0x82 => {
                // ADD A,D
                self.registers.setA(self.add(u8, self.registers.a(), self.registers.d()));
            },
            0x83 => {
                // ADD A,E
                self.registers.setA(self.add(u8, self.registers.a(), self.registers.e()));
            },
            0x84 => {
                // ADD A,H
                self.registers.setA(self.add(u8, self.registers.a(), self.registers.h()));
            },
            0x85 => {
                // ADD A,L
                self.registers.setA(self.add(u8, self.registers.a(), self.registers.l()));
            },
            0x86 => {
                // ADD A,(HL)
                self.registers.setA(self.add(u8, self.registers.a(),
                                             self.memory.get(self.registers.hl)));
            },
            0x87 => {
                // ADD A,A
                self.registers.setA(self.add(u8, self.registers.a(), self.registers.a()));
            },
            0x88 => {
                // ADC A,B
                self.registers.setA(self.add(u8,
                    self.registers.a(),
                    self.registers.b() + @boolToInt(self.registers.carryFlag())));
            },
            0x89 => {
                // ADC A,C
                self.registers.setA(self.add(u8,
                    self.registers.a(),
                    self.registers.c() + @boolToInt(self.registers.carryFlag())));
            },
            0x8A => {
                // ADC A,D
                self.registers.setA(self.add(u8,
                    self.registers.a(),
                    self.registers.d() + @boolToInt(self.registers.carryFlag())));
            },
            0x8B => {
                // ADC A,E
                self.registers.setA(self.add(u8,
                    self.registers.a(),
                    self.registers.e() + @boolToInt(self.registers.carryFlag())));
            },
            0x8C => {
                // ADC A,H
                self.registers.setA(self.add(u8,
                    self.registers.a(),
                    self.registers.h() + @boolToInt(self.registers.carryFlag())));
            },
            0x8D => {
                // ADC A,L
                self.registers.setA(self.add(u8,
                    self.registers.a(),
                    self.registers.l() + @boolToInt(self.registers.carryFlag())));
            },
            0x8E => {
                // ADC A,(HL)
                self.registers.setA(self.add(u8,
                    self.registers.a(),
                    self.memory.get(self.registers.hl) + @boolToInt(self.registers.carryFlag())));
            },
            0x8F => {
                // ADC A,A
                self.registers.setA(self.add(u8,
                    self.registers.a(),
                    self.registers.a() + @boolToInt(self.registers.carryFlag())));
            },
            0x90 => {
                // SUB A,B
                self.registers.setA(self.sub(self.registers.a(), self.registers.b()));
            },
            0x91 => {
                // SUB A,C
                self.registers.setA(self.sub(self.registers.a(), self.registers.c()));
            },
            0x92 => {
                // SUB A,D
                self.registers.setA(self.sub(self.registers.a(), self.registers.d()));
            },
            0x93 => {
                // SUB A,E
                self.registers.setA(self.sub(self.registers.a(), self.registers.e()));
            },
            0x94 => {
                // SUB A,H
                self.registers.setA(self.sub(self.registers.a(), self.registers.h()));
            },
            0x95 => {
                // SUB A,L
                self.registers.setA(self.sub(self.registers.a(), self.registers.l()));
            },
            0x96 => {
                // SUB A,(HL)
                self.registers.setA(self.sub(
                    self.registers.a(),
                    self.memory.get(self.registers.hl)));
            },
            0x97 => {
                // SUB A,A
                self.registers.setA(self.sub(self.registers.a(), self.registers.a()));
            },
            0x98 => {
                // SBC A,B
                self.registers.setA(self.sub(
                    self.registers.a(),
                    self.registers.b() + @boolToInt(self.registers.carryFlag())));
            },
            0x99 => {
                // SBC A,C
                self.registers.setA(self.sub(
                    self.registers.a(),
                    self.registers.c() + @boolToInt(self.registers.carryFlag())));
            },
            0x9A => {
                // SBC A,D
                self.registers.setA(self.sub(
                    self.registers.a(),
                    self.registers.d() + @boolToInt(self.registers.carryFlag())));
            },
            0x9B => {
                // SBC A,E
                self.registers.setA(self.sub(
                    self.registers.a(),
                    self.registers.e() + @boolToInt(self.registers.carryFlag())));
            },
            0x9C => {
                // SBC A,H
                self.registers.setA(self.sub(
                    self.registers.a(),
                    self.registers.h() + @boolToInt(self.registers.carryFlag())));
            },
            0x9D => {
                // SBC A,L
                self.registers.setA(self.sub(
                    self.registers.a(),
                    self.registers.l() + @boolToInt(self.registers.carryFlag())));
            },
            0x9E => {
                // SBC A,(HL)
                self.registers.setA(self.sub(
                    self.registers.a(),
                    self.memory.get(self.registers.hl) + @boolToInt(self.registers.carryFlag())));
            },
            0x9F => {
                // SBC A,A
                self.registers.setA(self.sub(
                    self.registers.a(),
                    self.registers.a() + @boolToInt(self.registers.carryFlag())));
            },
            0xA0 => {
                // AND A,B
                self.registers.setA(self.bitwiseAnd(self.registers.a(), self.registers.b()));
            },
            0xA1 => {
                // AND A,C
                self.registers.setA(self.bitwiseAnd(self.registers.a(), self.registers.c()));
            },
            0xA2 => {
                // AND A,D
                self.registers.setA(self.bitwiseAnd(self.registers.a(), self.registers.d()));
            },
            0xA3 => {
                // AND A,E
                self.registers.setA(self.bitwiseAnd(self.registers.a(), self.registers.e()));
            },
            0xA4 => {
                // AND A,H
                self.registers.setA(self.bitwiseAnd(self.registers.a(), self.registers.h()));
            },
            0xA5 => {
                // AND A,L
                self.registers.setA(self.bitwiseAnd(self.registers.a(), self.registers.l()));
            },
            0xA6 => {
                // AND A,(HL)
                self.registers.setA(self.bitwiseAnd(
                    self.registers.a(),
                    self.memory.get(self.registers.hl)));
            },
            0xA7 => {
                // AND A,A
                self.registers.setA(self.bitwiseAnd(self.registers.a(), self.registers.a()));
            },
            0xA8 => {
                // XOR A,B
                self.registers.setA(self.bitwiseXor(self.registers.a(), self.registers.b()));
            },
            0xA9 => {
                // XOR A,C
                self.registers.setA(self.bitwiseXor(self.registers.a(), self.registers.c()));
            },
            0xAA => {
                // XOR A,D
                self.registers.setA(self.bitwiseXor(self.registers.a(), self.registers.d()));
            },
            0xAB => {
                // XOR A,E
                self.registers.setA(self.bitwiseXor(self.registers.a(), self.registers.e()));
            },
            0xAC => {
                // XOR A,H
                self.registers.setA(self.bitwiseXor(self.registers.a(), self.registers.h()));
            },
            0xAD => {
                // XOR A,L
                self.registers.setA(self.bitwiseXor(self.registers.a(), self.registers.l()));
            },
            0xAE => {
                // XOR A,(HL)
                self.registers.setA(self.bitwiseXor(
                    self.registers.a(),
                    self.memory.get(self.registers.hl)));
            },
            0xAF => {
                // XOR A,A
                self.registers.setA(self.bitwiseXor(self.registers.a(), self.registers.a()));
            },
            0xB0 => {
                // OR A,B
                self.registers.setA(self.bitwiseOr(self.registers.a(), self.registers.b()));
            },
            0xB1 => {
                // OR A,C
                self.registers.setA(self.bitwiseOr(self.registers.a(), self.registers.c()));
            },
            0xB2 => {
                // OR A,D
                self.registers.setA(self.bitwiseOr(self.registers.a(), self.registers.d()));
            },
            0xB3 => {
                // OR A,E
                self.registers.setA(self.bitwiseOr(self.registers.a(), self.registers.e()));
            },
            0xB4 => {
                // OR A,H
                self.registers.setA(self.bitwiseOr(self.registers.a(), self.registers.h()));
            },
            0xB5 => {
                // OR A,L
                self.registers.setA(self.bitwiseOr(self.registers.a(), self.registers.l()));
            },
            0xB6 => {
                // OR A,(HL)
                self.registers.setA(self.bitwiseOr(
                    self.registers.a(),
                    self.memory.get(self.registers.hl)));
            },
            0xB7 => {
                // OR A,A
                self.registers.setA(self.bitwiseOr(self.registers.a(), self.registers.a()));
            },
            0xB8 => {
                // CP A,B
                _ = self.sub(self.registers.a(), self.registers.b());
            },
            0xB9 => {
                // CP A,C
                _ = self.sub(self.registers.a(), self.registers.c());
            },
            0xBA => {
                // CP A,D
                _ = self.sub(self.registers.a(), self.registers.d());
            },
            0xBB => {
                // CP A,E
                _ = self.sub(self.registers.a(), self.registers.e());
            },
            0xBC => {
                // CP A,H
                _ = self.sub(self.registers.a(), self.registers.h());
            },
            0xBD => {
                // CP A,B
                _ = self.sub(self.registers.a(), self.registers.l());
            },
            0xBE => {
                // CP A,B
                _ = self.sub(self.registers.a(), self.memory.get(self.registers.hl));
            },
            0xBF => {
                // CP A,A
                _ = self.sub(self.registers.a(), self.registers.a());
            },
            0xC1 => {
                // POP BC
                self.registers.bc = self.pop(u16);
            },
            0xC5 => {
                // PUSH BC
                self.push(self.registers.bc);
            },
            0xC6 => {
                // ADD A,n
                self.registers.setA(self.add(u8, self.registers.a(), try self.stream.readByte()));
            },
            0xCB => {
                switch (try self.stream.readByte()) {
                    0x00 => {
                        // RLC B
                        self.registers.setB(self.rlc(self.registers.b()));
                    },
                    0x01 => {
                        // RLC C
                        self.registers.setC(self.rlc(self.registers.c()));
                    },
                    0x02 => {
                        // RLC D
                        self.registers.setD(self.rlc(self.registers.d()));
                    },
                    0x03 => {
                        // RLC E
                        self.registers.setE(self.rlc(self.registers.e()));
                    },
                    0x04 => {
                        // RLC H
                        self.registers.setH(self.rlc(self.registers.h()));
                    },
                    0x05 => {
                        // RLC L
                        self.registers.setL(self.rlc(self.registers.l()));
                    },
                    0x06 => {
                        // RLC (HL)
                        self.memory.set(self.registers.hl,
                                        self.rlc(self.memory.get(self.registers.hl)));
                    },
                    0x07 => {
                        // RLC A
                        self.registers.setA(self.rlc(self.registers.a()));
                    },
                    0x08 => {
                        // RRC B
                        self.registers.setB(self.rrc(self.registers.b()));
                    },
                    0x09 => {
                        // RRC C
                        self.registers.setC(self.rrc(self.registers.c()));
                    },
                    0x0A => {
                        // RRC D
                        self.registers.setD(self.rrc(self.registers.d()));
                    },
                    0x0B => {
                        // RRC E
                        self.registers.setE(self.rrc(self.registers.e()));
                    },
                    0x0C => {
                        // RRC H
                        self.registers.setH(self.rrc(self.registers.h()));
                    },
                    0x0D => {
                        // RRC L
                        self.registers.setL(self.rrc(self.registers.l()));
                    },
                    0x0E => {
                        // RRC (HL)
                        self.memory.set(self.registers.hl,
                                        self.rrc(self.memory.get(self.registers.hl)));
                    },
                    0x0F => {
                        // RRC A
                        self.registers.setA(self.rrc(self.registers.a()));
                    },
                    0x10 => {
                        // RL B
                        self.registers.setB(self.rl(self.registers.b()));
                    },
                    0x11 => {
                        // RL C
                        self.registers.setC(self.rl(self.registers.c()));
                    },
                    0x12 => {
                        // RL D
                        self.registers.setD(self.rl(self.registers.d()));
                    },
                    0x13 => {
                        // RL E
                        self.registers.setE(self.rl(self.registers.e()));
                    },
                    0x14 => {
                        // RL H
                        self.registers.setH(self.rl(self.registers.h()));
                    },
                    0x15 => {
                        // RL L
                        self.registers.setL(self.rl(self.registers.l()));
                    },
                    0x16 => {
                        // RL (HL)
                        self.memory.set(self.registers.hl,
                                        self.rl(self.memory.get(self.registers.hl)));
                    },
                    0x17 => {
                        // RL A
                        self.registers.setA(self.rl(self.registers.a()));
                    },
                    0x18 => {
                        // RR B
                        self.registers.setB(self.rr(self.registers.b()));
                    },
                    0x19 => {
                        // RR C
                        self.registers.setC(self.rr(self.registers.c()));
                    },
                    0x1A => {
                        // RR D
                        self.registers.setD(self.rr(self.registers.d()));
                    },
                    0x1B => {
                        // RR E
                        self.registers.setE(self.rr(self.registers.e()));
                    },
                    0x1C => {
                        // RR H
                        self.registers.setH(self.rr(self.registers.h()));
                    },
                    0x1D => {
                        // RR L
                        self.registers.setL(self.rr(self.registers.l()));
                    },
                    0x1E => {
                        // RR (HL)
                        self.memory.set(self.registers.hl,
                                        self.rr(self.memory.get(self.registers.hl)));
                    },
                    0x1F => {
                        // RR A
                        self.registers.setA(self.rr(self.registers.a()));
                    },
                    0x20 => {
                        // SLA B
                        self.registers.setB(self.sla(self.registers.b()));
                    },
                    0x21 => {
                        // SLA C
                        self.registers.setC(self.sla(self.registers.c()));
                    },
                    0x22 => {
                        // SLA D
                        self.registers.setD(self.sla(self.registers.d()));
                    },
                    0x23 => {
                        // SLA E
                        self.registers.setE(self.sla(self.registers.e()));
                    },
                    0x24 => {
                        // SLA H
                        self.registers.setH(self.sla(self.registers.h()));
                    },
                    0x25 => {
                        // SLA L
                        self.registers.setL(self.sla(self.registers.l()));
                    },
                    0x26 => {
                        // SLA (HL)
                        self.memory.set(self.registers.hl,
                                        self.sla(self.memory.get(self.registers.hl)));
                    },
                    0x27 => {
                        // SLA A
                        self.registers.setA(self.sla(self.registers.a()));
                    },
                    0x28 => {
                        // SRA B
                        self.registers.setB(self.sra(self.registers.b()));
                    },
                    0x29 => {
                        // SRA C
                        self.registers.setC(self.sra(self.registers.c()));
                    },
                    0x2A => {
                        // SRA D
                        self.registers.setD(self.sra(self.registers.d()));
                    },
                    0x2B => {
                        // SRA E
                        self.registers.setE(self.sra(self.registers.e()));
                    },
                    0x2C => {
                        // SRA H
                        self.registers.setH(self.sra(self.registers.h()));
                    },
                    0x2D => {
                        // SRA L
                        self.registers.setL(self.sra(self.registers.l()));
                    },
                    0x2E => {
                        // SRA (HL)
                        self.memory.set(self.registers.hl,
                                        self.sra(self.memory.get(self.registers.hl)));
                    },
                    0x2F => {
                        // SRA A
                        self.registers.setA(self.sra(self.registers.a()));
                    },
                    0x30 => {
                        // SWAP B
                        self.registers.setB(self.swap(self.registers.b()));
                    },
                    0x31 => {
                        // SWAP C
                        self.registers.setC(self.swap(self.registers.c()));
                    },
                    0x32 => {
                        // SWAP D
                        self.registers.setD(self.swap(self.registers.d()));
                    },
                    0x33 => {
                        // SWAP E
                        self.registers.setE(self.swap(self.registers.e()));
                    },
                    0x34 => {
                        // SWAP H
                        self.registers.setH(self.swap(self.registers.h()));
                    },
                    0x35 => {
                        // SWAP L
                        self.registers.setL(self.swap(self.registers.l()));
                    },
                    0x36 => {
                        // SWAP (HL)
                        self.memory.set(
                            self.registers.hl,
                            self.swap(self.memory.get(self.registers.hl)));
                    },
                    0x37 => {
                        // SWAP A
                        self.registers.setA(self.swap(self.registers.a()));
                    },
                    0x38 => {
                        // SRL B
                        self.registers.setB(self.srl(self.registers.b()));
                    },
                    0x39 => {
                        // SRL C
                        self.registers.setC(self.srl(self.registers.c()));
                    },
                    0x3A => {
                        // SRL D
                        self.registers.setD(self.srl(self.registers.d()));
                    },
                    0x3B => {
                        // SRL E
                        self.registers.setE(self.srl(self.registers.e()));
                    },
                    0x3C => {
                        // SRL H
                        self.registers.setH(self.srl(self.registers.h()));
                    },
                    0x3D => {
                        // SRL L
                        self.registers.setL(self.srl(self.registers.l()));
                    },
                    0x3E => {
                        // SRL (HL)
                        self.memory.set(self.registers.hl,
                                        self.srl(self.memory.get(self.registers.hl)));
                    },
                    0x3F => {
                        // SRL A
                        self.registers.setA(self.srl(self.registers.a()));
                    },
                    0x40 => {
                        // BIT n,B
                        self.testBit(self.registers.b(), try self.stream.readByte());
                    },
                    0x41 => {
                        // BIT n,C
                        self.testBit(self.registers.c(), try self.stream.readByte());
                    },
                    0x42 => {
                        // BIT n,D
                        self.testBit(self.registers.d(), try self.stream.readByte());
                    },
                    0x43 => {
                        // BIT n,E
                        self.testBit(self.registers.e(), try self.stream.readByte());
                    },
                    0x44 => {
                        // BIT n,H
                        self.testBit(self.registers.h(), try self.stream.readByte());
                    },
                    0x45 => {
                        // BIT n,L
                        self.testBit(self.registers.l(), try self.stream.readByte());
                    },
                    0x46 => {
                        // BIT n,(HL)
                        self.testBit(self.memory.get(self.registers.hl), try self.stream.readByte());
                    },
                    0x47 => {
                        // BIT n,A
                        self.testBit(self.registers.a(), try self.stream.readByte());
                    },
                    0x80 => {
                        // RES n,B
                        self.registers.setB(self.registers.b() & ~(u8(1) << @truncate(u3, try self.stream.readByte())));
                    },
                    0x81 => {
                        // RES n,C
                        self.registers.setC(self.registers.c() & ~(u8(1) << @truncate(u3, try self.stream.readByte())));
                    },
                    0x82 => {
                        // RES n,D
                        self.registers.setD(self.registers.d() & ~(u8(1) << @truncate(u3, try self.stream.readByte())));
                    },
                    0x83 => {
                        // RES n,E
                        self.registers.setE(self.registers.e() & ~(u8(1) << @truncate(u3, try self.stream.readByte())));
                    },
                    0x84 => {
                        // RES n,H
                        self.registers.setH(self.registers.h() & ~(u8(1) << @truncate(u3, try self.stream.readByte())));
                    },
                    0x85 => {
                        // RES n,L
                        self.registers.setL(self.registers.l() & ~(u8(1) << @truncate(u3, try self.stream.readByte())));
                    },
                    0x86 => {
                        // RES n,(HL)
                        self.memory.set(self.registers.hl,
                                        self.memory.get(self.registers.hl) & ~(u8(1) << @truncate(u3, try self.stream.readByte())));
                    },
                    0x87 => {
                        // RES n,A
                        self.registers.setA(self.registers.a() & ~(u8(1) << @truncate(u3, try self.stream.readByte())));
                    },
                    0xC0 => {
                        // SET n,B
                        self.registers.setB(self.registers.b() | (u8(1) << @truncate(u3, try self.stream.readByte())));
                    },
                    0xC1 => {
                        // SET n,C
                        self.registers.setC(self.registers.c() | (u8(1) << @truncate(u3, try self.stream.readByte())));
                    },
                    0xC2 => {
                        // SET n,D
                        self.registers.setD(self.registers.d() | (u8(1) << @truncate(u3, try self.stream.readByte())));
                    },
                    0xC3 => {
                        // SET n,E
                        self.registers.setE(self.registers.e() | (u8(1) << @truncate(u3, try self.stream.readByte())));
                    },
                    0xC4 => {
                        // SET n,H
                        self.registers.setH(self.registers.h() | (u8(1) << @truncate(u3, try self.stream.readByte())));
                    },
                    0xC5 => {
                        // SET n,L
                        self.registers.setL(self.registers.l() | (u8(1) << @truncate(u3, try self.stream.readByte())));
                    },
                    0xC6 => {
                        // SET n,(HL)
                        self.memory.set(self.registers.hl,
                                        self.memory.get(self.registers.hl) | (u8(1) << @truncate(u3, try self.stream.readByte())));
                    },
                    0xC7 => {
                        // SET n,A
                        self.registers.setA(self.registers.a() | (u8(1) << @truncate(u3, try self.stream.readByte())));
                    },
                    else => {
                        return ErrorSet.InvalidInstruction;
                    },
                }
            },
            0xCE => {
                // ADC A,n
                self.registers.setA(self.add(u8,
                    self.registers.a(),
                    (try self.stream.readByte()) + @boolToInt(self.registers.carryFlag())));
            },
            0xD1 => {
                // POP DE
                self.registers.de = self.pop(u16);
            },
            0xD5 => {
                // PUSH DE
                self.push(self.registers.de);
            },
            0xD6 => {
                // SUB A,n
                self.registers.setA(self.sub(self.registers.a(), try self.stream.readByte()));
            },
            0xDE => {
                // SBC A,n
                self.registers.setA(self.sub(
                    self.registers.a(),
                    (try self.stream.readByte()) + @boolToInt(self.registers.carryFlag())));
            },
            0xE0 => {
                // LDH ($FF00+n),A
                self.memory.set(0xFF00 | u16(try self.stream.readByte()),
                                self.registers.a());
            },
            0xE1 => {
                // POP HL
                self.registers.hl = self.pop(u16);
            },
            0xE2 => {
                // LD ($FF00+C),A
                self.memory.set(0xFF00 | u16(self.registers.c()), self.registers.a());
            },
            0xE5 => {
                // PUSH HL
                self.push(self.registers.hl);
            },
            0xE6 => {
                // AND A,n
                self.registers.setA(self.bitwiseAnd(
                    self.registers.a(),
                    try self.stream.readByte()));
            },
            0xE8 => {
                // ADD SP,n
                self.registers.sp = self.add(u16,
                    self.registers.sp,
                    try self.stream.readByte());
            },
            0xEA => {
                // LD (nn),A
                self.memory.set(try self.stream.readIntLe(u16), self.registers.a());
            },
            0xEE => {
                // XOR A,n
                self.registers.setA(self.bitwiseXor(
                    self.registers.a(),
                    try self.stream.readByte()));
            },
            0xF0 => {
                // LDH A,($FF00+n)
                self.registers.setA(self.memory.get(0xFF00 | u16(try self.stream.readByte())));
            },
            0xF1 => {
                // POP AF
                self.registers.af = self.pop(u16);
            },
            0xF2 => {
                // LD A,($FF00+C)
                self.registers.setA(self.memory.get(u16(0xFF00) | self.registers.c()));
            },
            0xF3 => {
                // DI
                return Mode.DisableInterrupts;
            },
            0xF5 => {
                // PUSH AF
                self.push(self.registers.af);
            },
            0xF6 => {
                // OR A,n
                self.registers.setA(self.bitwiseOr(
                    self.registers.a(),
                    try self.stream.readByte()));
            },
            0xF8 => {
                // LDHL SP,n
                const n = try self.stream.readByte();
                self.registers.hl = self.add(u8, @truncate(u8, self.registers.sp), n);
            },
            0xF9 => {
                // LD SP,HL
                self.registers.sp = self.registers.hl;
            },
            0xFA => {
                // LD A,(HL)
                const address = try self.stream.readIntLe(u16);
                self.registers.setA(self.memory.get(address));
            },
            0xFB => {
                return Mode.EnableInterrupts;
            },
            0xFE => {
                // CP A,n
                _ = self.sub(self.registers.a(), try self.stream.readByte());
            },
            else => {
                return ErrorSet.InvalidInstruction;
            },
        }
        return Mode.Default;
    }
};

test "CPU" {
    var cpu = try CPU.init(std.debug.global_allocator);
    cpu.registers.pc = 0;
    cpu.registers.hl = 0x55;
    cpu.memory.set(0x0, 0x7E);
    cpu.memory.set(0x55, 0x20);
    _ = try cpu.execute();
    std.debug.assert(cpu.registers.a() == 0x20);
    std.debug.assert(cpu.registers.pc == 1);
    std.debug.assert(cpu.add(u8, 0x4, 0x6) == 0xA);
    std.debug.assert(!cpu.registers.halfCarryFlag());
    std.debug.assert(cpu.add(u8, 0xA, 0x6) == 0x10);
    std.debug.assert(cpu.registers.halfCarryFlag());
    cpu.deinit();
}
