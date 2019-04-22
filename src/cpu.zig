const std = @import("std");
const builtin = @import("builtin");

const opcode = @import("opcode.zig");

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
        self.af = (u16(value) << 8) | self.f();
    }

    pub fn b(self: *const Registers) u8 {
        return @truncate(u8, self.bc >> 8);
    }

    pub fn setB(self: *Registers, value: u8) void {
        self.bc = (u16(value) << 8) | self.c();
    }

    pub fn c(self: *const Registers) u8 {
        return @truncate(u8, self.bc);
    }

    pub fn setC(self: *Registers, value: u8) void {
        self.bc = (u16(self.b()) << 8) | value;
    }

    pub fn d(self: *const Registers) u8 {
        return @truncate(u8, self.de >> 8);
    }

    pub fn setD(self: *Registers, value: u8) void {
        self.de = (u16(value) << 8) | self.e();
    }

    pub fn e(self: *const Registers) u8 {
        return @truncate(u8, self.de);
    }

    pub fn setE(self: *Registers, value: u8) void {
        self.de = (u16(self.d()) << 8) | value;
    }

    pub fn f(self: *const Registers) u8 {
        return @truncate(u8, self.af);
    }

    pub fn setF(self: *Registers, value: u8) void {
        self.af = (u16(self.a()) << 8) | value;
    }

    pub fn h(self: *const Registers) u8 {
        return @truncate(u8, self.hl >> 8);
    }

    pub fn setH(self: *Registers, value: u8) void {
        self.hl = (u16(value) << 8) | self.l();
    }

    pub fn l(self: *const Registers) u8 {
        return @truncate(u8, self.hl);
    }

    pub fn setL(self: *Registers, value: u8) void {
        self.hl = (u16(self.h()) << 8) | value;
    }

    const zero_flag_mask: u8 = 0x80;
    const subtract_flag_mask: u8 = 0x40;
    const half_carry_flag_mask: u8 = 0x20;
    const carry_flag_mask: u8 = 0x10;

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
    var registers: Registers = undefined;
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
        return self.memory[offset .. offset + len];
    }

    pub fn slice(self: *Memory, index: u16, len: usize) []u8 {
        const offset = internalIndex(index);
        return self.memory[offset .. offset + len];
    }
};

pub const CPU = struct {
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
        var len: usize = undefined;
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
        std.mem.writeIntSliceLittle(@typeOf(value), self.memory.slice(self.registers.sp, len), value);
    }

    fn pop(self: *CPU, comptime T: type) T {
        const value = std.mem.readIntSliceLittle(T, self.memory.sliceConst(self.registers.sp, @sizeOf(T)));
        self.registers.sp +%= @sizeOf(T);
        return value;
    }

    fn add(self: *CPU, comptime T: type, a: T, b: T) T {
        const B = @IntType(false, @sizeOf(T) * 16);
        const high_test_bit_num_trailing_zeros = @sizeOf(T) * 8;
        const high_test_bit: B = 1 << high_test_bit_num_trailing_zeros;
        const high_operand_mask = high_test_bit - 1;
        const low_test_bit = high_test_bit >> 4;
        const low_operand_mask = (low_test_bit) - 1;
        const result = a + b;
        self.registers.setHalfCarryFlag((((a & low_operand_mask) + (b & low_operand_mask)) & low_test_bit) == low_test_bit);
        self.registers.setCarryFlag((((a & high_operand_mask) + (b & high_operand_mask)) & high_test_bit) == high_test_bit);
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

    fn jumpRelative(self: *CPU, n: i8) void {
        const UnsignedAddress = @typeOf(self.registers.pc);
        const SignedAddress = @IntType(true, UnsignedAddress.bit_count << 1);
        const result = @intCast(SignedAddress, self.registers.pc) + n;
        self.registers.pc = @truncate(u16, @intCast(UnsignedAddress, result));
    }

    fn call(self: *CPU, call_address: u16) void {
        const return_address: u16 = self.registers.pc + 1;
        self.push(return_address);
        self.registers.pc = call_address;
    }

    pub fn execute(self: *CPU) !Mode {
        switch (@intToEnum(opcode.Opcode, try self.stream.readByte())) {
            opcode.Opcode.NOP => {
                // NOP
            },
            opcode.Opcode.LD_BC_nn => {
                // LD BC,nn
                self.registers.bc = try self.stream.readIntLittle(u16);
            },
            opcode.Opcode.LD_BC_A => {
                // LD (BC),A
                self.memory.set(self.registers.bc, self.registers.a());
            },
            opcode.Opcode.INC_BC => {
                // INC BC
                self.registers.bc = self.add(u16, self.registers.bc, 1);
            },
            opcode.Opcode.INC_B => {
                // INC B
                self.registers.setB(self.add(u8, self.registers.b(), 1));
            },
            opcode.Opcode.DEC_B => {
                // DEC B
                self.registers.setB(self.sub(self.registers.b(), 1));
            },
            opcode.Opcode.LD_B_n => {
                // LD B,n
                self.registers.setB(try self.stream.readByte());
            },
            opcode.Opcode.RLCA => {
                // RLCA
                self.registers.setA(self.rlc(self.registers.a()));
            },
            opcode.Opcode.LD_nn_SP => {
                // LD (nn),SP
                const value = try self.stream.readIntLittle(u16);
                self.memory.set(value, @truncate(u8, (self.registers.sp & 0xFF00) >> 8));
                self.memory.set(value + 1, @truncate(u8, self.registers.sp));
            },
            opcode.Opcode.ADD_HL_BC => {
                // ADD HL,BC
                self.registers.hl = self.add(u16, self.registers.hl, self.registers.bc);
            },
            opcode.Opcode.LD_A_BC => {
                // LD A,(BC)
                self.registers.setA(self.memory.get(self.registers.bc));
            },
            opcode.Opcode.DEC_BC => {
                // DEC BC
                self.registers.bc -%= 1;
            },
            opcode.Opcode.INC_C => {
                // INC C
                self.registers.setC(self.add(u8, self.registers.c(), 1));
            },
            opcode.Opcode.DEC_C => {
                // DEC D
                self.registers.setD(self.sub(self.registers.d(), 1));
            },
            opcode.Opcode.LD_C_n => {
                // LD C,n
                self.registers.setC(try self.stream.readByte());
            },
            opcode.Opcode.RRCA => {
                // RRCA
                self.registers.setA(self.rrc(self.registers.a()));
            },
            opcode.Opcode.STOP_FIRST_BYTE => {
                // STOP
                switch (try self.stream.readByte()) {
                    0x00 => {
                        return Mode.Stop;
                    },
                    else => {
                        unreachable;
                    },
                }
            },
            opcode.Opcode.LD_DE_nn => {
                // LD DE,nn
                self.registers.de = try self.stream.readIntLittle(u16);
            },
            opcode.Opcode.LD_DE_A => {
                // LD (DE),A
                self.memory.set(self.registers.de, self.registers.a());
            },
            opcode.Opcode.INC_DE => {
                // INC DE
                self.registers.de = self.add(u16, self.registers.de, 1);
            },
            opcode.Opcode.INC_D => {
                // INC D
                self.registers.setD(self.add(u8, self.registers.d(), 1));
            },
            opcode.Opcode.DEC_D => {
                // DEC D
                self.registers.setD(self.sub(self.registers.d(), 1));
            },
            opcode.Opcode.LD_D_n => {
                // LD D,n
                self.registers.setD(try self.stream.readByte());
            },
            opcode.Opcode.RLA => {
                // RLA
                self.registers.setA(self.rl(self.registers.a()));
            },
            opcode.Opcode.JR_n => {
                // JR
                const n = try self.stream.readByteSigned();
                self.jumpRelative(n);
            },
            opcode.Opcode.ADD_HL_DE => {
                // ADD HL,DE
                self.registers.hl = self.add(u16, self.registers.hl, self.registers.de);
            },
            opcode.Opcode.LD_A_DE => {
                // LD A,(DE)
                self.registers.setA(self.memory.get(self.registers.de));
            },
            opcode.Opcode.DEC_DE => {
                // DEC DE
                self.registers.de -%= 1;
            },
            opcode.Opcode.INC_E => {
                // INC E
                self.registers.setE(self.add(u8, self.registers.e(), 1));
            },
            opcode.Opcode.DEC_E => {
                // DEC E
                self.registers.setE(self.sub(self.registers.e(), 1));
            },
            opcode.Opcode.LD_E_n => {
                // LD E,n
                self.registers.setE(try self.stream.readByte());
            },
            opcode.Opcode.RRA => {
                // RRA
                self.registers.setA(self.rr(self.registers.a()));
            },
            opcode.Opcode.JR_NZ_n => {
                // JR NZ,n
                const n = try self.stream.readByteSigned();
                if (!self.registers.zeroFlag()) {
                    self.jumpRelative(n);
                }
            },
            opcode.Opcode.LD_HL_nn => {
                // LD HL,nn
                self.registers.hl = try self.stream.readIntLittle(u16);
            },
            opcode.Opcode.LDI_HL_A => {
                // LDI (HL),A
                self.memory.set(self.registers.hl, self.registers.a());
                self.registers.hl +%= 1;
            },
            opcode.Opcode.INC_HL => {
                // INC HL
                self.registers.hl = self.add(u16, self.registers.hl, 1);
            },
            opcode.Opcode.INC_H => {
                // INC H
                self.registers.setH(self.add(u8, self.registers.h(), 1));
            },
            opcode.Opcode.DEC_H => {
                // DEC H
                self.registers.setH(self.sub(self.registers.h(), 1));
            },
            opcode.Opcode.LD_H_n => {
                // LD H,n
                self.registers.setH(try self.stream.readByte());
            },
            opcode.Opcode.DAA => {
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
            opcode.Opcode.JR_Z_n => {
                // JR Z,n
                const n = try self.stream.readByteSigned();
                if (self.registers.zeroFlag()) {
                    self.jumpRelative(n);
                }
            },
            opcode.Opcode.ADD_HL_HL => {
                // ADD HL,HL
                self.registers.hl = self.add(u16, self.registers.hl, self.registers.hl);
            },
            opcode.Opcode.LDI_A_HL => {
                // LDI A,(HL)
                self.registers.setA(self.memory.get(self.registers.hl));
                self.registers.hl +%= 1;
            },
            opcode.Opcode.DEC_HL => {
                // DEC HL
                self.registers.hl -%= 1;
            },
            opcode.Opcode.INC_L => {
                // INC L
                self.registers.setL(self.add(u8, self.registers.l(), 1));
            },
            opcode.Opcode.DEC_L => {
                // DEC L
                self.registers.setL(self.sub(self.registers.l(), 1));
            },
            opcode.Opcode.LD_L_n => {
                // LD L,n
                self.registers.setL(try self.stream.readByte());
            },
            opcode.Opcode.CPL => {
                // CPL
                self.registers.setA(~self.registers.a());
                self.registers.setSubtractFlag(true);
                self.registers.setHalfCarryFlag(true);
            },
            opcode.Opcode.JR_NC_n => {
                // JR NC,n
                const n = try self.stream.readByteSigned();
                if (!self.registers.carryFlag()) {
                    self.jumpRelative(n);
                }
            },
            opcode.Opcode.LD_SP_nn => {
                // LD SP,nn
                self.registers.sp = try self.stream.readIntLittle(u16);
            },
            opcode.Opcode.LDD_HL_A => {
                // LDD (HL),A
                self.memory.set(self.registers.hl, self.registers.a());
                self.registers.hl -%= 1;
            },
            opcode.Opcode.INC_SP => {
                // INC SP
                self.registers.sp = self.add(u16, self.registers.sp, 1);
            },
            opcode.Opcode.INC_mem_HL => {
                // INC (HL)
                self.memory.set(self.registers.hl, self.add(u8, self.memory.get(self.registers.hl), 1));
            },
            opcode.Opcode.DEC_mem_HL => {
                // DEC (HL)
                self.memory.set(self.registers.hl, self.sub(self.memory.get(self.registers.hl), 1));
            },
            opcode.Opcode.LD_HL_n => {
                // LD (HL),n
                self.memory.set(self.registers.hl, try self.stream.readByte());
            },
            opcode.Opcode.SCF => {
                // SCF
                self.registers.setSubtractFlag(false);
                self.registers.setHalfCarryFlag(false);
                self.registers.setCarryFlag(true);
            },
            opcode.Opcode.JR_C_n => {
                // JR C,n
                const n = try self.stream.readByteSigned();
                if (self.registers.carryFlag()) {
                    self.jumpRelative(n);
                }
            },
            opcode.Opcode.ADD_HL_SP => {
                // ADD HL,SP
                self.registers.hl = self.add(u16, self.registers.hl, self.registers.sp);
            },
            opcode.Opcode.LDD_A_HL => {
                // LDD A,(HL)
                self.registers.setA(self.memory.get(self.registers.hl));
                self.registers.hl -%= 1;
            },
            opcode.Opcode.DEC_SP => {
                // DEC HL
                self.registers.hl -%= 1;
            },
            opcode.Opcode.INC_A => {
                // INC A
                self.registers.setA(self.add(u8, self.registers.a(), 1));
            },
            opcode.Opcode.DEC_A => {
                // DEC A
                self.registers.setA(self.sub(self.registers.a(), 1));
            },
            opcode.Opcode.LD_A_n => {
                // LD A,n
                self.registers.setA(try self.stream.readByte());
            },
            opcode.Opcode.CCF => {
                // CCF
                self.registers.setSubtractFlag(false);
                self.registers.setHalfCarryFlag(false);
                self.registers.setCarryFlag(!self.registers.carryFlag());
            },
            opcode.Opcode.LD_B_B => {
                // LD B,B
                self.registers.setB(self.registers.b());
            },
            opcode.Opcode.LD_B_C => {
                // LD B,C
                self.registers.setB(self.registers.c());
            },
            opcode.Opcode.LD_B_D => {
                // LD B,D
                self.registers.setB(self.registers.d());
            },
            opcode.Opcode.LD_B_E => {
                // LD B,E
                self.registers.setB(self.registers.e());
            },
            opcode.Opcode.LD_B_H => {
                // LD B,H
                self.registers.setB(self.registers.h());
            },
            opcode.Opcode.LD_B_L => {
                // LD B,L
                self.registers.setB(self.registers.l());
            },
            opcode.Opcode.LD_B_HL => {
                // LD B,(HL)
                self.registers.setB(self.memory.get(self.registers.hl));
            },
            opcode.Opcode.LD_B_A => {
                // LD B,A
                self.registers.setB(self.registers.a());
            },
            opcode.Opcode.LD_C_B => {
                // LD C,B
                self.registers.setC(self.registers.b());
            },
            opcode.Opcode.LD_C_C => {
                // LD C,C
                self.registers.setC(self.registers.c());
            },
            opcode.Opcode.LD_C_D => {
                // LD C,D
                self.registers.setC(self.registers.d());
            },
            opcode.Opcode.LD_C_E => {
                // LD C,E
                self.registers.setC(self.registers.e());
            },
            opcode.Opcode.LD_C_H => {
                // LD C,H
                self.registers.setC(self.registers.h());
            },
            opcode.Opcode.LD_C_L => {
                // LD C,L
                self.registers.setC(self.registers.l());
            },
            opcode.Opcode.LD_C_HL => {
                // LD C,(HL)
                self.registers.setC(self.memory.get(self.registers.hl));
            },
            opcode.Opcode.LD_C_A => {
                // LD C,A
                self.registers.setC(self.registers.a());
            },
            opcode.Opcode.LD_D_B => {
                // LD D,B
                self.registers.setD(self.registers.b());
            },
            opcode.Opcode.LD_D_C => {
                // LD D,C
                self.registers.setD(self.registers.c());
            },
            opcode.Opcode.LD_D_D => {
                // LD D,D
                self.registers.setD(self.registers.d());
            },
            opcode.Opcode.LD_D_E => {
                // LD D,E
                self.registers.setD(self.registers.e());
            },
            opcode.Opcode.LD_D_H => {
                // LD D,H
                self.registers.setD(self.registers.h());
            },
            opcode.Opcode.LD_D_L => {
                // LD D,L
                self.registers.setD(self.registers.l());
            },
            opcode.Opcode.LD_D_HL => {
                // LD D,(HL)
                self.registers.setD(self.memory.get(self.registers.hl));
            },
            opcode.Opcode.LD_D_A => {
                // LD D,A
                self.registers.setD(self.registers.a());
            },
            opcode.Opcode.LD_E_B => {
                // LD E,B
                self.registers.setE(self.registers.b());
            },
            opcode.Opcode.LD_E_C => {
                // LD E,C
                self.registers.setE(self.registers.c());
            },
            opcode.Opcode.LD_E_D => {
                // LD E,D
                self.registers.setE(self.registers.d());
            },
            opcode.Opcode.LD_E_E => {
                // LD E,E
                self.registers.setE(self.registers.e());
            },
            opcode.Opcode.LD_E_H => {
                // LD E,H
                self.registers.setE(self.registers.h());
            },
            opcode.Opcode.LD_E_L => {
                // LD E,L
                self.registers.setE(self.registers.l());
            },
            opcode.Opcode.LD_E_HL => {
                // LD E,(HL)
                self.registers.setE(self.memory.get(self.registers.hl));
            },
            opcode.Opcode.LD_E_A => {
                // LD E,A
                self.registers.setE(self.registers.a());
            },
            opcode.Opcode.LD_H_B => {
                // LD H,B
                self.registers.setH(self.registers.b());
            },
            opcode.Opcode.LD_H_C => {
                // LD H,C
                self.registers.setH(self.registers.c());
            },
            opcode.Opcode.LD_H_D => {
                // LD H,D
                self.registers.setH(self.registers.d());
            },
            opcode.Opcode.LD_H_E => {
                // LD H,E
                self.registers.setH(self.registers.e());
            },
            opcode.Opcode.LD_H_H => {
                // LD H,H
                self.registers.setH(self.registers.h());
            },
            opcode.Opcode.LD_H_L => {
                // LD H,L
                self.registers.setH(self.registers.l());
            },
            opcode.Opcode.LD_H_HL => {
                // LD H,(HL)
                self.registers.setH(self.memory.get(self.registers.hl));
            },
            opcode.Opcode.LD_H_A => {
                // LD H,A
                self.registers.setH(self.registers.a());
            },
            opcode.Opcode.LD_L_B => {
                // LD L,B
                self.registers.setL(self.registers.b());
            },
            opcode.Opcode.LD_L_C => {
                // LD L,C
                self.registers.setL(self.registers.c());
            },
            opcode.Opcode.LD_L_D => {
                // LD L,D
                self.registers.setL(self.registers.d());
            },
            opcode.Opcode.LD_L_E => {
                // LD L,E
                self.registers.setL(self.registers.e());
            },
            opcode.Opcode.LD_L_H => {
                // LD L,H
                self.registers.setL(self.registers.h());
            },
            opcode.Opcode.LD_L_L => {
                // LD L,L
                self.registers.setL(self.registers.l());
            },
            opcode.Opcode.LD_L_HL => {
                // LD L,(HL)
                self.registers.setL(self.memory.get(self.registers.hl));
            },
            opcode.Opcode.LD_L_A => {
                // LD L,A
                self.registers.setL(self.registers.a());
            },
            opcode.Opcode.LD_HL_B => {
                // LD (HL),B
                self.memory.set(self.registers.hl, self.registers.b());
            },
            opcode.Opcode.LD_HL_C => {
                // LD (HL),C
                self.memory.set(self.registers.hl, self.registers.c());
            },
            opcode.Opcode.LD_HL_D => {
                // LD (HL),D
                self.memory.set(self.registers.hl, self.registers.d());
            },
            opcode.Opcode.LD_HL_E => {
                // LD (HL),E
                self.memory.set(self.registers.hl, self.registers.e());
            },
            opcode.Opcode.LD_HL_H => {
                // LD (HL),H
                self.memory.set(self.registers.hl, self.registers.h());
            },
            opcode.Opcode.LD_HL_L => {
                // LD (HL),L
                self.memory.set(self.registers.hl, self.registers.l());
            },
            opcode.Opcode.HALT => {
                // HALT
                return Mode.Halt;
            },
            opcode.Opcode.LD_HL_A => {
                // LD (HL),A
                self.memory.set(self.registers.hl, self.registers.a());
            },
            opcode.Opcode.LD_A_B => {
                // LD A,B
                self.registers.setA(self.registers.b());
            },
            opcode.Opcode.LD_A_C => {
                // LD A,C
                self.registers.setA(self.registers.c());
            },
            opcode.Opcode.LD_A_D => {
                // LD A,D
                self.registers.setA(self.registers.d());
            },
            opcode.Opcode.LD_A_E => {
                // LD A,E
                self.registers.setA(self.registers.e());
            },
            opcode.Opcode.LD_A_H => {
                // LD A,H
                self.registers.setA(self.registers.h());
            },
            opcode.Opcode.LD_A_L => {
                // LD A,L
                self.registers.setA(self.registers.l());
            },
            opcode.Opcode.LD_A_HL => {
                // LD A,(HL)
                self.registers.setA(self.memory.get(self.registers.hl));
            },
            opcode.Opcode.ADD_A_B => {
                // ADD A,B
                self.registers.setA(self.add(u8, self.registers.a(), self.registers.b()));
            },
            opcode.Opcode.ADD_A_C => {
                // ADD A,C
                self.registers.setA(self.add(u8, self.registers.a(), self.registers.c()));
            },
            opcode.Opcode.ADD_A_D => {
                // ADD A,D
                self.registers.setA(self.add(u8, self.registers.a(), self.registers.d()));
            },
            opcode.Opcode.ADD_A_E => {
                // ADD A,E
                self.registers.setA(self.add(u8, self.registers.a(), self.registers.e()));
            },
            opcode.Opcode.ADD_A_H => {
                // ADD A,H
                self.registers.setA(self.add(u8, self.registers.a(), self.registers.h()));
            },
            opcode.Opcode.ADD_A_L => {
                // ADD A,L
                self.registers.setA(self.add(u8, self.registers.a(), self.registers.l()));
            },
            opcode.Opcode.ADD_A_HL => {
                // ADD A,(HL)
                self.registers.setA(self.add(u8, self.registers.a(), self.memory.get(self.registers.hl)));
            },
            opcode.Opcode.ADD_A_A => {
                // ADD A,A
                self.registers.setA(self.add(u8, self.registers.a(), self.registers.a()));
            },
            opcode.Opcode.ADC_A_B => {
                // ADC A,B
                self.registers.setA(self.add(u8, self.registers.a(), self.registers.b() + @boolToInt(self.registers.carryFlag())));
            },
            opcode.Opcode.ADC_A_C => {
                // ADC A,C
                self.registers.setA(self.add(u8, self.registers.a(), self.registers.c() + @boolToInt(self.registers.carryFlag())));
            },
            opcode.Opcode.ADC_A_D => {
                // ADC A,D
                self.registers.setA(self.add(u8, self.registers.a(), self.registers.d() + @boolToInt(self.registers.carryFlag())));
            },
            opcode.Opcode.ADC_A_E => {
                // ADC A,E
                self.registers.setA(self.add(u8, self.registers.a(), self.registers.e() + @boolToInt(self.registers.carryFlag())));
            },
            opcode.Opcode.ADC_A_H => {
                // ADC A,H
                self.registers.setA(self.add(u8, self.registers.a(), self.registers.h() + @boolToInt(self.registers.carryFlag())));
            },
            opcode.Opcode.ADC_A_L => {
                // ADC A,L
                self.registers.setA(self.add(u8, self.registers.a(), self.registers.l() + @boolToInt(self.registers.carryFlag())));
            },
            opcode.Opcode.ADC_A_HL => {
                // ADC A,(HL)
                self.registers.setA(self.add(u8, self.registers.a(), self.memory.get(self.registers.hl) + @boolToInt(self.registers.carryFlag())));
            },
            opcode.Opcode.ADC_A_A => {
                // ADC A,A
                self.registers.setA(self.add(u8, self.registers.a(), self.registers.a() + @boolToInt(self.registers.carryFlag())));
            },
            opcode.Opcode.SUB_A_B => {
                // SUB A,B
                self.registers.setA(self.sub(self.registers.a(), self.registers.b()));
            },
            opcode.Opcode.SUB_A_C => {
                // SUB A,C
                self.registers.setA(self.sub(self.registers.a(), self.registers.c()));
            },
            opcode.Opcode.SUB_A_D => {
                // SUB A,D
                self.registers.setA(self.sub(self.registers.a(), self.registers.d()));
            },
            opcode.Opcode.SUB_A_E => {
                // SUB A,E
                self.registers.setA(self.sub(self.registers.a(), self.registers.e()));
            },
            opcode.Opcode.SUB_A_H => {
                // SUB A,H
                self.registers.setA(self.sub(self.registers.a(), self.registers.h()));
            },
            opcode.Opcode.SUB_A_L => {
                // SUB A,L
                self.registers.setA(self.sub(self.registers.a(), self.registers.l()));
            },
            opcode.Opcode.SUB_A_HL => {
                // SUB A,(HL)
                self.registers.setA(self.sub(self.registers.a(), self.memory.get(self.registers.hl)));
            },
            opcode.Opcode.SUB_A_A => {
                // SUB A,A
                self.registers.setA(self.sub(self.registers.a(), self.registers.a()));
            },
            opcode.Opcode.SBC_A_B => {
                // SBC A,B
                self.registers.setA(self.sub(self.registers.a(), self.registers.b() + @boolToInt(self.registers.carryFlag())));
            },
            opcode.Opcode.SBC_A_C => {
                // SBC A,C
                self.registers.setA(self.sub(self.registers.a(), self.registers.c() + @boolToInt(self.registers.carryFlag())));
            },
            opcode.Opcode.SBC_A_D => {
                // SBC A,D
                self.registers.setA(self.sub(self.registers.a(), self.registers.d() + @boolToInt(self.registers.carryFlag())));
            },
            opcode.Opcode.SBC_A_E => {
                // SBC A,E
                self.registers.setA(self.sub(self.registers.a(), self.registers.e() + @boolToInt(self.registers.carryFlag())));
            },
            opcode.Opcode.SBC_A_H => {
                // SBC A,H
                self.registers.setA(self.sub(self.registers.a(), self.registers.h() + @boolToInt(self.registers.carryFlag())));
            },
            opcode.Opcode.SBC_A_L => {
                // SBC A,L
                self.registers.setA(self.sub(self.registers.a(), self.registers.l() + @boolToInt(self.registers.carryFlag())));
            },
            opcode.Opcode.SBC_A_HL => {
                // SBC A,(HL)
                self.registers.setA(self.sub(self.registers.a(), self.memory.get(self.registers.hl) + @boolToInt(self.registers.carryFlag())));
            },
            opcode.Opcode.SBC_A_A => {
                // SBC A,A
                self.registers.setA(self.sub(self.registers.a(), self.registers.a() + @boolToInt(self.registers.carryFlag())));
            },
            opcode.Opcode.AND_A_B => {
                // AND A,B
                self.registers.setA(self.bitwiseAnd(self.registers.a(), self.registers.b()));
            },
            opcode.Opcode.AND_A_C => {
                // AND A,C
                self.registers.setA(self.bitwiseAnd(self.registers.a(), self.registers.c()));
            },
            opcode.Opcode.AND_A_D => {
                // AND A,D
                self.registers.setA(self.bitwiseAnd(self.registers.a(), self.registers.d()));
            },
            opcode.Opcode.AND_A_E => {
                // AND A,E
                self.registers.setA(self.bitwiseAnd(self.registers.a(), self.registers.e()));
            },
            opcode.Opcode.AND_A_H => {
                // AND A,H
                self.registers.setA(self.bitwiseAnd(self.registers.a(), self.registers.h()));
            },
            opcode.Opcode.AND_A_L => {
                // AND A,L
                self.registers.setA(self.bitwiseAnd(self.registers.a(), self.registers.l()));
            },
            opcode.Opcode.AND_A_HL => {
                // AND A,(HL)
                self.registers.setA(self.bitwiseAnd(self.registers.a(), self.memory.get(self.registers.hl)));
            },
            opcode.Opcode.AND_A_A => {
                // AND A,A
                self.registers.setA(self.bitwiseAnd(self.registers.a(), self.registers.a()));
            },
            opcode.Opcode.XOR_A_B => {
                // XOR A,B
                self.registers.setA(self.bitwiseXor(self.registers.a(), self.registers.b()));
            },
            opcode.Opcode.XOR_A_C => {
                // XOR A,C
                self.registers.setA(self.bitwiseXor(self.registers.a(), self.registers.c()));
            },
            opcode.Opcode.XOR_A_D => {
                // XOR A,D
                self.registers.setA(self.bitwiseXor(self.registers.a(), self.registers.d()));
            },
            opcode.Opcode.XOR_A_E => {
                // XOR A,E
                self.registers.setA(self.bitwiseXor(self.registers.a(), self.registers.e()));
            },
            opcode.Opcode.XOR_A_H => {
                // XOR A,H
                self.registers.setA(self.bitwiseXor(self.registers.a(), self.registers.h()));
            },
            opcode.Opcode.XOR_A_L => {
                // XOR A,L
                self.registers.setA(self.bitwiseXor(self.registers.a(), self.registers.l()));
            },
            opcode.Opcode.XOR_A_HL => {
                // XOR A,(HL)
                self.registers.setA(self.bitwiseXor(self.registers.a(), self.memory.get(self.registers.hl)));
            },
            opcode.Opcode.XOR_A_A => {
                // XOR A,A
                self.registers.setA(self.bitwiseXor(self.registers.a(), self.registers.a()));
            },
            opcode.Opcode.OR_A_B => {
                // OR A,B
                self.registers.setA(self.bitwiseOr(self.registers.a(), self.registers.b()));
            },
            opcode.Opcode.OR_A_C => {
                // OR A,C
                self.registers.setA(self.bitwiseOr(self.registers.a(), self.registers.c()));
            },
            opcode.Opcode.OR_A_D => {
                // OR A,D
                self.registers.setA(self.bitwiseOr(self.registers.a(), self.registers.d()));
            },
            opcode.Opcode.OR_A_E => {
                // OR A,E
                self.registers.setA(self.bitwiseOr(self.registers.a(), self.registers.e()));
            },
            opcode.Opcode.OR_A_H => {
                // OR A,H
                self.registers.setA(self.bitwiseOr(self.registers.a(), self.registers.h()));
            },
            opcode.Opcode.OR_A_L => {
                // OR A,L
                self.registers.setA(self.bitwiseOr(self.registers.a(), self.registers.l()));
            },
            opcode.Opcode.OR_A_HL => {
                // OR A,(HL)
                self.registers.setA(self.bitwiseOr(self.registers.a(), self.memory.get(self.registers.hl)));
            },
            opcode.Opcode.OR_A_A => {
                // OR A,A
                self.registers.setA(self.bitwiseOr(self.registers.a(), self.registers.a()));
            },
            opcode.Opcode.CP_A_B => {
                // CP A,B
                _ = self.sub(self.registers.a(), self.registers.b());
            },
            opcode.Opcode.CP_A_C => {
                // CP A,C
                _ = self.sub(self.registers.a(), self.registers.c());
            },
            opcode.Opcode.CP_A_D => {
                // CP A,D
                _ = self.sub(self.registers.a(), self.registers.d());
            },
            opcode.Opcode.CP_A_E => {
                // CP A,E
                _ = self.sub(self.registers.a(), self.registers.e());
            },
            opcode.Opcode.CP_A_H => {
                // CP A,H
                _ = self.sub(self.registers.a(), self.registers.h());
            },
            opcode.Opcode.CP_A_L => {
                // CP A,B
                _ = self.sub(self.registers.a(), self.registers.l());
            },
            opcode.Opcode.CP_A_HL => {
                // CP A,B
                _ = self.sub(self.registers.a(), self.memory.get(self.registers.hl));
            },
            opcode.Opcode.CP_A_A => {
                // CP A,A
                _ = self.sub(self.registers.a(), self.registers.a());
            },
            opcode.Opcode.RET_NZ => {
                // RET NZ
                if (!self.registers.zeroFlag()) {
                    self.registers.pc = self.pop(u16);
                }
            },
            opcode.Opcode.POP_BC => {
                // POP BC
                self.registers.bc = self.pop(u16);
            },
            opcode.Opcode.JP_NZ_nn => {
                // JP NZ,nn
                const address = try self.stream.readIntLittle(u16);
                if (!self.registers.zeroFlag()) {
                    self.registers.pc = address;
                }
            },
            opcode.Opcode.JP_nn => {
                // JP
                self.registers.pc = try self.stream.readIntLittle(u16);
            },
            opcode.Opcode.CALL_NZ_nn => {
                // CALL NZ,nn
                if (!self.registers.zeroFlag()) {
                    self.call(try self.stream.readIntLittle(u16));
                }
            },
            opcode.Opcode.PUSH_BC => {
                // PUSH BC
                self.push(self.registers.bc);
            },
            opcode.Opcode.ADC_A_n => {
                // ADD A,n
                self.registers.setA(self.add(u8, self.registers.a(), try self.stream.readByte()));
            },
            opcode.Opcode.RST_00, opcode.Opcode.RST_08, opcode.Opcode.RST_10, opcode.Opcode.RST_18, opcode.Opcode.RST_20, opcode.Opcode.RST_28, opcode.Opcode.RST_30, opcode.Opcode.RST_38 => |op| {
                // RST n
                self.registers.pc = switch (op) {
                    opcode.Opcode.RST_00 => u16(0x00),
                    opcode.Opcode.RST_08 => u16(0x08),
                    opcode.Opcode.RST_10 => u16(0x10),
                    opcode.Opcode.RST_18 => u16(0x18),
                    opcode.Opcode.RST_20 => u16(0x20),
                    opcode.Opcode.RST_28 => u16(0x28),
                    opcode.Opcode.RST_30 => u16(0x30),
                    opcode.Opcode.RST_38 => u16(0x38),
                    else => unreachable,
                };
            },
            opcode.Opcode.RET_Z => {
                // RET Z
                if (self.registers.zeroFlag()) {
                    self.registers.pc = self.pop(u16);
                }
            },
            opcode.Opcode.RET => {
                // RET
                self.registers.pc = self.pop(u16);
            },
            opcode.Opcode.JP_Z_nn => {
                // JP Z,nn
                const address = try self.stream.readIntLittle(u16);
                if (self.registers.zeroFlag()) {
                    self.registers.pc = address;
                }
            },
            opcode.Opcode.MISC => {
                switch (@intToEnum(opcode.MiscOpcode, try self.stream.readByte())) {
                    opcode.MiscOpcode.RLC_B => {
                        // RLC B
                        self.registers.setB(self.rlc(self.registers.b()));
                    },
                    opcode.MiscOpcode.RLC_C => {
                        // RLC C
                        self.registers.setC(self.rlc(self.registers.c()));
                    },
                    opcode.MiscOpcode.RLC_D => {
                        // RLC D
                        self.registers.setD(self.rlc(self.registers.d()));
                    },
                    opcode.MiscOpcode.RLC_E => {
                        // RLC E
                        self.registers.setE(self.rlc(self.registers.e()));
                    },
                    opcode.MiscOpcode.RLC_H => {
                        // RLC H
                        self.registers.setH(self.rlc(self.registers.h()));
                    },
                    opcode.MiscOpcode.RLC_L => {
                        // RLC L
                        self.registers.setL(self.rlc(self.registers.l()));
                    },
                    opcode.MiscOpcode.RLC_HL => {
                        // RLC (HL)
                        self.memory.set(self.registers.hl, self.rlc(self.memory.get(self.registers.hl)));
                    },
                    opcode.MiscOpcode.RLC_A => {
                        // RLC A
                        self.registers.setA(self.rlc(self.registers.a()));
                    },
                    opcode.MiscOpcode.RRC_B => {
                        // RRC B
                        self.registers.setB(self.rrc(self.registers.b()));
                    },
                    opcode.MiscOpcode.RRC_C => {
                        // RRC C
                        self.registers.setC(self.rrc(self.registers.c()));
                    },
                    opcode.MiscOpcode.RRC_D => {
                        // RRC D
                        self.registers.setD(self.rrc(self.registers.d()));
                    },
                    opcode.MiscOpcode.RRC_E => {
                        // RRC E
                        self.registers.setE(self.rrc(self.registers.e()));
                    },
                    opcode.MiscOpcode.RRC_H => {
                        // RRC H
                        self.registers.setH(self.rrc(self.registers.h()));
                    },
                    opcode.MiscOpcode.RRC_L => {
                        // RRC L
                        self.registers.setL(self.rrc(self.registers.l()));
                    },
                    opcode.MiscOpcode.RRC_HL => {
                        // RRC (HL)
                        self.memory.set(self.registers.hl, self.rrc(self.memory.get(self.registers.hl)));
                    },
                    opcode.MiscOpcode.RRC_A => {
                        // RRC A
                        self.registers.setA(self.rrc(self.registers.a()));
                    },
                    opcode.MiscOpcode.RL_B => {
                        // RL B
                        self.registers.setB(self.rl(self.registers.b()));
                    },
                    opcode.MiscOpcode.RL_C => {
                        // RL C
                        self.registers.setC(self.rl(self.registers.c()));
                    },
                    opcode.MiscOpcode.RL_D => {
                        // RL D
                        self.registers.setD(self.rl(self.registers.d()));
                    },
                    opcode.MiscOpcode.RL_E => {
                        // RL E
                        self.registers.setE(self.rl(self.registers.e()));
                    },
                    opcode.MiscOpcode.RL_H => {
                        // RL H
                        self.registers.setH(self.rl(self.registers.h()));
                    },
                    opcode.MiscOpcode.RL_L => {
                        // RL L
                        self.registers.setL(self.rl(self.registers.l()));
                    },
                    opcode.MiscOpcode.RL_HL => {
                        // RL (HL)
                        self.memory.set(self.registers.hl, self.rl(self.memory.get(self.registers.hl)));
                    },
                    opcode.MiscOpcode.RL_A => {
                        // RL A
                        self.registers.setA(self.rl(self.registers.a()));
                    },
                    opcode.MiscOpcode.RR_B => {
                        // RR B
                        self.registers.setB(self.rr(self.registers.b()));
                    },
                    opcode.MiscOpcode.RR_C => {
                        // RR C
                        self.registers.setC(self.rr(self.registers.c()));
                    },
                    opcode.MiscOpcode.RR_D => {
                        // RR D
                        self.registers.setD(self.rr(self.registers.d()));
                    },
                    opcode.MiscOpcode.RR_E => {
                        // RR E
                        self.registers.setE(self.rr(self.registers.e()));
                    },
                    opcode.MiscOpcode.RR_H => {
                        // RR H
                        self.registers.setH(self.rr(self.registers.h()));
                    },
                    opcode.MiscOpcode.RR_L => {
                        // RR L
                        self.registers.setL(self.rr(self.registers.l()));
                    },
                    opcode.MiscOpcode.RR_HL => {
                        // RR (HL)
                        self.memory.set(self.registers.hl, self.rr(self.memory.get(self.registers.hl)));
                    },
                    opcode.MiscOpcode.RR_A => {
                        // RR A
                        self.registers.setA(self.rr(self.registers.a()));
                    },
                    opcode.MiscOpcode.SLA_B => {
                        // SLA B
                        self.registers.setB(self.sla(self.registers.b()));
                    },
                    opcode.MiscOpcode.SLA_C => {
                        // SLA C
                        self.registers.setC(self.sla(self.registers.c()));
                    },
                    opcode.MiscOpcode.SLA_D => {
                        // SLA D
                        self.registers.setD(self.sla(self.registers.d()));
                    },
                    opcode.MiscOpcode.SLA_E => {
                        // SLA E
                        self.registers.setE(self.sla(self.registers.e()));
                    },
                    opcode.MiscOpcode.SLA_H => {
                        // SLA H
                        self.registers.setH(self.sla(self.registers.h()));
                    },
                    opcode.MiscOpcode.SLA_L => {
                        // SLA L
                        self.registers.setL(self.sla(self.registers.l()));
                    },
                    opcode.MiscOpcode.SLA_HL => {
                        // SLA (HL)
                        self.memory.set(self.registers.hl, self.sla(self.memory.get(self.registers.hl)));
                    },
                    opcode.MiscOpcode.SLA_A => {
                        // SLA A
                        self.registers.setA(self.sla(self.registers.a()));
                    },
                    opcode.MiscOpcode.SRA_B => {
                        // SRA B
                        self.registers.setB(self.sra(self.registers.b()));
                    },
                    opcode.MiscOpcode.SRA_C => {
                        // SRA C
                        self.registers.setC(self.sra(self.registers.c()));
                    },
                    opcode.MiscOpcode.SRA_D => {
                        // SRA D
                        self.registers.setD(self.sra(self.registers.d()));
                    },
                    opcode.MiscOpcode.SRA_E => {
                        // SRA E
                        self.registers.setE(self.sra(self.registers.e()));
                    },
                    opcode.MiscOpcode.SRA_H => {
                        // SRA H
                        self.registers.setH(self.sra(self.registers.h()));
                    },
                    opcode.MiscOpcode.SRA_L => {
                        // SRA L
                        self.registers.setL(self.sra(self.registers.l()));
                    },
                    opcode.MiscOpcode.SRA_HL => {
                        // SRA (HL)
                        self.memory.set(self.registers.hl, self.sra(self.memory.get(self.registers.hl)));
                    },
                    opcode.MiscOpcode.SRA_A => {
                        // SRA A
                        self.registers.setA(self.sra(self.registers.a()));
                    },
                    opcode.MiscOpcode.SWAP_B => {
                        // SWAP B
                        self.registers.setB(self.swap(self.registers.b()));
                    },
                    opcode.MiscOpcode.SWAP_C => {
                        // SWAP C
                        self.registers.setC(self.swap(self.registers.c()));
                    },
                    opcode.MiscOpcode.SWAP_D => {
                        // SWAP D
                        self.registers.setD(self.swap(self.registers.d()));
                    },
                    opcode.MiscOpcode.SWAP_E => {
                        // SWAP E
                        self.registers.setE(self.swap(self.registers.e()));
                    },
                    opcode.MiscOpcode.SWAP_H => {
                        // SWAP H
                        self.registers.setH(self.swap(self.registers.h()));
                    },
                    opcode.MiscOpcode.SWAP_L => {
                        // SWAP L
                        self.registers.setL(self.swap(self.registers.l()));
                    },
                    opcode.MiscOpcode.SWAP_HL => {
                        // SWAP (HL)
                        self.memory.set(self.registers.hl, self.swap(self.memory.get(self.registers.hl)));
                    },
                    opcode.MiscOpcode.SWAP_A => {
                        // SWAP A
                        self.registers.setA(self.swap(self.registers.a()));
                    },
                    opcode.MiscOpcode.SRL_B => {
                        // SRL B
                        self.registers.setB(self.srl(self.registers.b()));
                    },
                    opcode.MiscOpcode.SRL_C => {
                        // SRL C
                        self.registers.setC(self.srl(self.registers.c()));
                    },
                    opcode.MiscOpcode.SRL_D => {
                        // SRL D
                        self.registers.setD(self.srl(self.registers.d()));
                    },
                    opcode.MiscOpcode.SRL_E => {
                        // SRL E
                        self.registers.setE(self.srl(self.registers.e()));
                    },
                    opcode.MiscOpcode.SRL_H => {
                        // SRL H
                        self.registers.setH(self.srl(self.registers.h()));
                    },
                    opcode.MiscOpcode.SRL_L => {
                        // SRL L
                        self.registers.setL(self.srl(self.registers.l()));
                    },
                    opcode.MiscOpcode.SRL_HL => {
                        // SRL (HL)
                        self.memory.set(self.registers.hl, self.srl(self.memory.get(self.registers.hl)));
                    },
                    opcode.MiscOpcode.SRL_A => {
                        // SRL A
                        self.registers.setA(self.srl(self.registers.a()));
                    },
                    opcode.MiscOpcode.BIT_B => {
                        // BIT n,B
                        self.testBit(self.registers.b(), try self.stream.readByte());
                    },
                    opcode.MiscOpcode.BIT_C => {
                        // BIT n,C
                        self.testBit(self.registers.c(), try self.stream.readByte());
                    },
                    opcode.MiscOpcode.BIT_D => {
                        // BIT n,D
                        self.testBit(self.registers.d(), try self.stream.readByte());
                    },
                    opcode.MiscOpcode.BIT_E => {
                        // BIT n,E
                        self.testBit(self.registers.e(), try self.stream.readByte());
                    },
                    opcode.MiscOpcode.BIT_H => {
                        // BIT n,H
                        self.testBit(self.registers.h(), try self.stream.readByte());
                    },
                    opcode.MiscOpcode.BIT_L => {
                        // BIT n,L
                        self.testBit(self.registers.l(), try self.stream.readByte());
                    },
                    opcode.MiscOpcode.BIT_HL => {
                        // BIT n,(HL)
                        self.testBit(self.memory.get(self.registers.hl), try self.stream.readByte());
                    },
                    opcode.MiscOpcode.BIT_A => {
                        // BIT n,A
                        self.testBit(self.registers.a(), try self.stream.readByte());
                    },
                    opcode.MiscOpcode.RES_B => {
                        // RES n,B
                        self.registers.setB(self.registers.b() & ~(u8(1) << @truncate(u3, try self.stream.readByte())));
                    },
                    opcode.MiscOpcode.RES_C => {
                        // RES n,C
                        self.registers.setC(self.registers.c() & ~(u8(1) << @truncate(u3, try self.stream.readByte())));
                    },
                    opcode.MiscOpcode.RES_D => {
                        // RES n,D
                        self.registers.setD(self.registers.d() & ~(u8(1) << @truncate(u3, try self.stream.readByte())));
                    },
                    opcode.MiscOpcode.RES_E => {
                        // RES n,E
                        self.registers.setE(self.registers.e() & ~(u8(1) << @truncate(u3, try self.stream.readByte())));
                    },
                    opcode.MiscOpcode.RES_H => {
                        // RES n,H
                        self.registers.setH(self.registers.h() & ~(u8(1) << @truncate(u3, try self.stream.readByte())));
                    },
                    opcode.MiscOpcode.RES_L => {
                        // RES n,L
                        self.registers.setL(self.registers.l() & ~(u8(1) << @truncate(u3, try self.stream.readByte())));
                    },
                    opcode.MiscOpcode.RES_HL => {
                        // RES n,(HL)
                        self.memory.set(self.registers.hl, self.memory.get(self.registers.hl) & ~(u8(1) << @truncate(u3, try self.stream.readByte())));
                    },
                    opcode.MiscOpcode.RES_A => {
                        // RES n,A
                        self.registers.setA(self.registers.a() & ~(u8(1) << @truncate(u3, try self.stream.readByte())));
                    },
                    opcode.MiscOpcode.SET_B => {
                        // SET n,B
                        self.registers.setB(self.registers.b() | (u8(1) << @truncate(u3, try self.stream.readByte())));
                    },
                    opcode.MiscOpcode.SET_C => {
                        // SET n,C
                        self.registers.setC(self.registers.c() | (u8(1) << @truncate(u3, try self.stream.readByte())));
                    },
                    opcode.MiscOpcode.SET_D => {
                        // SET n,D
                        self.registers.setD(self.registers.d() | (u8(1) << @truncate(u3, try self.stream.readByte())));
                    },
                    opcode.MiscOpcode.SET_E => {
                        // SET n,E
                        self.registers.setE(self.registers.e() | (u8(1) << @truncate(u3, try self.stream.readByte())));
                    },
                    opcode.MiscOpcode.SET_H => {
                        // SET n,H
                        self.registers.setH(self.registers.h() | (u8(1) << @truncate(u3, try self.stream.readByte())));
                    },
                    opcode.MiscOpcode.SET_L => {
                        // SET n,L
                        self.registers.setL(self.registers.l() | (u8(1) << @truncate(u3, try self.stream.readByte())));
                    },
                    opcode.MiscOpcode.SET_HL => {
                        // SET n,(HL)
                        self.memory.set(self.registers.hl, self.memory.get(self.registers.hl) | (u8(1) << @truncate(u3, try self.stream.readByte())));
                    },
                    opcode.MiscOpcode.SET_A => {
                        // SET n,A
                        self.registers.setA(self.registers.a() | (u8(1) << @truncate(u3, try self.stream.readByte())));
                    },
                    else => {
                        unreachable;
                    },
                }
            },
            opcode.Opcode.CALL_Z_nn => {
                // CALL Z,nn
                if (self.registers.zeroFlag()) {
                    self.call(try self.stream.readIntLittle(u16));
                }
            },
            opcode.Opcode.CALL_nn => {
                // CALL nn
                self.call(try self.stream.readIntLittle(u16));
            },
            opcode.Opcode.ADD_A_n => {
                // ADC A,n
                self.registers.setA(self.add(u8, self.registers.a(), (try self.stream.readByte()) + @boolToInt(self.registers.carryFlag())));
            },
            opcode.Opcode.RET_NC => {
                // RET NC
                if (!self.registers.carryFlag()) {
                    self.registers.pc = self.pop(u16);
                }
            },
            opcode.Opcode.POP_DE => {
                // POP DE
                self.registers.de = self.pop(u16);
            },
            opcode.Opcode.JP_NC_nn => {
                // JP NC,nn
                const address = try self.stream.readIntLittle(u16);
                if (!self.registers.carryFlag()) {
                    self.registers.pc = address;
                }
            },
            opcode.Opcode.CALL_NC_nn => {
                // CALL NC,nn
                if (!self.registers.carryFlag()) {
                    self.call(try self.stream.readIntLittle(u16));
                }
            },
            opcode.Opcode.PUSH_DE => {
                // PUSH DE
                self.push(self.registers.de);
            },
            opcode.Opcode.SBC_A_n => {
                // SUB A,n
                self.registers.setA(self.sub(self.registers.a(), try self.stream.readByte()));
            },
            opcode.Opcode.RET_C => {
                // RET C
                if (self.registers.carryFlag()) {
                    self.registers.pc = self.pop(u16);
                }
            },
            opcode.Opcode.RETI => {
                // RETI
                self.registers.pc = self.pop(u16);
                return Mode.EnableInterrupts;
            },
            opcode.Opcode.JP_C_nn => {
                // JP C,nn
                const address = try self.stream.readIntLittle(u16);
                if (self.registers.carryFlag()) {
                    self.registers.pc = address;
                }
            },
            opcode.Opcode.CALL_C_nn => {
                // CALL C,nn
                if (self.registers.carryFlag()) {
                    self.call(try self.stream.readIntLittle(u16));
                }
            },
            opcode.Opcode.SUB_A_n => {
                // SBC A,n
                self.registers.setA(self.sub(self.registers.a(), (try self.stream.readByte()) + @boolToInt(self.registers.carryFlag())));
            },
            opcode.Opcode.LDH_n_A => {
                // LDH ($FF00+n),A
                self.memory.set(0xFF00 | u16(try self.stream.readByte()), self.registers.a());
            },
            opcode.Opcode.POP_HL => {
                // POP HL
                self.registers.hl = self.pop(u16);
            },
            opcode.Opcode.LD_mem_C_A => {
                // LD ($FF00+C),A
                self.memory.set(0xFF00 | u16(self.registers.c()), self.registers.a());
            },
            opcode.Opcode.PUSH_HL => {
                // PUSH HL
                self.push(self.registers.hl);
            },
            opcode.Opcode.AND_A_n => {
                // AND A,n
                self.registers.setA(self.bitwiseAnd(self.registers.a(), try self.stream.readByte()));
            },
            opcode.Opcode.ADD_SP_n => {
                // ADD SP,n
                self.registers.sp = self.add(u16, self.registers.sp, try self.stream.readByte());
            },
            opcode.Opcode.JP_HL => {
                // JP (HL)
                self.registers.pc = self.memory.get(self.registers.hl);
            },
            opcode.Opcode.LD_nn_A => {
                // LD (nn),A
                self.memory.set(try self.stream.readIntLittle(u16), self.registers.a());
            },
            opcode.Opcode.XOR_A_n => {
                // XOR A,n
                self.registers.setA(self.bitwiseXor(self.registers.a(), try self.stream.readByte()));
            },
            opcode.Opcode.LDH_A_n => {
                // LDH A,($FF00+n)
                self.registers.setA(self.memory.get(0xFF00 | u16(try self.stream.readByte())));
            },
            opcode.Opcode.POP_AF => {
                // POP AF
                self.registers.af = self.pop(u16);
            },
            opcode.Opcode.LD_A_mem_C => {
                // LD A,($FF00+C)
                self.registers.setA(self.memory.get(u16(0xFF00) | self.registers.c()));
            },
            opcode.Opcode.DI => {
                // DI
                return Mode.DisableInterrupts;
            },
            opcode.Opcode.PUSH_AF => {
                // PUSH AF
                self.push(self.registers.af);
            },
            opcode.Opcode.OR_A_n => {
                // OR A,n
                self.registers.setA(self.bitwiseOr(self.registers.a(), try self.stream.readByte()));
            },
            opcode.Opcode.LDHL_SP_n => {
                // LDHL SP,n
                const n = try self.stream.readByte();
                self.registers.hl = self.add(u8, @truncate(u8, self.registers.sp), n);
            },
            opcode.Opcode.LD_SP_HL => {
                // LD SP,HL
                self.registers.sp = self.registers.hl;
            },
            opcode.Opcode.LD_A_nn => {
                // LD A,(HL)
                const address = try self.stream.readIntLittle(u16);
                self.registers.setA(self.memory.get(address));
            },
            opcode.Opcode.EI => {
                return Mode.EnableInterrupts;
            },
            opcode.Opcode.CP_A_n => {
                // CP A,n
                _ = self.sub(self.registers.a(), try self.stream.readByte());
            },
            else => {
                unreachable;
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
