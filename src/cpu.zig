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

    fn add(self: *CPU, a: u8, b: u8) u8 {
        const result = a + b;
        self.registers.setHalfCarryFlag((((a & 0xF) + (b & 0xF)) & 0x10) == 0x10);
        self.registers.setCarryFlag(((((a >> 4) & 0xF) + ((b >> 4) & 0xF)) & 0x10) == 0x10);
        self.registers.setZeroFlag(result == 0);
        self.registers.setSubtractFlag(false);
        return result;
    }

    pub fn execute(self: *CPU) !void {
        switch (try self.stream.readByte()) {
            0x01 => {
                // LD BC,nn
                self.registers.bc = try self.stream.readIntLe(u16);
            },
            0x02 => {
                // LD (BC),A
                self.memory.set(self.registers.bc, self.registers.a());
            },
            0x06 => {
                // LD B,n
                self.registers.setB(try self.stream.readByte());
            },
            0x08 => {
                // LD (nn),SP
                const value = try self.stream.readIntLe(u16);
                self.memory.set(value, @truncate(u8, (self.registers.sp & 0xFF00) >> 8));
                self.memory.set(value + 1, @truncate(u8, self.registers.sp));
            },
            0x0A => {
                // LD A,(BC)
                self.registers.setA(self.memory.get(self.registers.bc));
            },
            0x0E => {
                // LD C,n
                self.registers.setC(try self.stream.readByte());
            },
            0x11 => {
                // LD DE,nn
                self.registers.de = try self.stream.readIntLe(u16);
            },
            0x12 => {
                // LD (DE),A
                self.memory.set(self.registers.de, self.registers.a());
            },
            0x16 => {
                // LD D,n
                self.registers.setD(try self.stream.readByte());
            },
            0x1A => {
                // LD A,(DE)
                self.registers.setA(self.memory.get(self.registers.de));
            },
            0x1E => {
                // LD E,n
                self.registers.setE(try self.stream.readByte());
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
            0x26 => {
                // LD H,n
                self.registers.setH(try self.stream.readByte());
            },
            0x2A => {
                // LDI A,(HL)
                self.registers.setA(self.memory.get(self.registers.hl));
                self.registers.hl +%= 1;
            },
            0x2E => {
                // LD L,n
                self.registers.setL(try self.stream.readByte());
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
            0x36 => {
                // LD (HL),n
                self.memory.set(self.registers.hl, try self.stream.readByte());
            },
            0x3A => {
                // LDD A,(HL)
                self.registers.setA(self.memory.get(self.registers.hl));
                self.registers.hl -%= 1;
            },
            0x3E => {
                // LD A,n
                self.registers.setA(try self.stream.readByte());
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
            0x87 => {
                // ADD A,A
                self.registers.setA(self.add(self.registers.a(), self.registers.a()));
            },
            0xC1 => {
                // POP BC
                self.registers.bc = self.pop(u16);
            },
            0xC5 => {
                // PUSH BC
                self.push(self.registers.bc);
            },
            0xD1 => {
                // POP DE
                self.registers.de = self.pop(u16);
            },
            0xD5 => {
                // PUSH DE
                self.push(self.registers.de);
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
            0xEA => {
                // LD (nn),A
                self.memory.set(try self.stream.readIntLe(u16), self.registers.a());
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
            0xF5 => {
                // PUSH AF
                self.push(self.registers.af);
            },
            0xF8 => {
                // LDHL SP,n
                const n = try self.stream.readByte();
                self.registers.hl = self.add(@truncate(u8, self.registers.sp), n);
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
            else => {
                return ErrorSet.InvalidInstruction;
            },
        }
    }
};

test "CPU" {
    var cpu = try CPU.init(std.debug.global_allocator);
    cpu.registers.pc = 0;
    cpu.registers.hl = 0x55;
    cpu.memory.set(0x0, 0x7E);
    cpu.memory.set(0x55, 0x20);
    try cpu.execute();
    std.debug.assert(cpu.registers.a() == 0x20);
    std.debug.assert(cpu.registers.pc == 1);
    cpu.deinit();
}
