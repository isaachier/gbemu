const std = @import("std");

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

    pub fn zeroFlag(self: *const Registers) bool {
        return (self.f() & 0x80) != 0;
    }

    pub fn subtractFlag(self: *const Registers) bool {
        return (self.f() & 0x40) != 0;
    }

    pub fn halfCarryFlag(self: *const Registers) bool {
        return (self.f() & 0x20) != 0;
    }

    pub fn carryFlag(self: *const Registers) bool {
        return (self.f() & 0x10) != 0;
    }
};

test "Registers" {
    var registers : Registers = undefined;
    registers.af = 0xff11;
    std.debug.assert(registers.a() == 0xff);
    std.debug.assert(registers.f() == 0x11);
    registers.setA(0x11);
    registers.setF(0x55);
    std.debug.assert(registers.a() == 0x11);
    std.debug.assert(registers.f() == 0x55);

    registers.bc = 0xff11;
    std.debug.assert(registers.b() == 0xff);
    std.debug.assert(registers.c() == 0x11);
    registers.setB(0x11);
    registers.setC(0x55);
    std.debug.assert(registers.b() == 0x11);
    std.debug.assert(registers.c() == 0x55);

    registers.de = 0xff11;
    std.debug.assert(registers.d() == 0xff);
    std.debug.assert(registers.e() == 0x11);
    registers.setD(0x11);
    registers.setE(0x55);
    std.debug.assert(registers.d() == 0x11);
    std.debug.assert(registers.e() == 0x55);

    registers.hl = 0xff11;
    std.debug.assert(registers.h() == 0xff);
    std.debug.assert(registers.l() == 0x11);
    registers.setH(0x11);
    registers.setL(0x55);
    std.debug.assert(registers.h() == 0x11);
    std.debug.assert(registers.l() == 0x55);
}

pub const Memory = struct {
    pub const ErrorSet = error{
        InvalidAddress,
    };

    const memory_len = 0xffff - 0x2000;

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

    fn internalIndex(index: u16) !u16 {
        if (index > 0xffff) {
            return ErrorSet.InvalidAddress;
        }
        if (index < 0xe000) {
            return index;
        }
        return index - 0x2000;
    }

    pub fn get(self: *const Memory, index: u16) !u8 {
        return self.memory[try internalIndex(index)];
    }

    pub fn set(self: *Memory, index: u16, value: u8) !void {
        self.memory[try internalIndex(index)] = value;
    }

};

pub const CPU = struct {
    pub const ErrorSet = error{ InvalidInstruction, };

    registers: Registers,
    memory: Memory,

    pub fn init(allocator: *std.mem.Allocator) !CPU {
        return CPU{
            .registers = undefined,
            .memory = try Memory.init(allocator),
        };
    }

    pub fn deinit(self: *CPU) void {
        self.memory.deinit();
    }

    pub fn execute(self: *CPU) !void {
        switch (try self.memory.get(self.registers.pc)) {
            // 8-Bit Loads
            // Immediate value loads
            0x06 => {
                // LD B,n
                self.registers.setB(try self.memory.get(self.registers.pc + 1));
                self.registers.pc += 2;
            },
            0x0E => {
                // LD C,n
                self.registers.setC(try self.memory.get(self.registers.pc + 1));
                self.registers.pc += 2;
            },
            0x16 => {
                // LD D,n
                self.registers.setD(try self.memory.get(self.registers.pc + 1));
                self.registers.pc += 2;
            },
            0x1E => {
                // LD E,n
                self.registers.setE(try self.memory.get(self.registers.pc + 1));
                self.registers.pc += 2;
            },
            0x26 => {
                // LD H,n
                self.registers.setH(try self.memory.get(self.registers.pc + 1));
                self.registers.pc += 2;
            },
            0x2E => {
                // LD L,n
                self.registers.setL(try self.memory.get(self.registers.pc + 1));
                self.registers.pc += 2;
            },
            // Register loads
            0x7F => {
                // LD A,A
                self.registers.setA(self.registers.a());
                self.registers.pc += 1;
            },
            0x78 => {
                // LD A,B
                self.registers.setA(self.registers.b());
                self.registers.pc += 1;
            },
            0x79 => {
                // LD A,C
                self.registers.setA(self.registers.c());
                self.registers.pc += 1;
            },
            0x7A => {
                // LD A,D
                self.registers.setA(self.registers.d());
                self.registers.pc += 1;
            },
            0x7B => {
                // LD A,E
                self.registers.setA(self.registers.e());
                self.registers.pc += 1;
            },
            0x7C => {
                // LD A,H
                self.registers.setA(self.registers.h());
                self.registers.pc += 1;
            },
            0x7D => {
                // LD A,L
                self.registers.setA(self.registers.l());
                self.registers.pc += 1;
            },
            0x7E => {
                // LD A,(HL)
                self.registers.setA(try self.memory.get(self.registers.hl));
                self.registers.pc += 1;
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
    try cpu.memory.set(0x0, 0x7E);
    try cpu.memory.set(0x55, 0x20);
    try cpu.execute();
    std.debug.warn("a = {x}\n", cpu.registers.a());
    std.debug.assert(cpu.registers.a() == 0x20);
    std.debug.assert(cpu.registers.pc == 1);
    cpu.deinit();
}
