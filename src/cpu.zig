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

pub const CPU = struct {
    allocator: *std.mem.Allocator,
    registers: Registers,
    memory: []u8,

    pub fn init(allocator: *std.mem.Allocator) !CPU {
        const memory_len = 0xffff;
        return CPU{
            .allocator = allocator,
            .registers = undefined,
            .memory = try allocator.alloc(u8, memory_len),
        };
    }

    pub fn deinit(self: *CPU) void {
        self.allocator.free(self.memory);
    }

    pub fn execute(self: *CPU) void {
        // TODO
    }
};

test "CPU" {
    var cpu = try CPU.init(std.debug.global_allocator);
    cpu.deinit();
}
