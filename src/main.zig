const std = @import("std");

const c = @import("c.zig");

const ErrorSet = error{SDLError};

pub const CPU = struct {
    af: u16,
    bc: u16,
    de: u16,
    hl: u16,
    sp: u16,
    pc: u16,
    flag: u8,

    const stack_init_address = 0xfffe;
    const pc_init_address = 0x100;

    pub fn init() CPU {
        return CPU{
            .af = 0,
            .bc = 0,
            .de = 0,
            .hl = 0,
            .sp = stack_init_address,
            .pc = pc_init_address,
            .flag = 0,
        };
    }

    pub fn a(self: *const CPU) u8 {
        return @truncate(u8, self.af >> 8);
    }

    pub fn setA(self: *CPU, value: u8) void {
        return self.af = (u16(value) << 8) | self.f();
    }

    pub fn b(self: *const CPU) u8 {
        return @truncate(u8, self.bc >> 8);
    }

    pub fn setB(self: *CPU, value: u8) void {
        return self.bc = (u16(value) << 8) | self.c();
    }

    pub fn c(self: *const CPU) u8 {
        return @truncate(u8, self.bc);
    }

    pub fn setC(self: *CPU, value: u8) void {
        return self.bc = (u16(self.b()) << 8) | value;
    }

    pub fn d(self: *const CPU) u8 {
        return @truncate(u8, self.de >> 8);
    }

    pub fn setD(self: *CPU, value: u8) void {
        return self.de = (u16(value) << 8) | self.e();
    }

    pub fn e(self: *const CPU) u8 {
        return @truncate(u8, self.de);
    }

    pub fn setE(self: *CPU, value: u8) void {
        return self.de = (u16(self.d()) << 8) | value;
    }

    pub fn f(self: *const CPU) u8 {
        return @truncate(u8, self.af);
    }

    pub fn setF(self: *CPU, value: u8) void {
        return self.af = (u16(self.a()) << 8) | value;
    }

    pub fn h(self: *const CPU) u8 {
        return @truncate(u8, self.hl >> 8);
    }

    pub fn setH(self: *CPU, value: u8) void {
        return self.hl = (u16(value) << 8) | self.l();
    }

    pub fn l(self: *const CPU) u8 {
        return @truncate(u8, self.hl);
    }

    pub fn setL(self: *CPU, value: u8) void {
        return self.hl = (u16(self.h()) << 8) | value;
    }

    pub fn zeroFlag(self: *const CPU) bool {
        return (self.flag & 0x80) != 0;
    }

    pub fn subtractFlag(self: *const CPU) bool {
        return (self.flag & 0x40) != 0;
    }

    pub fn halfCarryFlag(self: *const CPU) bool {
        return (self.flag & 0x20) != 0;
    }

    pub fn carryFlag(self: *const CPU) bool {
        return (self.flag & 0x10) != 0;
    }
};

test "CPU registers" {
    var cpu = CPU.init();
    cpu.af = 0xff11;
    std.debug.assert(cpu.a() == 0xff);
    std.debug.assert(cpu.f() == 0x11);
    cpu.setA(0x11);
    cpu.setF(0x55);
    std.debug.assert(cpu.a() == 0x11);
    std.debug.assert(cpu.f() == 0x55);

    cpu.bc = 0xff11;
    std.debug.assert(cpu.b() == 0xff);
    std.debug.assert(cpu.c() == 0x11);
    cpu.setB(0x11);
    cpu.setC(0x55);
    std.debug.assert(cpu.b() == 0x11);
    std.debug.assert(cpu.c() == 0x55);

    cpu.de = 0xff11;
    std.debug.assert(cpu.d() == 0xff);
    std.debug.assert(cpu.e() == 0x11);
    cpu.setD(0x11);
    cpu.setE(0x55);
    std.debug.assert(cpu.d() == 0x11);
    std.debug.assert(cpu.e() == 0x55);

    cpu.hl = 0xff11;
    std.debug.assert(cpu.h() == 0xff);
    std.debug.assert(cpu.l() == 0x11);
    cpu.setH(0x11);
    cpu.setL(0x55);
    std.debug.assert(cpu.h() == 0x11);
    std.debug.assert(cpu.l() == 0x55);
}

pub fn main() !void {
    if (c.SDL_Init(c.SDL_INIT_VIDEO | c.SDL_INIT_AUDIO) != 0) {
        std.debug.warn("Unable to initialize SDL: {s}", c.SDL_GetError());
        return ErrorSet.SDLError;
    }
    defer c.SDL_Quit();

    const screen_width = 256;
    const screen_height = 256;
    const undefined_pos = 0x1FFF0000;

    const window = c.SDL_CreateWindow(
        c"gbemu",
        undefined_pos,
        undefined_pos,
        screen_width,
        screen_height,
        c.SDL_WINDOW_SHOWN,
    );
    if (window == null) {
        std.debug.warn("Cannot create window: {s}", c.SDL_GetError());
        return ErrorSet.SDLError;
    }
    defer c.SDL_DestroyWindow(window);
    const screen = c.SDL_GetWindowSurface(window);
}
