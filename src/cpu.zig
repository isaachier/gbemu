const std = @import("std");

pub const Registers = struct {
    af: u16,
    bc: u16,
    de: u16,
    hl: u16,
    sp: u16,
    pc: u16,

    const stack_init_address = 0xfffe;
    const pc_init_address = 0x100;

    pub fn init() Registers {
        return Registers{
            .af = 0,
            .bc = 0,
            .de = 0,
            .hl = 0,
            .sp = stack_init_address,
            .pc = pc_init_address,
        };
    }

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
    var registers = Registers.init();
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
    pub const ErrorSet = error{ InvalidAddress, };

    const interrupt_enable_register_address = 0xffff;

    const secondary_internal_ram_start = 0xff80;
    const secondary_internal_ram_len = interrupt_enable_register_address - internal_ram_start;

    const io_ports_end = 0xff4c;
    const io_ports_start = 0xff00;
    const io_ports_len = io_ports_end - io_ports_start;

    const sprite_attrib_memory_end = 0xfea0;
    const sprite_attrib_memory_start = 0xfe00;
    const sprite_attrib_memory_len = sprite_attrib_memory_end - sprite_attrib_memory_start;

    const internal_ram_end = 0xe000;
    const internal_ram_start = 0xc000;
    const internal_ram_len = internal_ram_end - internal_ram_start;

    const echo_ram_len = sprite_attrib_memory_start - internal_ram_end;

    const switchable_ram_bank_start = 0xa000;
    const switchable_ram_bank_len = internal_ram_start - switchable_ram_bank_start;

    const video_ram_start = 0x8000;
    const video_ram_len = switchable_ram_bank_start - video_ram_start;

    const switchable_rom_bank_start = 0x4000;
    const switchable_rom_bank_len = video_ram_start - switchable_rom_bank_start;

    const init_rom_bank_start = 0x0000;
    const init_rom_bank_len = switchable_rom_bank_start - init_rom_bank_start;

    allocator: *std.mem.Allocator,
    interrupt_enable_register: u8,
    secondary_internal_ram: []u8,
    io_ports: []u8,
    sprite_attrib_memory: []u8,
    internal_ram: []u8,
    switchable_ram_bank: []u8,
    video_ram: []u8,
    switchable_rom_bank: []u8,
    init_rom_bank: []u8,

    pub fn init(allocator: *std.mem.Allocator) !Memory {
        var secondary_internal_ram = try allocator.alloc(u8, secondary_internal_ram_len);
        errdefer allocator.free(secondary_internal_ram);
        var io_ports = try allocator.alloc(u8, io_ports_len);
        errdefer allocator.free(io_ports);
        var sprite_attrib_memory = try allocator.alloc(u8, sprite_attrib_memory_len);
        errdefer allocator.free(sprite_attrib_memory);
        var internal_ram = try allocator.alloc(u8, internal_ram_len);
        errdefer allocator.free(internal_ram);
        var switchable_ram_bank = try allocator.alloc(u8, switchable_ram_bank_len);
        errdefer allocator.free(switchable_ram_bank);
        var video_ram = try allocator.alloc(u8, video_ram_len);
        errdefer allocator.free(video_ram);
        var switchable_rom_bank = try allocator.alloc(u8, switchable_rom_bank_len);
        errdefer allocator.free(switchable_rom_bank);
        var init_rom_bank = try allocator.alloc(u8, init_rom_bank_len);
        errdefer allocator.free(init_rom_bank);

        return Memory{
            .allocator = allocator,
            .interrupt_enable_register = 0,
            .secondary_internal_ram = secondary_internal_ram,
            .io_ports = io_ports,
            .sprite_attrib_memory = sprite_attrib_memory,
            .internal_ram = internal_ram,
            .switchable_ram_bank = switchable_ram_bank,
            .video_ram = video_ram,
            .switchable_rom_bank = switchable_rom_bank,
            .init_rom_bank = init_rom_bank,
        };
    }

    pub fn deinit(self: *Memory) void {
        self.allocator.free(self.secondary_internal_ram);
        self.allocator.free(self.io_ports);
        self.allocator.free(self.sprite_attrib_memory);
        self.allocator.free(self.internal_ram);
        self.allocator.free(self.switchable_ram_bank);
        self.allocator.free(self.video_ram);
        self.allocator.free(self.switchable_rom_bank);
        self.allocator.free(self.init_rom_bank);
    }

    fn internalGet(self: *Memory, address: u16) !*u8 {
        return switch (address) {
            interrupt_enable_register_address => &self.interrupt_enable_register,
            secondary_internal_ram_start...interrupt_enable_register_address - 1 =>
                &self.secondary_internal_ram[address - secondary_internal_ram_start],
            io_ports_start...io_ports_end - 1 =>
                &self.io_ports[address - io_ports_start],
            sprite_attrib_memory_start...sprite_attrib_memory_end - 1 =>
                &self.sprite_attrib_memory[address - sprite_attrib_memory_start],
            internal_ram_end...sprite_attrib_memory_start - 1 =>
                &self.internal_ram[address - echo_ram_len - internal_ram_start],
            internal_ram_start...internal_ram_end - 1 =>
                &self.internal_ram[address - internal_ram_start],
            switchable_ram_bank_start...internal_ram_start - 1 =>
                &self.switchable_ram_bank[address - switchable_ram_bank_start],
            video_ram_start...switchable_ram_bank_start - 1 =>
                &self.video_ram[address - video_ram_start],
            switchable_rom_bank_start...video_ram_start - 1 =>
                &self.switchable_rom_bank[address - switchable_rom_bank_start],
            init_rom_bank_start...switchable_rom_bank_start - 1 =>
                &self.switchable_rom_bank[address - init_rom_bank_start],
            else => return ErrorSet.InvalidAddress,
        };
    }

    pub fn get(self: *Memory, address: u16) !u8 {
        return (try self.internalGet(address)).*;
    }

    pub fn set(self: *Memory, address: u16, value: u8) !void {
        (try self.internalGet(address)).* = value;
    }
};

test "Memory" {
    var mem = try Memory.init(std.debug.global_allocator);
    try mem.set(0xFFFF, 3);
    std.debug.assert((try mem.get(0xFFFF)) == 3);
    std.debug.assert(mem.interrupt_enable_register == 3);
    mem.deinit();
}

pub const CPU = struct {
    registers: Registers,
    memory: Memory,

    pub fn init() CPU {
        return CPU{
            registers: Registers.init(),
            memory: Memory.init(),
        };
    }

    pub fn execute(self: *CPU) void {
    }
};
