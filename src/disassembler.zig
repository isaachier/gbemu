const std = @import("std");

const opcode = @import("opcode.zig");

pub const Disassembler = struct {
    const InStream = std.io.InStream(std.os.File.ReadError);
    const OutStream = std.io.OutStream(std.os.File.WriteError);

    input: *InStream,
    output: *OutStream,

    pub fn init(input: *InStream, output: *OutStream) Disassembler {
        return Disassembler{
            .input = input,
            .output = output,
        };
    }

    pub fn disassemble(self: *Disassembler) !void {
        const byte = try self.input.readByte();
        const op = @intToEnum(opcode.Opcode, byte);
        switch (op) {
            opcode.Opcode.NOP => try self.output.print("nop"),
            opcode.Opcode.LD_A_n => try self.output.print("ld a {x}", try self.input.readByte()),
            opcode.Opcode.LD_B_n => try self.output.print("ld b {x}", try self.input.readByte()),
            opcode.Opcode.LD_D_n => try self.output.print("ld d {x}", try self.input.readByte()),
            opcode.Opcode.LD_E_n => try self.output.print("ld e {x}", try self.input.readByte()),
            opcode.Opcode.LD_H_n => try self.output.print("ld h {x}", try self.input.readByte()),
            opcode.Opcode.LD_L_n => try self.output.print("ld l {x}", try self.input.readByte()),
            opcode.Opcode.LD_A_A => try self.output.print("ld a,a"),
            opcode.Opcode.LD_A_B => try self.output.print("ld a,b"),
            opcode.Opcode.LD_A_C => try self.output.print("ld a,c"),
            opcode.Opcode.LD_A_D => try self.output.print("ld a,d"),
            opcode.Opcode.LD_A_E => try self.output.print("ld a,e"),
            opcode.Opcode.LD_A_H => try self.output.print("ld a,h"),
            opcode.Opcode.LD_A_L => try self.output.print("ld a,l"),
            opcode.Opcode.LD_A_HL => try self.output.print("ld a,(hl)"),
            opcode.Opcode.LD_B_B => try self.output.print("ld b,b"),
            opcode.Opcode.LD_B_C => try self.output.print("ld b,c"),
            opcode.Opcode.LD_B_D => try self.output.print("ld b,d"),
            opcode.Opcode.LD_B_E => try self.output.print("ld b,e"),
            opcode.Opcode.LD_B_H => try self.output.print("ld b,h"),
            opcode.Opcode.LD_B_L => try self.output.print("ld b,l"),
            opcode.Opcode.LD_B_HL => try self.output.print("ld b,(hl)"),
            opcode.Opcode.LD_C_B => try self.output.print("ld c,b"),
            opcode.Opcode.LD_C_C => try self.output.print("ld c,c"),
            opcode.Opcode.LD_C_D => try self.output.print("ld c,d"),
            opcode.Opcode.LD_C_E => try self.output.print("ld c,e"),
            opcode.Opcode.LD_C_H => try self.output.print("ld c,h"),
            opcode.Opcode.LD_C_L => try self.output.print("ld c,l"),
            opcode.Opcode.LD_C_HL => try self.output.print("ld c,(hl)"),
            opcode.Opcode.LD_D_B => try self.output.print("ld d,b"),
            opcode.Opcode.LD_D_C => try self.output.print("ld d,c"),
            opcode.Opcode.LD_D_D => try self.output.print("ld d,d"),
            opcode.Opcode.LD_D_E => try self.output.print("ld d,e"),
            opcode.Opcode.LD_D_H => try self.output.print("ld d,h"),
            opcode.Opcode.LD_D_L => try self.output.print("ld d,l"),
            opcode.Opcode.LD_D_HL => try self.output.print("ld d,(hl)"),
            opcode.Opcode.LD_E_B => try self.output.print("ld e,b"),
            opcode.Opcode.LD_E_C => try self.output.print("ld e,c"),
            opcode.Opcode.LD_E_D => try self.output.print("ld e,d"),
            opcode.Opcode.LD_E_E => try self.output.print("ld e,e"),
            opcode.Opcode.LD_E_H => try self.output.print("ld e,h"),
            opcode.Opcode.LD_E_L => try self.output.print("ld e,l"),
            opcode.Opcode.LD_E_HL => try self.output.print("ld e,(hl)"),
            opcode.Opcode.LD_H_B => try self.output.print("ld h,b"),
            opcode.Opcode.LD_H_C => try self.output.print("ld h,c"),
            opcode.Opcode.LD_H_D => try self.output.print("ld h,d"),
            opcode.Opcode.LD_H_E => try self.output.print("ld h,e"),
            opcode.Opcode.LD_H_H => try self.output.print("ld h,h"),
            opcode.Opcode.LD_H_L => try self.output.print("ld h,l"),
            opcode.Opcode.LD_H_HL => try self.output.print("ld h,(hl)"),
            opcode.Opcode.LD_L_B => try self.output.print("ld l,b"),
            opcode.Opcode.LD_L_C => try self.output.print("ld l,c"),
            opcode.Opcode.LD_L_D => try self.output.print("ld l,d"),
            opcode.Opcode.LD_L_E => try self.output.print("ld l,e"),
            opcode.Opcode.LD_L_H => try self.output.print("ld l,h"),
            opcode.Opcode.LD_L_L => try self.output.print("ld l,l"),
            opcode.Opcode.LD_L_HL => try self.output.print("ld l,(hl)"),
            opcode.Opcode.LD_HL_B => try self.output.print("ld (hl),b"),
            opcode.Opcode.LD_HL_C => try self.output.print("ld (hl),c"),
            opcode.Opcode.LD_HL_D => try self.output.print("ld (hl),d"),
            opcode.Opcode.LD_HL_E => try self.output.print("ld (hl),e"),
            opcode.Opcode.LD_HL_H => try self.output.print("ld (hl),h"),
            opcode.Opcode.LD_HL_L => try self.output.print("ld (hl),l"),
            opcode.Opcode.LD_HL_n => try self.output.print("ld (hl),{x}", try self.input.readByte()),
            opcode.Opcode.LD_A_BC => try self.output.print("ld a,(bc)"),
            opcode.Opcode.LD_A_DE => try self.output.print("ld a,(de)"),
            opcode.Opcode.LD_A_nn => try self.output.print("ld a,({x})", try self.input.readIntLe(u16)),
            opcode.Opcode.LD_B_A => try self.output.print("ld b,a"),
            opcode.Opcode.LD_C_A => try self.output.print("ld c,a"),
            opcode.Opcode.LD_D_A => try self.output.print("ld d,a"),
            opcode.Opcode.LD_E_A => try self.output.print("ld e,a"),
            opcode.Opcode.LD_H_A => try self.output.print("ld h,a"),
            opcode.Opcode.LD_L_A => try self.output.print("ld l,a"),
            opcode.Opcode.LD_BC_A => try self.output.print("ld (bc),a"),
            opcode.Opcode.LD_DE_A => try self.output.print("ld (de),a"),
            opcode.Opcode.LD_HL_A => try self.output.print("ld (hl),a"),
            opcode.Opcode.LD_A_mem_C => try self.output.print("ld a,($FF00 + c)"),
            opcode.Opcode.LD_mem_C_A => try self.output.print("ld ($FF00 + c),a"),
            opcode.Opcode.LDD_A_HL => try self.output.print("ldd a,(hl)"),
            opcode.Opcode.LDD_HL_A => try self.output.print("ldd (hl),a"),
            opcode.Opcode.LDI_A_HL => try self.output.print("ldi a,(hl)"),
            opcode.Opcode.LDH_n_A => try self.output.print("ldh {x},a", try self.input.readByte()),
            else => unreachable,
        }
    }
};

test "Disassembler" {
    var stdin = try std.io.getStdIn();
    var stdout = try std.io.getStdOut();
    var inStream = std.io.FileInStream.init(&stdin);
    var outStream = std.io.FileOutStream.init(&stdout);
    var disassembler = Disassembler.init(&inStream.stream, &outStream.stream);
    try disassembler.disassemble();
}
