const std = @import("std");

const opcode = @import("opcode.zig");

fn toLower(byte: u8) u8 {
    return switch (byte) {
        'A' ... 'Z' => byte - 'A',
        else => byte,
    };
}

const Tokenizer = struct {
    allocator: *std.mem.Allocator,
    input: *Assembler.InStream,

    pub fn init(allocator: *std.mem.Allocator, input: *Assembler.InStream) Tokenizer {
        return Tokenizer{
            .allocator = allocator,
            .input = input,
        };
    }

    pub fn next(self: *Tokenizer) ![]const u8 {
        const delims = std.cstr.line_sep ++ "\t ,";
        var buffer = std.Buffer.initNull(self.allocator);
        try buffer.resize(0);
        defer buffer.deinit();
        var byte = try self.input.readByte();
        while (std.mem.indexOfScalar(u8, delims, byte) == null) {
            const lower = toLower(byte);
            try buffer.appendByte(@truncate(u8, @intCast(c_uint, lower)));
            byte = try self.input.readByte();
        }
        return buffer.toOwnedSlice();
    }
};

const Instruction = enum {
    ADC,
    ADD,
    AND,
    BIT,
    CALL,
    CCF,
    CP,
    CPL,
    DAA,
    DEC,
    DI,
    EI,
    HALT,
    INC,
    JP,
    JR,
    LD,
    LDD,
    LDH,
    LDI,
    NOP,
    OR,
    POP,
    PUSH,
    RES,
    RET,
    RL,
    RLA,
    RLC,
    RLCA,
    RR,
    RRA,
    RRC,
    RRCA,
    RST,
    SBC,
    SCF,
    SET,
    SLA,
    SRA,
    SRL,
    STOP,
    SUB,
    SWAP,
    XOR,
};

const instruction_strings = [][]const u8{
    "adc",
    "add",
    "and",
    "bit",
    "call",
    "ccf",
    "cp",
    "cpl",
    "daa",
    "dec",
    "di",
    "ei",
    "halt",
    "inc",
    "jp",
    "jr",
    "ld",
    "ldd",
    "ldh",
    "ldi",
    "nop",
    "or",
    "pop",
    "push",
    "res",
    "ret",
    "rl",
    "rla",
    "rlc",
    "rlca",
    "rr",
    "rra",
    "rrc",
    "rrca",
    "rst",
    "sbc",
    "scf",
    "set",
    "sla",
    "sra",
    "srl",
    "stop",
    "sub",
    "swap",
    "xor",
};

const EightBitOperand = enum {
    A,
    B,
    C,
    D,
    E,
    H,
    L,
    MEM_HL,
};

const eight_bit_operand_strings = [][]const u8 {
    "a",
    "b",
    "c",
    "d",
    "e",
    "h",
    "l",
    "(hl)",
};

fn binarySearchEnumString(comptime enum_type: type, str: []const u8, array: []const[]const u8) ?enum_type {
    var l: @TagType(enum_type) = 0;
    var r = @truncate(@TagType(enum_type), array.len - 1);
    while (l <= r) {
        const m = (l + r) / 2;
        if (std.mem.lessThan(u8, array[m], str)) {
            l = m + 1;
        } else if (std.mem.lessThan(u8, str, array[m])) {
            r = m - 1;
        } else {
            return @intToEnum(enum_type, m);
        }
    }
    return null;
}

pub const Assembler = struct {
    const InStream = std.io.InStream(std.os.File.ReadError);
    const OutStream = std.io.OutStream(std.os.File.WriteError);

    const Error = error{
        InvalidDestination,
    };

    tokenizer: Tokenizer,
    output: *OutStream,

    pub fn init(allocator: *std.mem.Allocator, input: *InStream, output: *OutStream) Assembler {
        return Assembler{
            .tokenizer = Tokenizer.init(allocator, input),
            .output = output,
        };
    }

    pub fn assemble(self: *Assembler) !opcode.Opcode {
        while (true) {
            const token = try self.tokenizer.next();
            defer self.tokenizer.allocator.free(token);
            const instruction = binarySearchEnumString(Instruction, token, instruction_strings[0..]).?;
            return switch (instruction) {
                Instruction.ADC => blk: {
                    const dst = try self.tokenizer.next();
                    if (!std.mem.eql(u8, dst, "a")) {
                        return Error.InvalidDestination;
                    }
                    const src = try self.tokenizer.next();
                    break :blk switch (binarySearchEnumString(EightBitOperand, src, eight_bit_operand_strings).?) {
                        EightBitOperand.A => opcode.Opcode.ADC_A_A,
                        EightBitOperand.B => opcode.Opcode.ADC_A_B,
                        EightBitOperand.C => opcode.Opcode.ADC_A_C,
                        EightBitOperand.D => opcode.Opcode.ADC_A_D,
                        EightBitOperand.E => opcode.Opcode.ADC_A_E,
                        EightBitOperand.H => opcode.Opcode.ADC_A_H,
                        EightBitOperand.L => opcode.Opcode.ADC_A_L,
                        EightBitOperand.MEM_HL => opcode.Opcode.ADC_A_HL,
                    };
                },
                else => opcode.Opcode.NOP,
            };
        }
    }
};

test "Assembler" {
    var test_assembly_file = try std.os.File.openRead(std.debug.global_allocator, "testdata/test.s");
    var test_output_file = try std.os.File.openWrite(std.debug.global_allocator, "testdata/test.bin");
    defer test_assembly_file.close();
    defer test_output_file.close();
    var inStream = std.io.FileInStream.init(&test_assembly_file);
    var outStream = std.io.FileOutStream.init(&test_output_file);
    var assembler = Assembler.init(std.debug.global_allocator, &inStream.stream, &outStream.stream);
    while (true) {
        const op = assembler.assemble() catch |err| {
            if (err == error.EndOfStream) {
                break;
            }
            return err;
        };
        std.debug.warn("op = {}\n", @tagName(op));
    }
}
