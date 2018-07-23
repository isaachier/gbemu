const std = @import("std");

const opcode = @import("opcode.zig");

fn toLower(byte: u8) u8 {
    return switch (byte) {
        'A' ... 'Z' => byte - 'A',
        else => byte,
    };
}

const Token = struct {
    id: Id,
    start: usize,
    end: usize,

    const Keyword = union(enum) {
        bytes: []const u8,
        id: Id,
    };

    const keywords = []Keyword{
        Keyword{ .bytes = "adc", .id = Id.KeywordAdc },
        Keyword{ .bytes = "add", .id = Id.KeywordAdd },
        Keyword{ .bytes = "and", .id = Id.KeywordAnd },
        Keyword{ .bytes = "bit", .id = Id.KeywordBit },
        Keyword{ .bytes = "call", .id = Id.KeywordCall },
        Keyword{ .bytes = "ccf", .id = Id.KeywordCcf },
        Keyword{ .bytes = "cp", .id = Id.KeywordCp },
        Keyword{ .bytes = "cpl", .id = Id.KeywordCpl },
        Keyword{ .bytes = "daa", .id = Id.KeywordDaa },
        Keyword{ .bytes = "dec", .id = Id.KeywordDec },
        Keyword{ .bytes = "di", .id = Id.KeywordDi },
        Keyword{ .bytes = "db", .id = Id.KeywordDb },
        Keyword{ .bytes = "dw", .id = Id.KeywordDw },
        Keyword{ .bytes = "ei", .id = Id.KeywordEi },
        Keyword{ .bytes = "endC", .id = Id.KeywordEndC },
        Keyword{ .bytes = "endM", .id = Id.KeywordEndM },
        Keyword{ .bytes = "equs", .id = Id.KeywordEqus },
        Keyword{ .bytes = "halt", .id = Id.KeywordHalt },
        Keyword{ .bytes = "inc", .id = Id.KeywordInc },
        Keyword{ .bytes = "jp", .id = Id.KeywordJp },
        Keyword{ .bytes = "jr", .id = Id.KeywordJr },
        Keyword{ .bytes = "ld", .id = Id.KeywordLd },
        Keyword{ .bytes = "ldd", .id = Id.KeywordLdd },
        Keyword{ .bytes = "ldh", .id = Id.KeywordLdh },
        Keyword{ .bytes = "ldi", .id = Id.KeywordLdi },
        Keyword{ .bytes = "macro", .id = Id.KeywordMacro },
        Keyword{ .bytes = "nop", .id = Id.KeywordNop },
        Keyword{ .bytes = "or", .id = Id.KeywordOr },
        Keyword{ .bytes = "pop", .id = Id.KeywordPop },
        Keyword{ .bytes = "push", .id = Id.KeywordPush },
        Keyword{ .bytes = "res", .id = Id.KeywordRes },
        Keyword{ .bytes = "ret", .id = Id.KeywordRet },
        Keyword{ .bytes = "rl", .id = Id.KeywordRl },
        Keyword{ .bytes = "rla", .id = Id.KeywordRla },
        Keyword{ .bytes = "rlc", .id = Id.KeywordRlc },
        Keyword{ .bytes = "rlca", .id = Id.KeywordRlca },
        Keyword{ .bytes = "rr", .id = Id.KeywordRr },
        Keyword{ .bytes = "rra", .id = Id.KeywordRra },
        Keyword{ .bytes = "rrc", .id = Id.KeywordRrc },
        Keyword{ .bytes = "rrca", .id = Id.KeywordRrca },
        Keyword{ .bytes = "rst", .id = Id.KeywordRst },
        Keyword{ .bytes = "sbc", .id = Id.KeywordSbc },
        Keyword{ .bytes = "scf", .id = Id.KeywordScf },
        Keyword{ .bytes = "set", .id = Id.KeywordSet },
        Keyword{ .bytes = "sla", .id = Id.KeywordSla },
        Keyword{ .bytes = "sra", .id = Id.KeywordSra },
        Keyword{ .bytes = "srl", .id = Id.KeywordSrl },
        Keyword{ .bytes = "stop", .id = Id.KeywordStop },
        Keyword{ .bytes = "sub", .id = Id.KeywordSub },
        Keyword{ .bytes = "swap", .id = Id.KeywordSwap },
        Keyword{ .bytes = "xor", .id = Id.KeywordXor },
    };

    const Id = union(enum) {
        Eof,
        Identifier,
        Number,
        MacroParam,
        LeftParen,
        RightParen,
        LeftBracket,
        RightBracket,
        Colon,
        Newline,
        StringLiteral,
        KeywordAdc,
        KeywordAdd,
        KeywordAnd,
        KeywordBit,
        KeywordCall,
        KeywordCcf,
        KeywordCp,
        KeywordCpl,
        KeywordDaa,
        KeywordDec,
        KeywordDi,
        KeywordDb,
        KeywordDw,
        KeywordEi,
        KeywordEndC,
        KeywordEndM,
        KeywordEqus,
        KeywordHalt,
        KeywordInc,
        KeywordJp,
        KeywordJr,
        KeywordLd,
        KeywordLdd,
        KeywordLdh,
        KeywordLdi,
        KeywordMacro,
        KeywordNop,
        KeywordOr,
        KeywordPop,
        KeywordPush,
        KeywordRes,
        KeywordRet,
        KeywordRl,
        KeywordRla,
        KeywordRlc,
        KeywordRlca,
        KeywordRr,
        KeywordRra,
        KeywordRrc,
        KeywordRrca,
        KeywordRst,
        KeywordSbc,
        KeywordScf,
        KeywordSet,
        KeywordSla,
        KeywordSra,
        KeywordSrl,
        KeywordStop,
        KeywordSub,
        KeywordSwap,
        KeywordXor,
    };
};

const Tokenizer = struct {
    const State = enum {
        Default,
        StringLiteral,
        Identifier,
        Newline,
    };

    buffer: []const u8,
    index: usize,
    pending_invalid_token: ?Token,

    pub fn init(buffer: []const u8) Tokenizer {
        return Tokenizer{
            .buffer = buffer,
            .index = 0,
            .pending_invalid_token = null,
        };
    }

    pub fn next(self: *Tokenizer) !Token {
        if (self.pending_invalid_token) |token| {
            self.pending_invalid_token = null;
            return token;
        }
        const start_index = self.index;
        var state = State.Default;
        var result = Token{
            .id = Token.Id.Eof,
            .start = self.index,
            .end = undefined,
        };
        while (self.index < self.buffer.len) : (self.index += 1) {
            const c = self.buffer[self.index];
            switch (state) {
                State.Default => {
                    switch (c) {
                        '(' => {
                            result.id = Token.Id.LeftParen;
                            break;
                        },
                        ')' => {
                            result.id = Token.Id.RightParen;
                            break;
                        },
                        '[' => {
                            result.id = Token.Id.LeftBracket;
                            break;
                        },
                        ']' => {
                            result.id = Token.Id.RightBracket;
                            break;
                        },
                        ':' => {
                            result.id = Token.Id.Colon;
                            break;
                        },
                        ' ', '\t' => {
                            result.start = self.index + 1;
                        },
                        '"' => {
                            result.id = Token.Id.StringLiteral;
                            state = State.StringLiteral;
                        },
                        std.cstr.line_sep[0] => {
                            if (std.cstr.line_sep.len == 1) {
                                result.id = Token.Id.Newline;
                                break;
                            }
                            state = State.Newline;
                        },
                    }
                },
                State.StringLiteral => {
                },
                State.Identifier => {},
                State.Newline => {},
            }
        }
    }
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
    tokenizer: Tokenizer,

    pub fn init(buffer: []const u8) Assembler {
        return Assembler{
            .tokenizer = Tokenizer.init(buffer),
        };
    }

    pub fn assemble(self: *Assembler) !opcode.Opcode {
        // TODO
        try self.tokenizer.next();
        return opcode.Opcode.NOP;
    }
};

test "Assembler" {
    var test_assembly_file = try std.os.File.openRead(std.debug.global_allocator, "testdata/test.s");
    var test_output_file = try std.os.File.openWrite(std.debug.global_allocator, "testdata/test.bin");
    defer test_assembly_file.close();
    defer test_output_file.close();
    var inStream = std.io.FileInStream.init(&test_assembly_file);
    var outStream = std.io.FileOutStream.init(&test_output_file);
    var assembler = Assembler.init("ld a,1");
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
