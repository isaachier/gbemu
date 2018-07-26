const std = @import("std");

const opcode = @import("opcode.zig");

fn toLower(byte: u8) u8 {
    return switch (byte) {
        'A' ... 'Z' => byte - 'A' + 'a',
        else => byte,
    };
}

const Token = struct {
    id: Id,
    start: usize,
    end: usize,

    const Keyword = struct {
        bytes: []const u8,
        id: Id,
    };

    const keywords = []const Keyword{
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
        Keyword{ .bytes = "endc", .id = Id.KeywordEndC },
        Keyword{ .bytes = "endm", .id = Id.KeywordEndM },
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
        Keyword{ .bytes = "rept", .id = Id.KeywordRept },
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

    const Id = enum {
        Invalid,
        Eof,
        Identifier,
        IntLiteral,
        LeftParen,
        RightParen,
        LeftBracket,
        RightBracket,
        Comma,
        Colon,
        Newline,
        Comment,
        StringLiteral,
        MacroArgument,
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
        KeywordRept,
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

    pub fn str(self: *const Token, input: []const u8) []const u8 {
        return input[self.start..self.end];
    }
};

const Tokenizer = struct {
    const State = enum {
        Default,
        IntLiteral,
        StringLiteral,
        EscapeSequence,
        Identifier,
        Newline,
        Comment,
    };

    buffer: []const u8,
    index: usize,

    pub fn init(buffer: []const u8) Tokenizer {
        return Tokenizer{
            .buffer = buffer,
            .index = 0,
        };
    }

    pub fn next(self: *Tokenizer) Token {
        const start_index = self.index;
        var state = State.Default;
        var result = Token{
            .id = Token.Id.Eof,
            .start = self.index,
            .end = self.index,
        };
        while (self.index < self.buffer.len) : (self.index += 1) {
            const c = self.buffer[self.index];
            switch (state) {
                State.Default => {
                    switch (c) {
                        '$' => {
                            result.id = Token.Id.IntLiteral;
                            state = State.IntLiteral;
                        },
                        '(' => {
                            result.id = Token.Id.LeftParen;
                            self.index += 1;
                            break;
                        },
                        ')' => {
                            result.id = Token.Id.RightParen;
                            self.index += 1;
                            break;
                        },
                        '[' => {
                            result.id = Token.Id.LeftBracket;
                            self.index += 1;
                            break;
                        },
                        ']' => {
                            result.id = Token.Id.RightBracket;
                            self.index += 1;
                            break;
                        },
                        ',' => {
                            result.id = Token.Id.Comma;
                            self.index += 1;
                            break;
                        },
                        ':' => {
                            result.id = Token.Id.Colon;
                            self.index += 1;
                            break;
                        },
                        '\\' => {
                            result.id = Token.Id.MacroArgument;
                            state = State.IntLiteral;
                        },
                        ' ', '\t' => {
                            result.start = self.index + 1;
                        },
                        '"' => {
                            result.id = Token.Id.StringLiteral;
                            state = State.StringLiteral;
                        },
                        ';' => {
                            result.id = Token.Id.Comment;
                            state = State.Comment;
                        },
                        std.cstr.line_sep[0] => {
                            if (std.cstr.line_sep.len == 1) {
                                result.id = Token.Id.Newline;
                                self.index += 1;
                                break;
                            }
                            state = State.Newline;
                        },
                        'a' ... 'z', 'A' ... 'Z', '_', '.' => {
                            result.id = Token.Id.Identifier;
                            state = State.Identifier;
                        },
                        else => {
                            result.id = Token.Id.Invalid;
                            break;
                        },
                    }
                },
                State.IntLiteral => {
                    switch (c) {
                        '0' ... '9', 'A' ... 'F', 'a' ... 'f'  => {},
                        else => {
                            if (result.start == self.index - 1) {
                                result.id = Token.Id.Invalid;
                            }
                            break;
                        },
                    }
                },
                State.StringLiteral => {
                    switch (c) {
                        '"' => {
                            self.index += 1;
                            break;
                        },
                        '\\' => {
                            state = State.EscapeSequence;
                        },
                        else => {},
                    }
                },
                State.EscapeSequence => {
                    switch (c) {
                        'i', 'p', 'f', 'b', 'I', 'o', 'a', 't', 'P', 'C', '\\', '\'', '"', '0' => {
                            state = State.StringLiteral;
                        },
                        else => {
                            result.id = Token.Id.Invalid;
                            break;
                        },
                    }
                },
                State.Identifier => {
                    switch (c) {
                        'A' ... 'Z', 'a' ... 'z', '0' ... '9', '_' => {},
                        else => {
                            const str = self.buffer[result.start..self.index];
                            var lower: [256]u8 = undefined;
                            for (str) |byte, i| {
                                lower[i] = toLower(byte);
                            }
                            if (findKeyword(lower[0..str.len])) |keyword| {
                                result.id = keyword.id;
                            }
                            break;
                        },
                    }
                },
                State.Newline => {
                    std.debug.assert(std.cstr.line_sep.len == 2);
                    switch (c) {
                        std.cstr.line_sep[std.cstr.line_sep.len-1] => {
                            self.index += 1;
                            break;
                        },
                        else => {
                            result.id = Token.Id.Invalid;
                            break;
                        },
                    }
                },
                State.Comment => {
                    if (c == std.cstr.line_sep[0]) {
                        self.index += 1;
                        break;
                    }
                },
            }
        }

        result.end = self.index;
        if (self.index == self.buffer.len) {
            switch (state) {
                State.EscapeSequence => {
                    result.id = Token.Id.Invalid;
                },
                else => {},
            }
        }

        return result;
    }

    fn findKeyword(str: []const u8) ?Token.Keyword {
        var first: usize = 0;
        var last: usize = Token.keywords.len;
        var pos: usize = undefined;
        var count: usize = Token.keywords.len;
        var step: usize = undefined;
        while (count > 0) {
            pos = first;
            step = count / 2;
            pos += step;
            if (std.mem.lessThan(u8, Token.keywords[pos].bytes, str)) {
                pos += 1;
                first = pos;
                count -= step + 1;
            } else {
                count = step;
            }
        }
        if (std.mem.eql(u8, Token.keywords[first].bytes, str)) {
            return Token.keywords[first];
        }
        return null;
    }
};

pub const Macro = struct {
    tokens: []const Token,
    num_args: usize,

    pub fn init(input: []const u8, tokens: []const Token) !Macro {
        var num_args: usize = 0;
        for (tokens) |token| {
            if (token.id == Token.Id.MacroArgument) {
                const arg_num = try std.fmt.parseUnsigned(usize, input[token.start+1..token.end], 10);
                if (num_args < arg_num) {
                    num_args = arg_num;
                }
            }
        }
        return Macro{
            .tokens = tokens,
            .num_args = num_args,
        };
    }
};

pub const Assembler = struct {
    const MacroMap = std.HashMap([]const u8, Macro, std.mem.hash_slice_u8, std.mem.eql_slice_u8);

    const State = enum {
        Default,
        Identifier,
        Label,
        Macro,
    };

    tokenizer: Tokenizer,
    macros: MacroMap,

    pub fn init(allocator: *std.mem.Allocator, input: []const u8) Assembler {
        return Assembler{
            .tokenizer = Tokenizer.init(input),
            .macros = MacroMap.init(allocator),
        };
    }

    pub fn deinit(self: *Assembler) void {
        var it = self.macros.iterator();
        while (it.next()) |entry| {
            self.macros.allocator.free(entry.key);
        }
        self.macros.deinit();
    }

    pub fn assemble(self: *Assembler) !opcode.Opcode {
        var state = State.Default;
        var label: Token = undefined;
        var tokens = std.ArrayList(Token).init(self.macros.allocator);
        defer tokens.deinit();
        while (true) {
            const token = self.tokenizer.next();
            switch (token.id) {
                Token.Id.Invalid => {
                    std.debug.warn("Invalid token\n");
                    break;
                },
                Token.Id.Eof => {
                    std.debug.warn("EOF\n");
                    break;
                },
                Token.Id.Comment => {
                    continue;
                },
                else => {},
            }

            switch (state) {
                State.Default => {
                    switch (token.id) {
                        Token.Id.Identifier => {
                            state = State.Identifier;
                        },
                        // TODO: Other cases
                        else => {
                        },
                    }
                },
                State.Identifier => {
                    switch (token.id) {
                        Token.Id.Colon => {
                            state = State.Label;
                            label = token;
                        },
                        // TODO: Other cases
                        else => {
                            state = State.Default;
                        },
                    }
                },
                State.Label => {
                    switch (token.id) {
                        Token.Id.KeywordMacro => {
                            state = State.Macro;
                            tokens = std.ArrayList(Token).init(self.macros.allocator);
                        },
                        else => {
                            state = State.Default;
                        },
                    }
                },
                State.Macro => {
                    switch (token.id) {
                        Token.Id.KeywordEndM => {
                            state = State.Default;
                            _ = try self.macros.put(label.str(self.tokenizer.buffer),
                                                    try Macro.init(self.tokenizer.buffer,
                                                                   tokens.toOwnedSlice()));
                        },
                        else => {},
                    }
                },
            }
            std.debug.warn("{}", @tagName(token.id));
            if (token.id != Token.Id.Newline) {
                std.debug.warn(" '{}'", token.str(self.tokenizer.buffer));
            }
            std.debug.warn("\n");
        }
        return opcode.Opcode.NOP;
    }
};

test "Assembler" {
    const contents = try std.io.readFileAlloc(std.debug.global_allocator, "testdata/test.s");
    defer std.debug.global_allocator.free(contents);
    var assembler = Assembler.init(std.debug.global_allocator, contents);
    defer assembler.deinit();
    const op = assembler.assemble();
}
