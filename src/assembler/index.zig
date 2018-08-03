const std = @import("std");

const opcode = @import("../opcode.zig");
const tokenizer = @import("tokenizer.zig");
const Token = tokenizer.Token;
const Tokenizer = tokenizer.Tokenizer;

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

    pub fn init(allocator: *std.mem.Allocator, buffer: []const u8) Assembler {
        return Assembler{
            .tokenizer = Tokenizer.init(buffer),
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
                            label = token;
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
