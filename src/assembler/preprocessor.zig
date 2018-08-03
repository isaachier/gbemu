const std = @import("std");

pub const Preprocessor = struct {
    buffer: []const u8,
    index: usize,

    const State = enum {
        Default,
        SawM,
        SawA,
        SawC,
        SawR,
        Macro,
    };

    const ErrorSet = error{
        EndOfFile,
        MacroMissingENDM,
    };

    pub fn init(buffer: []const u8) Preprocessor {
        return Preprocessor{
            .buffer = buffer,
            .index = 0,
        };
    }

    pub fn nextByte(self: *Preprocessor) ErrorSet!u8 {
        var state = State.Default;
        while (self.index < self.buffer.len) : (self.index += 1) {
            const byte = self.buffer[self.index];
            switch (state) {
                State.Default => {
                    switch (byte) {
                        'M' => {
                            state = State.SawM;
                        },
                        else => {
                            self.index += 1;
                            return byte;
                        },
                    }
                },
                State.SawM => {
                    switch (byte) {
                        'A' => {
                            state = State.SawA;
                        },
                        else => {
                            return 'M';
                        }
                    }
                },
                State.SawA => {
                    switch (byte) {
                        'C' => {
                            state = State.SawC;
                        },
                        else => {
                            self.index -= 1;
                            return 'M';
                        },
                    }
                },
                State.SawC => {
                    switch (byte) {
                        'R' => {
                            state = State.SawR;
                        },
                        else => {
                            self.index -= 2;
                            return 'M';
                        },
                    }
                },
                State.SawR => {
                    switch (byte) {
                        'R' => {
                            state = State.Macro;
                        },
                        else => {
                            self.index -= 3;
                            return 'M';
                        },
                    }
                },
                State.Macro => {
                    // TODO
                    return u8(0);
                },
            }
        }

        switch (state) {
            State.Default => {
                return error.EndOfFile;
            },
            State.SawM => {
                return 'M';
            },
            State.SawA => {
                self.index -= 1;
                return 'M';
            },
            State.SawC => {
                self.index -= 2;
                return 'M';
            },
            State.SawR => {
                self.index -= 3;
                return 'M';
            },
            State.Macro => {
                // TODO
                return ErrorSet.MacroMissingENDM;
            },
        }
    }
};

test "Preprocessor works" {
    const inputs = []const[]const u8{
        "x MACR",
        "x MAC",
        "x MA",
        "x M",
        "x MACARONI",
        "x",
    };

    for (inputs) |input| {
        var pp = Preprocessor.init(input);
        for (input) |byte| {
            std.debug.assert((try pp.nextByte()) == byte);
        }
    }
}
