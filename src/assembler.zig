const std = @import("std");

pub const Assembler = struct {
    const InStream = std.io.InStream(std.os.File.ReadError);
    const OutStream = std.io.OutStream(std.os.File.WriteError);

    allocator: *std.mem.Allocator,
    input: *InStream,
    output: *OutStream,

    pub fn init(allocator: *std.mem.Allocator, input: *InStream, output: *OutStream) Assembler {
        return Assembler{
            .allocator = allocator,
            .input = input,
            .output = output,
        };
    }

    pub fn assemble(self: *Assembler) !void {
        var buffer = std.Buffer.initNull(self.allocator);
        defer buffer.deinit();
        const len = try self.input.readUntilDelimiterBuffer(&buffer, '\n', 256);
        var iter = std.mem.split(buffer.toSliceConst(), " \t,");
        while (iter.next()) |token| {
            std.debug.warn("{}\n", token);
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
        assembler.assemble() catch |err| {
            if (err == error.EndOfStream) {
                break;
            } else {
                return err;
            }
        };
    }
}
