const std = @import("std");
const Builder = std.build.Builder;

pub fn build(b: *Builder) void {
    const mode = b.standardReleaseOptions();
    const exe = b.addExecutable("gbemu", "src/main.zig");
    exe.addLibPath("/usr/lib/x86_64-linux-gnu");
    exe.linkSystemLibrary("c");
    exe.linkSystemLibrary("SDL2");
    exe.setBuildMode(mode);

    const run_cmd = exe.run();

    const run_step = b.step("run", "Run gbemu");
    run_step.dependOn(&run_cmd.step);

    b.default_step.dependOn(&exe.step);
    b.installArtifact(exe);
}
