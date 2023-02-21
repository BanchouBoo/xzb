const std = @import("std");

pub fn build(b: *std.Build) void {
    b.addModule(.{
        .name = "xzb",
        .source_file = std.Build.FileSource.relative("xzb.zig"),
        .dependencies = &[_]std.Build.ModuleDependency{},
    });
}
