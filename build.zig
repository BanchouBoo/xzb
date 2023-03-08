const std = @import("std");

pub fn build(b: *std.Build) void {
    _ = b.addModule("xzb", .{
        .source_file = std.Build.FileSource.relative("xzb.zig"),
        .dependencies = &[_]std.Build.ModuleDependency{},
    });
}
