const std = @import("std");

pub fn build(b: *std.Build) void {
    _ = b.addModule("xzb", .{
        .root_source_file = b.path("xzb.zig"),
    });
}
