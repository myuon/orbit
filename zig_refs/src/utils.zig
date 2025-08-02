const std = @import("std");

pub fn startsWith(str: []const u8, prefix: []const u8) bool {
    if (prefix.len > str.len) {
        return false;
    }

    for (prefix, 0..) |prefix_ch, i| {
        if (str[i] != prefix_ch) {
            return false;
        }
    }

    return true;
}

test "startsWith" {
    try std.testing.expect(startsWith("hello world", "hello"));
    try std.testing.expect(!startsWith("hello world", "world"));
}

pub fn readFile(allocator: std.mem.Allocator, path: []const u8, content: *std.ArrayList(u8)) anyerror!void {
    var file = try std.fs.cwd().openFile(path, .{});
    defer file.close();

    while (try file.reader().readUntilDelimiterOrEofAlloc(allocator, '\n', std.math.maxInt(usize))) |line| {
        defer allocator.free(line);

        try content.appendSlice(line);
        try content.appendSlice("\n");
    }
}

pub fn Positioned(comptime T: type) type {
    return struct {
        position: usize,
        data: T,
    };
}
