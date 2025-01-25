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
