const std = @import("std");
const compiler = @import("compiler.zig");
const debugger = @import("debugger.zig");

pub fn main() !void {
    var gpallocator = std.heap.GeneralPurposeAllocator(.{}){};
    defer {
        switch (gpallocator.deinit()) {
            .leak => {
                std.debug.print("**Leaked memory**", .{});
            },
            else => {},
        }
    }

    const allocator = gpallocator.allocator();

    var c = compiler.Compiler.init(allocator);
    defer c.deinit();

    const argv = std.os.argv;
    if (argv.len < 2) {
        return std.debug.print("Usage: orbit [run, dbg] <file>\n", .{});
    }

    const stdout_file = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout_file);
    const stdout = bw.writer();

    const command = argv[1][0..std.mem.len(argv[1])];
    if (std.mem.eql(u8, command, "run")) {
        var file = try std.fs.cwd().openFile(argv[2][0..std.mem.len(argv[2])], .{});
        defer file.close();

        var content = std.ArrayList(u8).init(allocator);
        defer content.deinit();
        while (try file.reader().readUntilDelimiterOrEofAlloc(allocator, '\n', std.math.maxInt(usize))) |line| {
            defer allocator.free(line);

            try content.appendSlice(line);
        }

        const result = try c.evalModule(content.items);

        try stdout.print("Result: {any}\n", .{result});
        try bw.flush();
    } else if (std.mem.eql(u8, command, "dbg")) {
        var dbg = try debugger.Debugger.init(allocator);
        defer dbg.deinit();

        try dbg.start();

        while (true) {
            const event = try dbg.fetchEvent();
            if (!try dbg.handle_event(event)) break;

            try dbg.draw();
        }
    } else {
        return std.debug.print("Unknown command: {s}", .{command});
    }
}

test "simple test" {
    var list = std.ArrayList(i32).init(std.testing.allocator);
    defer list.deinit(); // try commenting this out and see if zig detects the memory leak!
    try list.append(42);
    try std.testing.expectEqual(@as(i32, 42), list.pop());
}

test {
    _ = @import("utils.zig");
    _ = @import("ast.zig");
    _ = @import("compiler.zig");
}
