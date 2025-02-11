const std = @import("std");
const compiler = @import("compiler.zig");
const tui = @import("tui.zig");
const vm = @import("vm.zig");

fn readFile(allocator: std.mem.Allocator, path: []const u8, content: *std.ArrayList(u8)) anyerror!void {
    var file = try std.fs.cwd().openFile(path, .{});
    defer file.close();

    while (try file.reader().readUntilDelimiterOrEofAlloc(allocator, '\n', std.math.maxInt(usize))) |line| {
        defer allocator.free(line);

        try content.appendSlice(line);
    }
}

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
        var content = std.ArrayList(u8).init(allocator);
        defer content.deinit();

        try readFile(allocator, argv[2][0..std.mem.len(argv[2])], &content);

        const result = try c.evalModule(content.items);

        try stdout.print("Result: {any}\n", .{result});
        try bw.flush();
    } else if (std.mem.eql(u8, command, "dbg")) {
        var content = std.ArrayList(u8).init(allocator);
        defer content.deinit();

        try readFile(allocator, argv[2][0..std.mem.len(argv[2])], &content);

        const prog = try c.compileInIr(content.items);

        var stack = std.ArrayList(i64).init(allocator);
        defer stack.deinit();

        var address_map = std.StringHashMap(i64).init(allocator);
        defer address_map.deinit();

        try address_map.put("return", -2 - 1);
        try stack.append(-2); // return value
        try stack.append(-1); // pc
        try stack.append(0); // bp

        var bp: i64 = @intCast(stack.items.len);

        var vmc = vm.Vm.init(allocator);

        var dbg = try tui.Tui.init(allocator);
        defer dbg.deinit();

        try dbg.start();

        while (true) {
            const event = try dbg.fetchEvent();
            if (!try dbg.handle_event(event)) break;

            switch (event) {
                .key_press => |key| {
                    if (key.matches('n', .{})) {
                        try vmc.step(prog, &stack, &bp, &address_map);

                        std.debug.print(".{any}\n", .{stack.items});
                    }
                },
                else => {},
            }

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
