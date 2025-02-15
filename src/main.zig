const std = @import("std");
const compiler = @import("compiler.zig");
const tui = @import("tui.zig");
const vm = @import("vm.zig");
const vaxis = @import("vaxis");

pub const panic = vaxis.panic_handler;

fn readFile(allocator: std.mem.Allocator, path: []const u8, content: *std.ArrayList(u8)) anyerror!void {
    var file = try std.fs.cwd().openFile(path, .{});
    defer file.close();

    while (try file.reader().readUntilDelimiterOrEofAlloc(allocator, '\n', std.math.maxInt(usize))) |line| {
        defer allocator.free(line);

        try content.appendSlice(line);
    }
}

fn writeStack(allocator: std.mem.Allocator, stack: []i64, bp: i64) ![]u8 {
    var stack_frames = std.AutoHashMap(i64, bool).init(allocator);
    defer stack_frames.deinit();

    var b = bp;
    while (b >= 2 and b <= stack.len) {
        try stack_frames.put(b - 1, true);
        b = stack[@intCast(b - 1)];
    }

    var result = std.ArrayList(u8).init(allocator);

    const h = try std.fmt.allocPrint(allocator, "stack <#{d}>\n", .{stack.len});
    defer allocator.free(h);
    try result.appendSlice(h);

    for (0..stack.len) |ri| {
        const i = stack.len - ri - 1;

        const p = try std.fmt.allocPrint(allocator, "#{d} | {d}", .{ i, stack[i] });
        try result.appendSlice(p);

        if (stack_frames.contains(@intCast(i + 1))) {
            try result.appendSlice(" (pc)");
        }
        if (stack_frames.contains(@intCast(i))) {
            try result.appendSlice(" === stackframe");
        }

        try result.appendSlice("\n");
    }

    return result.items;
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
    var arena_allocator = std.heap.ArenaAllocator.init(allocator);
    defer arena_allocator.deinit();

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
        var enableJit = true;
        for (argv[2..]) |arg| {
            if (std.mem.eql(u8, arg[0..std.mem.len(arg)], "--nojit")) {
                enableJit = false;
            }
        }

        var content = std.ArrayList(u8).init(allocator);
        defer content.deinit();

        try readFile(allocator, argv[2][0..std.mem.len(argv[2])], &content);

        const result = try c.evalModule(content.items, .{ .enable_jit = enableJit });

        try stdout.print("Result: {any}\n", .{result});
        try bw.flush();
    } else if (std.mem.eql(u8, command, "dbg")) {
        var content = std.ArrayList(u8).init(allocator);
        defer content.deinit();

        try readFile(allocator, argv[2][0..std.mem.len(argv[2])], &content);

        const prog = try c.compileInIr(content.items);

        var stack = std.ArrayList(i64).init(allocator);
        defer stack.deinit();
        defer {
            std.debug.print("stack: {any}\n", .{stack.items});
        }

        var address_map = std.StringHashMap(i64).init(allocator);
        defer address_map.deinit();

        try address_map.put("return", -2 - 1);
        try stack.append(-2); // return value
        try stack.append(-1); // pc
        try stack.append(0); // bp

        var bp: i64 = @intCast(stack.items.len);

        var vmc = vm.VmRuntime.init(arena_allocator.allocator());
        defer vmc.deinit();

        var dbg = try tui.Tui.init(allocator);
        defer dbg.deinit();

        try dbg.start();

        var progStack = std.ArrayList(u8).init(allocator);
        defer progStack.deinit();
        for (prog) |inst| {
            var next = std.ArrayList(u8).init(allocator);
            defer next.deinit();
            try std.json.stringify(inst, .{}, next.writer());

            try progStack.appendSlice(next.items);
            try progStack.appendSlice("\n");
        }

        try dbg.set_text("ir", progStack.items);
        try dbg.set_text("stack", "");

        var scroll: i32 = 0;

        var mode_resume = false;

        while (true) {
            if (!mode_resume) {
                const event = try dbg.fetchEvent();
                if (!try dbg.handle_event(event)) break;

                switch (event) {
                    .key_press => |key| {
                        if (key.matches('n', .{})) {
                            const result = try vmc.step(prog, &stack, &bp, &address_map);
                            if (result.isTerminated()) {
                                break;
                            }
                        } else if (key.matches('k', .{})) {
                            mode_resume = true;
                        } else if (key.matches(vaxis.Key.up, .{})) {
                            scroll -= 1;
                            if (scroll < 0) {
                                scroll = 0;
                            }

                            progStack.clearAndFree();
                            for (prog[@intCast(scroll)..]) |inst| {
                                var next = std.ArrayList(u8).init(allocator);
                                defer next.deinit();
                                try std.json.stringify(inst, .{}, next.writer());

                                try progStack.appendSlice(next.items);
                                try progStack.appendSlice("\n");
                            }

                            try dbg.set_text("ir", progStack.items);
                        } else if (key.matches(vaxis.Key.down, .{})) {
                            scroll += 1;

                            progStack.clearAndFree();
                            for (prog[@intCast(scroll)..]) |inst| {
                                var next = std.ArrayList(u8).init(allocator);
                                defer next.deinit();
                                try std.json.stringify(inst, .{}, next.writer());

                                try progStack.appendSlice(next.items);
                                try progStack.appendSlice("\n");
                            }

                            try dbg.set_text("ir", progStack.items);
                        }
                    },
                    else => {},
                }
            }

            if (mode_resume) {
                const result = try vmc.step(prog, &stack, &bp, &address_map);
                if (result.isTerminated()) {
                    break;
                }
            }

            var draw_allocator_arena = std.heap.ArenaAllocator.init(allocator);
            const draw_allocator = draw_allocator_arena.allocator();
            defer draw_allocator_arena.deinit();

            var next = std.ArrayList(u8).init(draw_allocator);
            try std.json.stringify(prog[vmc.pc], .{}, next.writer());

            var adm = std.ArrayList(u8).init(draw_allocator);
            defer adm.deinit();

            var iter = address_map.iterator();
            while (iter.next()) |entry| {
                try adm.appendSlice(entry.key_ptr.*);
                try adm.appendSlice(" -> ");
                try adm.appendSlice(try std.fmt.allocPrint(draw_allocator, "{d}", .{entry.value_ptr.*}));
                try adm.appendSlice(" (");

                const v = bp + entry.value_ptr.*;
                if (v >= 0 and v < stack.items.len) {
                    const t = try std.fmt.allocPrint(draw_allocator, "{d}", .{stack.items[@intCast(v)]});
                    try adm.appendSlice(t);
                }
                try adm.appendSlice(")");
                try adm.appendSlice("\n");
            }

            const s = try writeStack(draw_allocator, stack.items, bp);
            const k = try std.fmt.allocPrint(draw_allocator,
                \\pc: {d}, bp: {d}, next: {s}
                \\address_map: {s}
                \\stack: {s}
            , .{ vmc.pc, bp, next.items, adm.items, s });

            try dbg.set_text("stack", k);

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
