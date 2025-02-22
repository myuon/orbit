const std = @import("std");
const compiler = @import("compiler.zig");
const ast = @import("ast.zig");
const tui = @import("tui.zig");
const vm = @import("vm.zig");
const vaxis = @import("vaxis");
const P = @import("profiler");

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

fn writeIr(prog: []ast.Instruction, offset: usize, buffer: *std.ArrayList(u8)) anyerror!void {
    for (prog, 0..) |inst, k| {
        try std.fmt.format(buffer.writer(), "{d}: ", .{k + offset});
        try std.json.stringify(inst, .{}, buffer.writer());
        try buffer.appendSlice("\n");
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

        try P.init(.{});
        defer {
            P.dump("profile.json") catch |err| {
                std.log.err("Failed to dump profile: {any}\n", .{err});
            };
            P.deinit();
        }

        const zone = P.begin(@src(), "main.run");
        defer zone.end();

        var content = std.ArrayList(u8).init(allocator);
        defer content.deinit();

        try readFile(allocator, argv[2][0..std.mem.len(argv[2])], &content);

        const result = try c.evalModule(content.items, .{ .enable_jit = enableJit });

        try stdout.print("Result: {any}\n", .{result});
        try bw.flush();
    } else if (std.mem.eql(u8, command, "dbg")) {
        var breakpoint: i32 = -1;
        for (argv[2..], 0..) |arg, k| {
            if (std.mem.eql(u8, arg[0..std.mem.len(arg)], "--breakpoint")) {
                breakpoint = try std.fmt.parseInt(i32, argv[k + 3][0..std.mem.len(argv[k + 3])], 10);
            }
        }

        var content = std.ArrayList(u8).init(allocator);
        defer content.deinit();

        try readFile(allocator, argv[2][0..std.mem.len(argv[2])], &content);

        const prog = try c.compileInIr(content.items);

        var vmr = vm.VmRuntime.init(arena_allocator.allocator());
        defer vmr.deinit();

        var stack = std.ArrayList(i64).init(allocator);
        defer stack.deinit();
        defer {
            std.debug.print("stack: {any} (ip: {d})\n", .{ stack.items, vmr.pc });
        }

        try stack.append(-2); // return value
        try stack.append(-1); // pc
        try stack.append(0); // bp

        var bp: i64 = @intCast(stack.items.len);

        var dbg = try tui.Tui.init(allocator);
        defer dbg.deinit();

        try dbg.start();

        var progStack = std.ArrayList(u8).init(allocator);
        defer progStack.deinit();
        try writeIr(prog, 0, &progStack);

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
                            const result = try vmr.step(prog, &stack, &bp);
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
                            try writeIr(prog[@intCast(scroll)..], @intCast(scroll), &progStack);

                            try dbg.set_text("ir", progStack.items);
                        } else if (key.matches(vaxis.Key.down, .{})) {
                            scroll += 1;
                            if (scroll >= prog.len) {
                                scroll = @intCast(prog.len - 1);
                            }
                            if (scroll < 0) {
                                scroll = 0;
                            }

                            progStack.clearAndFree();
                            try writeIr(prog[@intCast(scroll)..], @intCast(scroll), &progStack);

                            try dbg.set_text("ir", progStack.items);
                        }
                    },
                    else => {},
                }
            }

            if (mode_resume) {
                const result = try vmr.step(prog, &stack, &bp);
                if (result.isTerminated()) {
                    break;
                }
                if (@as(i32, @intCast(vmr.pc)) == breakpoint) {
                    mode_resume = false;
                }
            }

            var draw_allocator_arena = std.heap.ArenaAllocator.init(allocator);
            const draw_allocator = draw_allocator_arena.allocator();
            defer draw_allocator_arena.deinit();

            var next = std.ArrayList(u8).init(draw_allocator);
            try std.json.stringify(prog[vmr.pc], .{}, next.writer());

            var adm = std.ArrayList(u8).init(draw_allocator);
            defer adm.deinit();

            const s = try writeStack(draw_allocator, stack.items, bp);

            var memory = std.ArrayList(u8).init(draw_allocator);
            defer memory.deinit();

            for (vmr.memory, 0..) |v, i| {
                const p = try std.fmt.allocPrint(draw_allocator, "{x:0>2} ", .{v});
                try memory.appendSlice(p);

                if (i % 16 == 16 - 1) {
                    try memory.appendSlice("\n");
                }
                if (i > 1024) {
                    break;
                }
            }

            const k = try std.fmt.allocPrint(draw_allocator,
                \\pc: {d}, bp: {d}, next: {s}
                \\address_map: {s}
                \\stack: {s}
                \\memory:
                \\{s}
            , .{ vmr.pc, bp, next.items, adm.items, s, memory.items });

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
