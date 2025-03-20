const std = @import("std");
const compiler = @import("compiler.zig");
const ast = @import("ast.zig");
const tui = @import("tui.zig");
const vm = @import("vm.zig");
const runtime = @import("runtime.zig");
const vaxis = @import("vaxis");
const P = @import("profiler");
const utils = @import("utils.zig");

pub const panic = vaxis.panic_handler;

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

    try P.init(.{});
    defer {
        P.dump("profile.json") catch |err| {
            std.log.err("Failed to dump profile: {any}\n", .{err});
        };
        P.deinit();
    }

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
        var enableOptimizeIr = true;
        var dumpIr = false;
        var dumpMonoAst = false;
        var dumpDesugaredAst = false;
        for (argv[2..]) |arg| {
            if (std.mem.eql(u8, arg[0..std.mem.len(arg)], "--nojit")) {
                enableJit = false;
            } else if (std.mem.eql(u8, arg[0..std.mem.len(arg)], "--dump-ir")) {
                dumpIr = true;
            } else if (std.mem.eql(u8, arg[0..std.mem.len(arg)], "--noopt")) {
                enableOptimizeIr = false;
            } else if (std.mem.eql(u8, arg[0..std.mem.len(arg)], "--dump-mono-ast")) {
                dumpMonoAst = true;
            } else if (std.mem.eql(u8, arg[0..std.mem.len(arg)], "--dump-desugared-ast")) {
                dumpDesugaredAst = true;
            }
        }

        if (dumpIr) {
            c.dump_ir_path = "out/dumped.ir";
        }
        if (dumpMonoAst) {
            c.dump_mono_ast_path = "out/dumped_mono_ast.ast";
        }
        if (dumpDesugaredAst) {
            c.dump_desugared_ast_path = "out/dumped_desugared_ast.ast";
        }

        const zone = P.begin(@src(), "main.run");
        defer zone.end();

        var content = std.ArrayList(u8).init(allocator);
        defer content.deinit();

        try utils.readFile(allocator, argv[2][0..std.mem.len(argv[2])], &content);

        c.enable_jit = enableJit;
        c.enable_optimize_ir = enableOptimizeIr;

        try c.compile(content.items, true);

        try stdout.print("Result: {any}\n", .{c.result});
        try bw.flush();
    } else if (std.mem.eql(u8, command, "dbg")) {
        var breakpoint: i32 = -1;
        var breakpoint_label: ?[]const u8 = null;
        for (argv[2..], 0..) |arg, k| {
            if (std.mem.eql(u8, arg[0..std.mem.len(arg)], "--breakpoint-at")) {
                breakpoint = try std.fmt.parseInt(i32, argv[k + 3][0..std.mem.len(argv[k + 3])], 10);
            } else if (std.mem.eql(u8, arg[0..std.mem.len(arg)], "--breakpoint")) {
                breakpoint_label = argv[k + 3][0..std.mem.len(argv[k + 3])];
            }
        }

        var content = std.ArrayList(u8).init(allocator);
        defer content.deinit();

        try utils.readFile(allocator, argv[2][0..std.mem.len(argv[2])], &content);

        try c.compile(content.items, false);
        const prog = c.ir.?;

        var vmr = runtime.VmRuntime.init(arena_allocator.allocator());
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
        try dbg.set_text("memory", "");

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
                } else if (breakpoint_label != null) {
                    switch (prog[vmr.pc]) {
                        .label => |label| {
                            if (std.mem.eql(u8, label, breakpoint_label.?)) {
                                mode_resume = false;
                            }
                        },
                        else => {},
                    }
                }
            }

            var draw_allocator_arena = std.heap.ArenaAllocator.init(allocator);
            const draw_allocator = draw_allocator_arena.allocator();
            defer draw_allocator_arena.deinit();

            var next = std.ArrayList(u8).init(draw_allocator);
            try std.json.stringify(prog[vmr.pc], .{}, next.writer());

            const s = try writeStack(draw_allocator, stack.items, bp);

            var memory = std.ArrayList(u8).init(draw_allocator);
            defer memory.deinit();

            try memory.appendSlice("section:meta [0-24]\n");
            const gsp = vmr.loadMemory(8, vm.global_section_ptr);
            try std.fmt.format(memory.writer(), "  global_section_ptr: {x:0>2}\n", .{gsp});
            const dsp = vmr.loadMemory(8, vm.data_section_ptr);
            try std.fmt.format(memory.writer(), "  data_section_ptr: {x:0>2}\n", .{dsp});
            const hsp = vmr.loadMemory(8, vm.heap_section_ptr);
            try std.fmt.format(memory.writer(), "  heap_section_ptr: {x:0>2}\n", .{hsp});
            const hp = vmr.loadMemory(8, vm.heap_ptr);
            try std.fmt.format(memory.writer(), "  heap_ptr: {x:0>2}\n", .{hp});

            try memory.appendSlice("memory (heap):\n");

            var ptr = hsp;
            while (ptr < hp) {
                const size = vmr.loadMemory(8, ptr);
                if (size == 0) {
                    break;
                }

                try std.fmt.format(memory.writer(), "size: {}, 0x{x} - 0x{x}\n", .{ size, ptr, ptr + size });

                ptr += size;
            }

            try memory.appendSlice("\nmemory (HEX):\n");

            for (vmr.memory, 0..) |v, i| {
                const p = try std.fmt.allocPrint(draw_allocator, "{x:0>2} ", .{v});
                if (v == 0) {
                    try memory.appendSlice(".. ");
                } else {
                    try memory.appendSlice(p);
                }

                if (i % 16 == 16 - 1) {
                    try memory.appendSlice("\n");
                }
                if (i > 1024) {
                    break;
                }
            }

            const k = try std.fmt.allocPrint(draw_allocator,
                \\pc: {d}, bp: {d}, next: {s}
                \\stack_traces: {s}
                \\stack: {s}
            , .{ vmr.pc, bp, next.items, vmr.stack_traces.items, s });

            try dbg.set_text("stack", k);
            try dbg.set_text("memory", memory.items);

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
