const std = @import("std");
const fs = std.fs;
const mman = @cImport(@cInclude("sys/mman.h"));
const pthread = @cImport(@cInclude("pthread.h"));

const ast = @import("ast.zig");

const Arm64 = struct {
    code_buf: std.ArrayList(u32),

    pub fn init(allocator: std.mem.Allocator) Arm64 {
        return Arm64{
            .code_buf = std.ArrayList(u32).init(allocator),
        };
    }

    pub fn deinit(self: *Arm64) void {
        self.code_buf.deinit();
    }

    fn emit(self: *Arm64, instr: u32) anyerror!void {
        try self.code_buf.append(instr);
    }

    fn emitRet(self: *Arm64) anyerror!void {
        try self.emit(0xd65f03c0); // ret x30
    }

    fn emitMovImm(self: *Arm64, target: u8, imm: u16) anyerror!void {
        try self.emit(0xD2800000 | (@as(u32, imm) << 5) | @as(u32, target));
    }

    fn emitStr(
        self: *Arm64,
        base: u8,
        value: u8,
    ) anyerror!void {
        try self.emit(0xF9000000 | (0x0 << 10) | (@as(u32, base) << 5) | @as(u32, value));
    }
};

const CompiledFn = *fn (
    c_stack: [*]i64,
    c_sp: *i64,
) callconv(.C) void;

pub fn compileJit(prog: []ast.Instruction) anyerror!CompiledFn {
    const buf = mman.mmap(
        null,
        4096,
        mman.PROT_WRITE | mman.PROT_EXEC,
        mman.MAP_PRIVATE | mman.MAP_ANONYMOUS | mman.MAP_JIT,
        -1,
        0,
    ) orelse unreachable;

    const buf_ptr: [*]u32 = @ptrCast(@alignCast(buf));

    var code = Arm64.init(std.testing.allocator);
    defer code.deinit();

    // for (prog) |inst| {
    //     switch (inst) {
    //         .push => |_| {
    //             // try code.emitMovkX9Imm(@intCast(n));
    //             // try code.emitPushX9();
    //             try code.emit(0xD2800009);
    //             try code.emit(0xF81F8FE9);
    //         },
    //         .ret => {
    //             // try code.emitPopX9();
    //             try code.emitRet();
    //         },
    //         else => {},
    //     }
    // }

    _ = prog;

    try code.emitMovImm(0x9, 1234);
    try code.emitStr(0x0, 0x9);

    try code.emitMovImm(0x9, 1);
    try code.emitStr(0x1, 0x9);

    try code.emitRet();

    pthread.pthread_jit_write_protect_np(0);
    @memcpy(buf_ptr, code.code_buf.items);
    pthread.pthread_jit_write_protect_np(1);

    std.debug.print("buf: {x}\n", .{code.code_buf.items});

    return @ptrCast(@alignCast(buf));
}

test {
    const fn_ptr = try compileJit(@constCast(&[_]ast.Instruction{
        .{ .push = 0x12 },
        .{ .ret = true },
    }));

    var k: i64 = 0;

    var stack = [_]i64{ 0, 0, 0, 0, 0 };
    fn_ptr(@constCast((&stack).ptr), @constCast(&k));

    std.debug.print("c_stack: {any}, c_sp: {d}\n", .{ stack, k });
}
