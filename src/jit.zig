const std = @import("std");
const fs = std.fs;
const mman = @cImport(@cInclude("sys/mman.h"));
const pthread = @cImport(@cInclude("pthread.h"));

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
        try self.emit(0xd65f03c0);
    }

    fn emitPopX0(self: *Arm64) anyerror!void {
        try self.emit(0xf84107e1); // ldr x1, [sp], 16
    }

    fn emitPushX0(self: *Arm64) anyerror!void {
        try self.emit(0xf81f0fe0); // str x0, [sp, -16]!
    }
};

pub fn compileJit() anyerror!*fn () callconv(.C) void {
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

    try code.emit(0xd2824680); // mov x0, #0x1234
    try code.emitPushX0();
    try code.emitPopX0();
    try code.emitRet();

    pthread.pthread_jit_write_protect_np(0);
    @memcpy(buf_ptr, code.code_buf.items);
    pthread.pthread_jit_write_protect_np(1);

    return @ptrCast(@alignCast(buf));
}

test {
    const fn_ptr = try compileJit();
    const r = fn_ptr();

    std.debug.print("{any}\n", .{r});
}
