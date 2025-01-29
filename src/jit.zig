const std = @import("std");
const fs = std.fs;
const mman = @cImport(@cInclude("sys/mman.h"));
const pthread = @cImport(@cInclude("pthread.h"));

pub fn compileJit() !*fn () callconv(.C) void {
    const buf = mman.mmap(
        null,
        4096,
        mman.PROT_WRITE | mman.PROT_EXEC,
        mman.MAP_PRIVATE | mman.MAP_ANONYMOUS | mman.MAP_JIT,
        -1,
        0,
    ) orelse unreachable;

    const buf_ptr: [*]u32 = @ptrCast(@alignCast(buf));

    pthread.pthread_jit_write_protect_np(0);
    @memcpy(
        buf_ptr,
        &[_]u32{
            0xd65f03c0, // ret
        },
    );
    pthread.pthread_jit_write_protect_np(1);

    return @ptrCast(@alignCast(buf));
}

test {
    const fn_ptr = try compileJit();
    const r = fn_ptr();

    std.debug.print("{any}\n", .{r});
}
