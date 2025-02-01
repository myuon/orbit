const std = @import("std");
const fs = std.fs;
const mman = @cImport(@cInclude("sys/mman.h"));
const pthread = @cImport(@cInclude("pthread.h"));

const ast = @import("ast.zig");

const Register = enum(u32) { x0 = 0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30 };
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

    fn emitMovImm(self: *Arm64, target: Register, imm: u16) anyerror!void {
        try self.emit(0xD2800000 | (@as(u32, imm) << 5) | @as(u32, @intFromEnum(target)));
    }

    fn emitMov(self: *Arm64, source: Register, target: Register) anyerror!void {
        try self.emit(0xAA000000 | (@as(u32, @intFromEnum(source)) << 16 | @as(u32, @intFromEnum(source)) << 5 | @as(u32, @intFromEnum(target))));
    }

    fn emitStr(self: *Arm64, base: Register, value: Register) anyerror!void {
        try self.emit(0xF9000000 | (0x0 << 10) | (@as(u32, @intFromEnum(base) << 5) | @as(u32, @intFromEnum(value))));
    }

    fn emitLdr(self: *Arm64, offset: u9, source: Register, target: Register) anyerror!void {
        try self.emit(0xF8400400 | (@as(u32, offset) << 12) | (@as(u32, @intFromEnum(source) << 5) | @as(u32, @intFromEnum(target))));
    }

    fn emitAdd(self: *Arm64, source1: Register, source2: Register, target: Register) anyerror!void {
        try self.emit(0x8B000000 | (@as(u32, @intFromEnum(source1) << 16) | @as(u32, @intFromEnum(source2) << 5) | @as(u32, @intFromEnum(target))));
    }

    fn emitAddImm(self: *Arm64, source: Register, imm: u12, target: Register) anyerror!void {
        try self.emit(0x91000000 | (@as(u32, imm) << 10) | @as(u32, @intFromEnum(source) << 5) | @as(u32, @intFromEnum(target)));
    }

    fn emitMul(self: *Arm64, source1: Register, source2: Register, target: Register) anyerror!void {
        try self.emit(0x9B00FC00 | (@as(u32, @intFromEnum(source1)) << 16) | (@as(u32, @intFromEnum(source2)) << 5) | @as(u32, @intFromEnum(target)));
    }

    fn emitMadd(self: *Arm64, target: Register, source1: Register, source2: Register, base: Register) anyerror!void {
        const t: u32 = @intFromEnum(target);
        const b: u32 = @intFromEnum(base);
        const s1: u32 = @intFromEnum(source1);
        const s2: u32 = @intFromEnum(source2);

        try self.emit(0x9B000000 | (s1 << 16) | (b << 10) | (s2 << 5) | t);
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

    const reg_c_stack = Register.x0;
    const reg_c_sp = Register.x1;

    for (prog) |inst| {
        switch (inst) {
            .push => |n| {
                try code.emitLdr(0, reg_c_sp, .x9);
                try code.emitMovImm(.x11, 0x8);
                try code.emitMadd(.x10, .x11, .x9, reg_c_stack);

                try code.emitMovImm(.x9, @intCast(n));
                try code.emitStr(.x10, .x9);

                try code.emitLdr(0, reg_c_sp, .x9);
                try code.emitAddImm(.x9, 0x1, .x9);
                try code.emitStr(reg_c_sp, .x9);
            },
            .ret => {
                try code.emitRet();
            },
            else => {},
        }
    }

    try code.emitRet();

    pthread.pthread_jit_write_protect_np(0);
    @memcpy(buf_ptr, code.code_buf.items);
    pthread.pthread_jit_write_protect_np(1);

    std.debug.print("buf: {x}\n", .{code.code_buf.items});

    return @ptrCast(@alignCast(buf));
}

test {
    const cases = [_]struct {
        prog: []ast.Instruction,
        expected: struct {
            c_stack: []i64,
            c_sp: i64,
        },
    }{
        .{
            .prog = @constCast(&[_]ast.Instruction{
                .{ .push = 0x12 },
                .{ .push = 0x34 },
                .{ .push = 0x56 },
                .{ .ret = true },
            }),
            .expected = .{
                .c_stack = @constCast(&[_]i64{ 0x12, 0x34, 0x56, 0, 0 }),
                .c_sp = 3,
            },
        },
    };

    for (cases) |c| {
        var c_sp: i64 = 0;
        var c_stack = [_]i64{ 0, 0, 0, 0, 0 };
        const fn_ptr = try compileJit(c.prog);
        fn_ptr(@constCast((&c_stack).ptr), @constCast(&c_sp));

        try std.testing.expectEqualSlices(i64, c.expected.c_stack, &c_stack);
        try std.testing.expectEqual(c.expected.c_sp, c_sp);
    }
}
