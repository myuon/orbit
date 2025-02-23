const std = @import("std");
const fs = std.fs;
const mman = @cImport(@cInclude("sys/mman.h"));
const pthread = @cImport(@cInclude("pthread.h"));

const ast = @import("ast.zig");

const Register = enum(u32) { x0 = 0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31 };
const reg_xzr = Register.x31;
const reg_sp = Register.x31;

const Condition = enum(u4) {
    eq = 0b0000,
    ne = 0b0001,
    hs = 0b0010,
    lo = 0b0011,
    mi = 0b0100,
    pl = 0b0101,
    vs = 0b0110,
    vc = 0b0111,
    hi = 0b1000,
    ls = 0b1001,
    ge = 0b1010,
    lt = 0b1011,
    gt = 0b1100,
    le = 0b1101,
    al = 0b1110,
    nv = 0b1111,

    fn not(self: Condition) Condition {
        switch (self) {
            .eq => return .ne,
            .ne => return .eq,
            .hs => return .lo,
            .lo => return .hs,
            .mi => return .pl,
            .pl => return .mi,
            .vs => return .vc,
            .vc => return .vs,
            .hi => return .ls,
            .ls => return .hi,
            .ge => return .lt,
            .lt => return .ge,
            .gt => return .le,
            .le => return .gt,
            .al => return .nv,
            .nv => return .al,
        }
    }
};

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
        try self.emit(Arm64.getMovImmInstr(target, imm));
    }

    fn getMovImmInstr(target: Register, imm: u16) u32 {
        return 0xD2800000 | (@as(u32, imm) << 5) | @as(u32, @intFromEnum(target));
    }

    fn emitMovnImm(self: *Arm64, target: Register, imm: u16) anyerror!void {
        const t: u32 = @intFromEnum(target);
        const i: u32 = @as(u32, @intCast(imm));

        try self.emit(0x92800000 | (i << 5) | t);
    }

    fn emitMov(self: *Arm64, source: Register, target: Register) anyerror!void {
        try self.emit(0xAA000000 | (@as(u32, @intFromEnum(source)) << 16 | @as(u32, @intFromEnum(source)) << 5 | @as(u32, @intFromEnum(target))));
    }

    fn emitStr(self: *Arm64, base: Register, value: Register) anyerror!void {
        try self.emit(0xF9000000 | (0x0 << 10) | (@as(u32, @intFromEnum(base) << 5) | @as(u32, @intFromEnum(value))));
    }

    fn emitLdr(self: *Arm64, offset: u9, source: Register, target: Register) anyerror!void {
        std.debug.assert(source != target);
        try self.emit(0xF8400400 | (@as(u32, offset) << 12) | (@as(u32, @intFromEnum(source) << 5) | @as(u32, @intFromEnum(target))));
    }

    fn emitAdd(self: *Arm64, source1: Register, source2: Register, target: Register) anyerror!void {
        try self.emit(0x8B000000 | (@as(u32, @intFromEnum(source1) << 16) | @as(u32, @intFromEnum(source2) << 5) | @as(u32, @intFromEnum(target))));
    }

    fn emitAddImm(self: *Arm64, source: Register, imm: u12, target: Register) anyerror!void {
        try self.emit(0x91000000 | (@as(u32, imm) << 10) | @as(u32, @intFromEnum(source) << 5) | @as(u32, @intFromEnum(target)));
    }

    fn emitSub(self: *Arm64, target: Register, source1: Register, source2: Register) anyerror!void {
        const t: u32 = @intFromEnum(target);
        const s1: u32 = @intFromEnum(source1);
        const s2: u32 = @intFromEnum(source2);

        try self.emit(0xCB000000 | (s1 << 16) | (s2 << 5) | t);
    }

    fn emitSubImm(self: *Arm64, source: Register, imm: u12, target: Register) anyerror!void {
        try self.emit(0xD1000000 | (@as(u32, imm) << 10) | @as(u32, @intFromEnum(source) << 5) | @as(u32, @intFromEnum(target)));
    }

    fn emitMul(self: *Arm64, source1: Register, source2: Register, target: Register) anyerror!void {
        try self.emitMadd(target, source1, source2, reg_xzr);
    }

    /// madd t, s1, s2, b === t = s1 * s2 + b
    fn emitMadd(self: *Arm64, target: Register, source1: Register, source2: Register, base: Register) anyerror!void {
        const t: u32 = @intFromEnum(target);
        const b: u32 = @intFromEnum(base);
        const s1: u32 = @intFromEnum(source1);
        const s2: u32 = @intFromEnum(source2);

        try self.emit(0x9B000000 | (s1 << 16) | (b << 10) | (s2 << 5) | t);
    }

    /// msub t, s1, s2, b === t = b - s1 * s2
    fn emitMsub(self: *Arm64, target: Register, source1: Register, source2: Register, base: Register) anyerror!void {
        const t: u32 = @intFromEnum(target);
        const b: u32 = @intFromEnum(base);
        const s1: u32 = @intFromEnum(source1);
        const s2: u32 = @intFromEnum(source2);

        try self.emit(0x9B008000 | (s1 << 16) | (b << 10) | (s2 << 5) | t);
    }

    fn emitSdiv(self: *Arm64, target: Register, source1: Register, source2: Register) anyerror!void {
        const t: u32 = @intFromEnum(target);
        const s1: u32 = @intFromEnum(source1);
        const s2: u32 = @intFromEnum(source2);

        try self.emit(0x9AC00C00 | (s1 << 16) | (s2 << 5) | t);
    }

    fn emitSubs(self: *Arm64, target: Register, source1: Register, source2: Register) anyerror!void {
        const t: u32 = @intFromEnum(target);
        const s1: u32 = @intFromEnum(source1);
        const s2: u32 = @intFromEnum(source2);

        try self.emit(0xEB000000 | (s1 << 16) | (s2 << 5) | t);
    }

    fn emitCset(self: *Arm64, target: Register, condition: Condition) anyerror!void {
        return self.emitCsinc(target, reg_xzr, reg_xzr, condition.not());
    }

    fn emitCmp(self: *Arm64, source1: Register, source2: Register) anyerror!void {
        const s1: u32 = @intFromEnum(source1);
        const s2: u32 = @intFromEnum(source2);

        try self.emit(0xEB00001F | (s1 << 16) | (s2 << 5));
    }

    fn emitMrsNzcv(self: *Arm64, target: Register) anyerror!void {
        const t: u32 = @intFromEnum(target);

        try self.emit(0xD53B4200 | t);
    }

    fn emitCsinc(self: *Arm64, target: Register, source1: Register, source2: Register, condition: Condition) anyerror!void {
        const t: u32 = @intFromEnum(target);
        const s1: u32 = @intFromEnum(source1);
        const s2: u32 = @intFromEnum(source2);
        const c: u32 = @intFromEnum(condition);

        try self.emit(0x9A800400 | (c << 12) | (s1 << 16) | (s2 << 5) | t);
    }

    fn emitB(self: *Arm64, offset: i26) anyerror!void {
        try self.emit(Arm64.getBInstr(offset));
    }

    fn emitBL(self: *Arm64, offset: i26) anyerror!void {
        try self.emit(Arm64.getBLInstr(offset));
    }

    fn getBLInstr(offset: i26) u32 {
        const o = @as(u32, @intCast(@as(u26, @bitCast(offset))));

        return 0x94000000 | o;
    }

    fn getBInstr(offset: i26) u32 {
        const o = @as(u32, @intCast(@as(u26, @bitCast(offset))));

        return 0x14000000 | o;
    }

    fn emitCbz(self: *Arm64, source: Register, offset: i19) anyerror!void {
        const s: u32 = @intFromEnum(source);
        const o: u32 = @as(u32, @intCast(@as(u19, @bitCast(offset))));

        try self.emit(0xB4000000 | (o << 5) | s);
    }

    fn getCBZOffset(offset: i19) u32 {
        const o: u32 = @as(u32, @intCast(@as(u19, @bitCast(offset))));

        return o << 5;
    }

    fn emitStpPreIndex(self: *Arm64, source1: Register, source2: Register, base: Register, offset: i7) anyerror!void {
        const s1: u32 = @intFromEnum(source1);
        const s2: u32 = @intFromEnum(source2);
        const b: u32 = @intFromEnum(base);
        const o: u32 = @as(u32, @intCast(@as(u7, @bitCast(offset))));

        try self.emit(0xA9800000 | (o << 15) | (s2 << 10) | (b << 5) | s1);
    }

    fn emitLdpPostIndex(self: *Arm64, source1: Register, source2: Register, base: Register, offset: i7) anyerror!void {
        const s1: u32 = @intFromEnum(source1);
        const s2: u32 = @intFromEnum(source2);
        const b: u32 = @intFromEnum(base);
        const o: u32 = @as(u32, @intCast(@as(u7, @bitCast(offset))));

        try self.emit(0xA8C00000 | (o << 15) | (s2 << 10) | (b << 5) | s1);
    }
};

pub const JitRuntimeError = error{InstructionNotSupported};

pub const CompiledFn = *fn (
    c_stack: [*]i64, // .x0
    c_sp: *i64, // .x1
    c_bp: *i64, // .x2
    c_ip: *i64, // .x3
    c_memory: [*]u8, // .x4
) callconv(.C) void;

pub const JitRuntime = struct {
    allocator: std.mem.Allocator,

    const reg_c_stack = Register.x0;
    const reg_c_sp = Register.x1;
    const reg_c_bp = Register.x2;
    const reg_c_ip = Register.x3;
    const reg_c_memory = Register.x4;

    pub fn init(allocator: std.mem.Allocator) JitRuntime {
        return JitRuntime{
            .allocator = allocator,
        };
    }

    /// *c_sp
    fn getCSp(code: *Arm64, target: Register) anyerror!void {
        try code.emitLdr(0, reg_c_sp, target);
    }

    /// *c_sp = value
    fn setCSp(code: *Arm64, value: Register) anyerror!void {
        try code.emitStr(reg_c_sp, value);
    }

    /// *c_sp += 1
    fn incrementCSp(code: *Arm64, tmp1: Register) anyerror!void {
        try JitRuntime.getCSp(code, tmp1);
        try code.emitAddImm(tmp1, 0x1, tmp1);
        try JitRuntime.setCSp(code, tmp1);
    }

    /// *c_sp -= 1
    fn decrementCSp(code: *Arm64, tmp1: Register) anyerror!void {
        try JitRuntime.getCSp(code, tmp1);
        try code.emitSubImm(tmp1, 0x1, tmp1);
        try JitRuntime.setCSp(code, tmp1);
    }

    /// *c_bp
    fn getCBp(code: *Arm64, target: Register) anyerror!void {
        try code.emitLdr(0, reg_c_bp, target);
    }

    /// *c_bp = value
    fn setCBp(code: *Arm64, value: Register) anyerror!void {
        try code.emitStr(reg_c_bp, value);
    }

    /// *c_ip = value
    fn setCIp(code: *Arm64, value: Register) anyerror!void {
        try code.emitStr(reg_c_ip, value);
    }

    /// getCStackAddress(i, t) === t := &c_stack[i]
    fn getCStackAddress(code: *Arm64, index: Register, target: Register, tmp1: Register) anyerror!void {
        try code.emitMovImm(tmp1, 0x8);
        try code.emitMadd(target, index, tmp1, reg_c_stack);
    }

    /// setCStack(i, v) === c_stack[i] := v
    fn setCStack(code: *Arm64, index: Register, value: Register, tmp1: Register, tmp2: Register) anyerror!void {
        try JitRuntime.getCStackAddress(code, index, tmp1, tmp2);
        try code.emitStr(tmp1, value);
    }

    fn popCStack(code: *Arm64, target: Register, tmp1: Register, tmp2: Register, tmp3: Register) anyerror!void {
        try JitRuntime.decrementCSp(code, tmp1);

        try JitRuntime.getCStackAddress(code, tmp1, tmp2, tmp3);
        try code.emitLdr(0, tmp2, target);
    }

    fn pushCStack(code: *Arm64, value: Register, tmp1: Register, tmp2: Register, tmp3: Register) anyerror!void {
        try code.emitLdr(0, reg_c_sp, tmp1);
        try JitRuntime.setCStack(code, tmp1, value, tmp2, tmp3);

        try JitRuntime.incrementCSp(code, tmp1);
    }

    fn storeCMemory(code: *Arm64, address: Register, value: Register, tmp1: Register) anyerror!void {
        try code.emitAdd(address, reg_c_memory, tmp1);
        try code.emitStr(tmp1, value);
    }

    // target := bp+k
    fn getIndexFromOffset(code: *Arm64, k: i32, target: Register, tmp1: Register) anyerror!void {
        try JitRuntime.getCBp(code, tmp1);

        if (k >= 0) {
            try code.emitMovImm(target, @intCast(k));
            try code.emitAdd(target, tmp1, target);
        } else {
            try code.emitMovImm(target, @intCast(@abs(k)));
            try code.emitSub(target, target, tmp1);
        }
    }

    // target := c_stack[bp+k]
    fn loadCStackOffset(code: *Arm64, k: i32, target: Register, tmp1: Register, tmp2: Register) anyerror!void {
        try JitRuntime.getIndexFromOffset(code, k, tmp1, tmp2);
        try JitRuntime.getCStackAddress(code, tmp1, tmp1, tmp2);
        try code.emitLdr(0, tmp1, target);
    }

    pub fn compile(self: *JitRuntime, prog: []ast.Instruction, trace_mode: bool) anyerror!CompiledFn {
        var jumpSources = std.AutoHashMap(usize, usize).init(self.allocator);
        defer jumpSources.deinit();

        var jumpTargets = std.AutoHashMap(usize, usize).init(self.allocator);
        defer jumpTargets.deinit();

        for (prog, 0..) |inst, source| {
            switch (inst) {
                .jump_d => |target| {
                    try jumpSources.put(source, std.math.maxInt(usize));
                    try jumpTargets.put(target, std.math.maxInt(usize));
                },
                .jump_ifzero_d => |target| {
                    try jumpSources.put(source, std.math.maxInt(usize));
                    try jumpTargets.put(target, std.math.maxInt(usize));
                },
                .call_d => |target| {
                    try jumpSources.put(source, std.math.maxInt(usize));
                    try jumpTargets.put(target, std.math.maxInt(usize));
                },
                else => {},
            }
        }

        const buf = mman.mmap(
            null,
            1024 * 1024,
            mman.PROT_WRITE | mman.PROT_EXEC,
            mman.MAP_PRIVATE | mman.MAP_ANONYMOUS | mman.MAP_JIT,
            -1,
            0,
        ) orelse unreachable;

        const buf_ptr: [*]u32 = @ptrCast(@alignCast(buf));

        var code = Arm64.init(self.allocator);
        defer code.deinit();

        for (prog, 0..) |inst, p| {
            if (p == 0 and trace_mode) {
                // prologue for a subroutine
                try code.emitStpPreIndex(.x29, .x30, reg_sp, -16);
            }
            if (jumpTargets.contains(p)) {
                try jumpTargets.put(p, @intCast(code.code_buf.items.len));
            }
            if (p == 0 and !trace_mode) {
                // prologue for a subroutine
                try code.emitStpPreIndex(.x29, .x30, reg_sp, -16);
            }

            switch (inst) {
                .push => |n| {
                    if (n >= 0) {
                        try code.emitMovImm(.x9, @intCast(n));
                    } else {
                        try code.emitMovnImm(.x9, @intCast(@abs(n) - 1));
                    }

                    try JitRuntime.pushCStack(&code, .x9, .x15, .x14, .x13);
                },
                .pop => {
                    try JitRuntime.popCStack(&code, .x9, .x15, .x14, .x13);
                },
                .ret => {
                    try JitRuntime.popCStack(&code, .x9, .x15, .x14, .x13);

                    // epilogue for a subroutine
                    try code.emitLdpPostIndex(.x29, .x30, reg_sp, 16);

                    try code.emitRet();
                },
                .eq => {
                    try JitRuntime.popCStack(&code, .x9, .x15, .x14, .x13);
                    try JitRuntime.popCStack(&code, .x10, .x15, .x14, .x13);

                    try code.emitSubs(.x9, .x9, .x10);
                    try code.emitCset(.x9, Condition.eq);
                    try JitRuntime.pushCStack(&code, .x9, .x15, .x14, .x13);
                },
                .add => {
                    try JitRuntime.popCStack(&code, .x9, .x15, .x14, .x13);
                    try JitRuntime.popCStack(&code, .x10, .x15, .x14, .x13);

                    try code.emitAdd(.x9, .x10, .x9);
                    try JitRuntime.pushCStack(&code, .x9, .x15, .x14, .x13);
                },
                .add_di => |add_di| {
                    try JitRuntime.loadCStackOffset(&code, add_di.lhs, .x9, .x15, .x14);
                    try code.emitAddImm(.x9, @intCast(add_di.imm), .x9);

                    if (add_di.target) |t| {
                        try JitRuntime.getIndexFromOffset(&code, t, .x10, .x15);
                        try JitRuntime.setCStack(&code, .x10, .x9, .x15, .x14);
                    } else {
                        try JitRuntime.pushCStack(&code, .x9, .x15, .x14, .x13);
                    }
                },
                .madd_d => |madd_d| {
                    try JitRuntime.loadCStackOffset(&code, madd_d.rhs, .x9, .x15, .x14);
                    try JitRuntime.loadCStackOffset(&code, madd_d.lhs, .x10, .x15, .x14);
                    try JitRuntime.loadCStackOffset(&code, madd_d.base, .x11, .x15, .x14);
                    try code.emitMadd(.x9, .x9, .x10, .x11);

                    if (madd_d.target) |t| {
                        try JitRuntime.getIndexFromOffset(&code, t, .x10, .x15);
                        try JitRuntime.setCStack(&code, .x10, .x9, .x15, .x14);
                    } else {
                        try JitRuntime.pushCStack(&code, .x9, .x15, .x14, .x13);
                    }
                },
                .sub => {
                    try JitRuntime.popCStack(&code, .x9, .x15, .x14, .x13);
                    try JitRuntime.popCStack(&code, .x10, .x15, .x14, .x13);

                    try code.emitSub(.x9, .x9, .x10);
                    try JitRuntime.pushCStack(&code, .x9, .x15, .x14, .x13);
                },
                .mul => {
                    try JitRuntime.popCStack(&code, .x9, .x15, .x14, .x13);
                    try JitRuntime.popCStack(&code, .x10, .x15, .x14, .x13);

                    try code.emitMul(.x9, .x10, .x9);
                    try JitRuntime.pushCStack(&code, .x9, .x15, .x14, .x13);
                },
                .div => {
                    try JitRuntime.popCStack(&code, .x9, .x15, .x14, .x13);
                    try JitRuntime.popCStack(&code, .x10, .x15, .x14, .x13);

                    try code.emitSdiv(.x11, .x9, .x10);
                    try JitRuntime.pushCStack(&code, .x11, .x15, .x14, .x13);
                },
                .mod => {
                    try JitRuntime.popCStack(&code, .x9, .x15, .x14, .x13);
                    try JitRuntime.popCStack(&code, .x10, .x15, .x14, .x13);

                    try code.emitSdiv(.x11, .x9, .x10);
                    try code.emitMsub(.x11, .x9, .x11, .x10);
                    try JitRuntime.pushCStack(&code, .x11, .x15, .x14, .x13);
                },
                .lt => {
                    try JitRuntime.popCStack(&code, .x9, .x15, .x14, .x13);
                    try JitRuntime.popCStack(&code, .x10, .x15, .x14, .x13);

                    try code.emitCmp(.x9, .x10);
                    try code.emitCset(.x9, Condition.lt);
                    try JitRuntime.pushCStack(&code, .x9, .x15, .x14, .x13);
                },
                .lt_d => |lt_d| {
                    try JitRuntime.loadCStackOffset(&code, lt_d.lhs, .x10, .x15, .x14);
                    try JitRuntime.loadCStackOffset(&code, lt_d.rhs, .x9, .x15, .x14);

                    try code.emitCmp(.x9, .x10);
                    try code.emitCset(.x9, Condition.lt);
                    try JitRuntime.pushCStack(&code, .x9, .x15, .x14, .x13);
                },
                .lte => {
                    try JitRuntime.popCStack(&code, .x9, .x15, .x14, .x13);
                    try JitRuntime.popCStack(&code, .x10, .x15, .x14, .x13);

                    try code.emitCmp(.x9, .x10);
                    try code.emitCset(.x9, Condition.le);
                    try JitRuntime.pushCStack(&code, .x9, .x15, .x14, .x13);
                },
                .gt => {
                    try JitRuntime.popCStack(&code, .x9, .x15, .x14, .x13);
                    try JitRuntime.popCStack(&code, .x10, .x15, .x14, .x13);

                    try code.emitSubs(.x9, .x9, .x10);
                    try code.emitCset(.x9, Condition.gt);
                    try JitRuntime.pushCStack(&code, .x9, .x15, .x14, .x13);
                },
                .gte => {
                    try JitRuntime.popCStack(&code, .x9, .x15, .x14, .x13);
                    try JitRuntime.popCStack(&code, .x10, .x15, .x14, .x13);

                    try code.emitSubs(.x9, .x9, .x10);
                    try code.emitCset(.x9, Condition.ge);
                    try JitRuntime.pushCStack(&code, .x9, .x15, .x14, .x13);
                },
                .call_d => {
                    try code.emit(0x0);
                    try jumpSources.put(p, @intCast(code.code_buf.items.len - 1));
                },
                .jump_d => {
                    try code.emit(0x0);
                    try jumpSources.put(p, @intCast(code.code_buf.items.len - 1));
                },
                .jump_ifzero_d => {
                    try JitRuntime.popCStack(&code, .x9, .x15, .x14, .x13);
                    try code.emitCbz(.x9, 0x0);
                    try jumpSources.put(p, @intCast(code.code_buf.items.len - 1));
                },
                .get_pc => {
                    // push(0)
                    try JitRuntime.pushCStack(&code, reg_xzr, .x15, .x14, .x13);
                },
                .get_bp => {
                    try JitRuntime.getCBp(&code, .x9);
                    try JitRuntime.pushCStack(&code, .x9, .x15, .x14, .x13);
                },
                .set_bp => {
                    try JitRuntime.popCStack(&code, .x9, .x15, .x14, .x13);
                    try JitRuntime.setCBp(&code, .x9);
                },
                .get_sp => {
                    try JitRuntime.getCSp(&code, .x9);
                    try JitRuntime.pushCStack(&code, .x9, .x15, .x14, .x13);
                },
                .set_sp => {
                    try JitRuntime.popCStack(&code, .x9, .x15, .x14, .x13);
                    try JitRuntime.setCSp(&code, .x9);
                },
                .get_local_d => |k| {
                    try JitRuntime.loadCStackOffset(&code, k, .x9, .x15, .x14);
                    try JitRuntime.pushCStack(&code, .x9, .x15, .x14, .x13);
                },
                .set_local_d => |k| {
                    try JitRuntime.getIndexFromOffset(&code, k, .x9, .x15);
                    try JitRuntime.popCStack(&code, .x10, .x15, .x14, .x13);
                    try JitRuntime.setCStack(&code, .x9, .x10, .x15, .x14);
                },
                .nop => {},
                .set_cip => |n| {
                    try code.emitMovImm(.x9, @intCast(n));
                    try JitRuntime.setCIp(&code, .x9);
                },
                .store => |size| {
                    std.debug.assert(size == 8);

                    try JitRuntime.popCStack(&code, .x9, .x15, .x14, .x13); // value
                    try JitRuntime.popCStack(&code, .x10, .x15, .x14, .x13); // address

                    try JitRuntime.storeCMemory(&code, .x10, .x9, .x15);
                },
                else => {
                    std.debug.print("unhandled instruction: {any}\n", .{inst});
                    return error.InstructionNotSupported;
                },
            }
        }

        // epilogue for a subroutine
        try code.emitLdpPostIndex(.x29, .x30, reg_sp, 16);

        try code.emitRet();

        for (prog, 0..) |inst, source| {
            switch (inst) {
                .call_d => |target| {
                    const source_addr = jumpSources.get(source) orelse unreachable;
                    const target_addr = jumpTargets.get(target) orelse unreachable;
                    std.debug.assert(source_addr < code.code_buf.items.len);
                    std.debug.assert(target_addr < code.code_buf.items.len);

                    const offset = @as(i26, @intCast(target_addr)) - @as(i26, @intCast(source_addr));
                    std.debug.assert(code.code_buf.items[source_addr] == 0x0);
                    code.code_buf.items[source_addr] = Arm64.getBLInstr(offset);
                },
                .jump_d => |target| {
                    const source_addr = jumpSources.get(source) orelse unreachable;
                    const target_addr = jumpTargets.get(target) orelse unreachable;
                    std.debug.assert(source_addr < code.code_buf.items.len);
                    std.debug.assert(target_addr < code.code_buf.items.len);

                    const offset = @as(i26, @intCast(target_addr)) - @as(i26, @intCast(source_addr));
                    std.debug.assert(code.code_buf.items[source_addr] == 0x0);
                    code.code_buf.items[source_addr] = Arm64.getBInstr(offset);
                },
                .jump_ifzero_d => |target| {
                    const source_addr = jumpSources.get(source) orelse unreachable;
                    const target_addr = jumpTargets.get(target) orelse unreachable;
                    std.debug.assert(source_addr < code.code_buf.items.len);
                    std.debug.assert(target_addr < code.code_buf.items.len);

                    const offset = @as(i19, @intCast(target_addr)) - @as(i19, @intCast(source_addr));
                    std.debug.assert(code.code_buf.items[source_addr] & 0xFF000000 == 0xB4000000);
                    code.code_buf.items[source_addr] |= Arm64.getCBZOffset(offset);
                },
                else => {},
            }
        }

        pthread.pthread_jit_write_protect_np(0);
        @memcpy(buf_ptr, code.code_buf.items);
        pthread.pthread_jit_write_protect_np(1);

        // std.debug.print("buf: {x}\n", .{code.code_buf.items});
        // for (code.code_buf.items) |instr| {
        //     std.debug.print("{x:0>2}{x:0>2}{x:0>2}{x:0>2}\n", .{ instr & 0xff, (instr >> 8) & 0xff, (instr >> 16) & 0xff, (instr >> 24) & 0xff });
        // }

        return @ptrCast(@alignCast(buf));
    }
};

test {
    const cases = comptime [_]struct {
        prog: []ast.Instruction,
        initial_stack: []i64 = &[_]i64{},
        initial_bp: i64 = 0,
        expected: []i64,
    }{
        .{
            .prog = @constCast(&[_]ast.Instruction{
                .{ .push = 0x12 },
                .{ .push = 0x34 },
                .{ .push = 0x56 },
                .{ .pop = true },
            }),
            .expected = @constCast(&[_]i64{ 0x12, 0x34 }),
        },
        .{
            .prog = @constCast(&[_]ast.Instruction{
                .{ .push = 0x1 },
                .{ .push = 0x2 },
                .{ .eq = true },
            }),
            .expected = @constCast(&[_]i64{0x0}),
        },
        .{
            .prog = @constCast(&[_]ast.Instruction{
                .{ .push = 0x2 },
                .{ .push = 0x2 },
                .{ .eq = true },
            }),
            .expected = @constCast(&[_]i64{0x1}),
        },
        .{
            .prog = @constCast(&[_]ast.Instruction{
                .{ .push = 0x12 },
                .{ .push = 0x34 },
                .{ .add = true },
            }),
            .expected = @constCast(&[_]i64{0x46}),
        },
        .{
            .prog = @constCast(&[_]ast.Instruction{
                .{ .push = 0x34 },
                .{ .push = 0x12 },
                .{ .sub = true },
            }),
            .expected = @constCast(&[_]i64{0x22}),
        },
        .{
            .prog = @constCast(&[_]ast.Instruction{
                .{ .push = 0x12 },
                .{ .push = 0x8 },
                .{ .mul = true },
            }),
            .expected = @constCast(&[_]i64{0x90}),
        },
        .{
            .prog = @constCast(&[_]ast.Instruction{
                .{ .push = 0x1 },
                .{ .jump_d = 3 },
                .{ .push = 0x2 },
                .{ .push = 0x3 },
            }),
            .expected = @constCast(&[_]i64{ 0x1, 0x3 }),
        },
        .{
            .prog = @constCast(&[_]ast.Instruction{
                .{ .push = 0x1 },
                .{ .jump_d = 2 },
                .{ .push = 0x2 },
                .{ .push = 0x3 },
            }),
            .expected = @constCast(&[_]i64{ 0x1, 0x2, 0x3 }),
        },
        .{
            .prog = @constCast(&[_]ast.Instruction{
                .{ .push = 0x1 },
                .{ .jump_ifzero_d = 3 },
                .{ .push = 0x2 },
                .{ .push = 0x3 },
            }),
            .expected = @constCast(&[_]i64{ 0x2, 0x3 }),
        },
        .{
            .prog = @constCast(&[_]ast.Instruction{
                .{ .push = 0x0 },
                .{ .jump_ifzero_d = 3 },
                .{ .push = 0x2 },
                .{ .push = 0x3 },
            }),
            .expected = @constCast(&[_]i64{0x3}),
        },
        .{
            .prog = @constCast(&[_]ast.Instruction{
                .{ .push = 0x1 },
                .{ .push = 0x2 },
                .{ .push = 0x3 },
                .{ .get_local_d = 1 },
            }),
            .expected = @constCast(&[_]i64{ 0x1, 0x2, 0x3, 0x2 }),
        },
        .{
            .prog = @constCast(&[_]ast.Instruction{
                .{ .push = 0x1 },
                .{ .push = 0x2 },
                .{ .push = 0x3 },
                .{ .set_local_d = 1 },
            }),
            .expected = @constCast(&[_]i64{ 0x1, 0x3 }),
        },
        .{
            .prog = @constCast(&[_]ast.Instruction{
                .{ .push = 0x1 },
                .{ .push = 0x2 },
                .{ .push = 0x3 },
                .{ .push = 0x4 },
                .{ .push = 0x5 },
                .{ .push = 0x3 },
                .{ .set_bp = true },
                .{ .get_local_d = -1 },
            }),
            .expected = @constCast(&[_]i64{ 0x1, 0x2, 0x3, 0x4, 0x5, 0x3 }),
        },
        .{
            .prog = @constCast(&[_]ast.Instruction{
                .{ .push = 0x1 },
                .{ .push = 0x2 },
                .{ .push = 0x3 },
                .{ .push = 0x4 },
                .{ .push = 0x5 },
                .{ .push = 0x3 },
                .{ .set_bp = true },
                .{ .push = 0x12 },
                .{ .set_local_d = -1 },
            }),
            .expected = @constCast(&[_]i64{ 0x1, 0x2, 0x12, 0x4, 0x5 }),
        },
        .{
            .prog = @constCast(&[_]ast.Instruction{
                .{ .push = -1 },
            }),
            .expected = @constCast(&[_]i64{-1}),
        },
        .{
            .prog = @constCast(&[_]ast.Instruction{
                .{ .get_local_d = -3 },
                .{ .push = 0 },
                .{ .eq = true },
                .{ .jump_ifzero_d = 12 },
                .{ .nop = true },
                .{ .push = 0 },
                .{ .set_local_d = -4 },
                .{ .get_bp = true },
                .{ .set_sp = true },
                .{ .set_bp = true },
                .{ .ret = true },
                .{ .jump_d = 13 },
                .{ .nop = true },
                .{ .nop = true },
                .{ .get_local_d = -3 },
                .{ .push = -2 },
                .{ .get_local_d = -3 },
                .{ .push = 1 },
                .{ .sub = true },
                .{ .get_pc = true },
                .{ .get_bp = true },
                .{ .get_sp = true },
                .{ .set_bp = true },
                .{ .call_d = 0 },
                .{ .pop = true },
                .{ .add = true },
                .{ .set_local_d = -4 },
                .{ .get_bp = true },
                .{ .set_sp = true },
                .{ .set_bp = true },
                .{ .ret = true },
            }),
            .initial_stack = @constCast(&[_]i64{ -2, 10, -1, 0 }),
            .initial_bp = 4,
            .expected = @constCast(&[_]i64{ 55, 10 }),
        },
        .{
            .prog = @constCast(&[_]ast.Instruction{
                .{ .push = 7 },
                .{ .push = 5 },
                .{ .lt = true },
            }),
            .expected = @constCast(&[_]i64{0}),
        },
        .{
            .prog = @constCast(&[_]ast.Instruction{
                .{ .push = 5 },
                .{ .push = 7 },
                .{ .lt = true },
            }),
            .expected = @constCast(&[_]i64{1}),
        },
        .{
            .prog = @constCast(&[_]ast.Instruction{
                .{ .push = 5 },
                .{ .push = 5 },
                .{ .lt = true },
            }),
            .expected = @constCast(&[_]i64{0}),
        },
        .{
            .prog = @constCast(&[_]ast.Instruction{
                .{ .push = 7 },
                .{ .push = 5 },
                .{ .lte = true },
            }),
            .expected = @constCast(&[_]i64{0}),
        },
        .{
            .prog = @constCast(&[_]ast.Instruction{
                .{ .push = 5 },
                .{ .push = 7 },
                .{ .lte = true },
            }),
            .expected = @constCast(&[_]i64{1}),
        },
        .{
            .prog = @constCast(&[_]ast.Instruction{
                .{ .push = 5 },
                .{ .push = 5 },
                .{ .lte = true },
            }),
            .expected = @constCast(&[_]i64{1}),
        },
        .{
            .prog = @constCast(&[_]ast.Instruction{
                .{ .push = 37 },
                .{ .push = 10 },
                .{ .div = true },
            }),
            .expected = @constCast(&[_]i64{3}),
        },
        .{
            .prog = @constCast(&[_]ast.Instruction{
                .{ .push = 37 },
                .{ .push = 10 },
                .{ .mod = true },
            }),
            .expected = @constCast(&[_]i64{7}),
        },
        .{
            .prog = @constCast(&[_]ast.Instruction{
                .{ .push = 25 },
                .{ .push = 5 },
                .{ .mod = true },
            }),
            .expected = @constCast(&[_]i64{0}),
        },
    };

    for (cases) |c| {
        var c_bp: i64 = 0;
        var c_sp: i64 = 0;
        var c_stack = [_]i64{0} ** 1024;
        var c_memory = [_]u8{0} ** 1024;

        if (c.initial_stack.len > 0) {
            for (c.initial_stack, 0..) |v, i| {
                c_stack[i] = v;
            }

            c_sp = @intCast(c.initial_stack.len);
        }
        if (c.initial_bp != 0) {
            c_bp = c.initial_bp;
        }

        var runtime = JitRuntime.init(std.testing.allocator);
        const fn_ptr = try runtime.compile(c.prog, false);
        var ip: i64 = 0;
        fn_ptr(@constCast((&c_stack).ptr), @constCast(&c_sp), @constCast(&c_bp), @constCast(&ip), @constCast((&c_memory).ptr));

        std.testing.expectEqualSlices(i64, c.expected, c_stack[0..@min(@as(usize, @intCast(c_sp)), c_stack.len)]) catch |err| {
            std.debug.panic("prog: {any}, error: {any}\n", .{ c.prog, err });
        };
    }
}
