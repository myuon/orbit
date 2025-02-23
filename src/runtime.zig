const std = @import("std");
const ast = @import("ast.zig");
const jit = @import("jit.zig");
const P = @import("profiler");
const vm = @import("vm.zig");

const ControlFlow = enum {
    Continue,
    Terminated,

    pub fn isTerminated(self: ControlFlow) bool {
        return self == ControlFlow.Terminated;
    }
};

const MapEntry = struct {
    key: []u8,
    value: ast.Value,
};

const VecData = struct {
    array_ptr: usize,
    len: usize,
    capacity: usize,
};

pub const VmRuntimeError = error{
    LabelNotFound,
    AssertionFailed,
    JitCompileFailed,
};

pub const VmRuntime = struct {
    const JitCache = std.StringHashMap(jit.CompiledFn);

    pc: usize,
    envs: std.ArrayList(std.StringHashMap(i64)),
    enable_jit: bool,
    hot_spot_labels: std.StringHashMap(usize),
    traces: ?std.ArrayList(ast.Instruction),
    jit_cache: JitCache,
    allocator: std.mem.Allocator,
    arena_allocator: std.heap.ArenaAllocator,
    memory: []u8,
    hp: usize = 0,

    pub fn init(allocator: std.mem.Allocator) VmRuntime {
        const size = 1024 * 1024;

        // allocate memory with zero-initialized
        var memory = std.heap.page_allocator.alloc(u8, size) catch unreachable;
        for (0..size) |i| {
            memory[i] = 0x0;
        }

        return VmRuntime{
            .pc = 0,
            .envs = std.ArrayList(std.StringHashMap(i64)).init(allocator),
            .hot_spot_labels = std.StringHashMap(usize).init(allocator),
            .traces = null,
            .jit_cache = JitCache.init(allocator),
            .enable_jit = true,
            .allocator = allocator,
            .arena_allocator = std.heap.ArenaAllocator.init(allocator),
            .memory = memory,
            .hp = 0,
        };
    }

    pub fn deinit(self: *VmRuntime) void {
        if (self.traces) |traces| {
            traces.deinit();
        }
        self.envs.deinit();
        self.hot_spot_labels.deinit();
        self.jit_cache.deinit();
        self.arena_allocator.deinit();
    }

    fn find_label(program: []ast.Instruction, target_label: []const u8) anyerror!?usize {
        var count: usize = 0;
        while (count < program.len) : (count += 1) {
            switch (program[count]) {
                .label => |l| {
                    if (std.mem.eql(u8, l, target_label)) {
                        return count;
                    }
                },
                else => {},
            }
        }

        std.log.err("Label not found: {s}\n", .{target_label});
        return error.LabelNotFound;
    }

    fn get_address_on_stack(stack: *std.ArrayList(i64), bp: *i64, k: i32) anyerror!usize {
        const b: i32 = @intCast(bp.*);
        if (b + k >= stack.items.len) {
            std.log.err("Invalid address: {d} at {any}\n", .{ b + k, stack.items });
            return error.AssertionFailed;
        }

        return @intCast(b + k);
    }

    pub fn step(
        self: *VmRuntime,
        program: []ast.Instruction,
        stack: *std.ArrayList(i64),
        bp: *i64,
    ) anyerror!ControlFlow {
        if (self.pc >= program.len) {
            return ControlFlow.Terminated;
        }

        const inst = program[self.pc];

        if (self.traces) |traces| {
            try self.traces.?.append(inst);

            if (traces.items.len > 1_000_000) {
                std.log.warn("Tracing limit exceeded", .{});

                self.traces.?.deinit();
                self.traces = null;
            }
        }

        switch (inst) {
            .nop => {
                self.pc += 1;
            },
            .push => |n| {
                try stack.append(n);
                self.pc += 1;
            },
            .pop => {
                _ = stack.pop();
                self.pc += 1;
            },
            .eq => {
                const rhs = stack.pop();
                const lhs = stack.pop();
                if (lhs == rhs) {
                    try stack.append(1);
                } else {
                    try stack.append(0);
                }
                self.pc += 1;
            },
            .ret => {
                const p = stack.pop();
                if (p == -1) {
                    return ControlFlow.Terminated;
                } else {
                    self.pc = @intCast(p + 5);
                }
            },
            .jump => |label| {
                const zone = P.begin(@src(), "VmRuntime.step.jump");
                defer zone.end();

                const entry = try self.hot_spot_labels.getOrPutValue(label, 0);
                entry.value_ptr.* += 1;

                const pc = self.pc;

                const target = (try VmRuntime.find_label(program, label)).?;

                var result_fn_ptr: ?jit.CompiledFn = null;
                if (self.jit_cache.get(label)) |fn_ptr| {
                    result_fn_ptr = fn_ptr;
                } else if (self.enable_jit and entry.value_ptr.* >= 10) {
                    // When tracing is finished
                    if (self.traces) |traces| {
                        if (std.mem.eql(u8, traces.items[0].label, label)) {
                            const jitCompile = P.begin(@src(), "VmRuntime.step.jump.jitCompile");
                            defer jitCompile.end();

                            // Only supports: [label, ..., jump label] fragment
                            std.debug.assert(std.mem.eql(u8, traces.items[0].label, label));
                            std.debug.assert(std.mem.eql(u8, traces.items[traces.items.len - 1].jump, label));

                            var vmc = vm.VmCompiler.init(self.allocator);
                            defer vmc.deinit();

                            var ir_block = std.ArrayList(ast.Instruction).init(self.allocator);
                            defer ir_block.deinit();

                            try ir_block.appendSlice(traces.items);

                            var exit_positions = std.ArrayList(usize).init(self.allocator);
                            defer exit_positions.deinit();

                            // label -> exit position in ir_block
                            var exit_stub = std.StringHashMap(usize).init(self.allocator);
                            defer exit_stub.deinit();

                            var quit_compiling = false;

                            var fallback_block = std.ArrayList(ast.Instruction).init(self.allocator);
                            defer fallback_block.deinit();

                            // find the exit path
                            // TODO: support `jump` to outside of the block
                            for (ir_block.items) |t| {
                                switch (t) {
                                    .jump_ifzero => |l| {
                                        if (!exit_stub.contains(l)) {
                                            try exit_stub.put(l, ir_block.items.len);

                                            // Add fallback block (when label not found)
                                            const ip = (try VmRuntime.find_label(program, l)).?;
                                            try fallback_block.append(ast.Instruction{ .set_cip = ip });
                                            try fallback_block.append(ast.Instruction{ .push = -1 });
                                            try fallback_block.append(ast.Instruction{ .ret = true });
                                        }
                                    },
                                    .call => {
                                        quit_compiling = true;
                                        break;
                                    },
                                    else => {},
                                }
                            }

                            if (!quit_compiling) {
                                try ir_block.appendSlice(fallback_block.items);

                                try vmc.resolveIrLabels(ir_block.items, exit_stub);

                                var runtime = jit.JitRuntime.init(self.allocator);

                                const result = runtime.compile(ir_block.items, true);
                                _ = result catch |err| {
                                    std.log.debug("JIT compile error, fallback to VM execution: {any}", .{err});

                                    quit_compiling = true;
                                };

                                if (!quit_compiling) {
                                    std.log.info("Tracing & compile finished, {d}", .{ir_block.items.len});

                                    const f = try result;
                                    try self.jit_cache.put(label, f);

                                    self.traces.?.deinit();
                                    self.traces = null;

                                    result_fn_ptr = f;
                                }
                            }
                        }
                    } else {
                        // When jumping backwards
                        if (target < pc) {
                            // start tracing
                            self.traces = std.ArrayList(ast.Instruction).init(self.allocator);
                        }
                    }
                }

                if (result_fn_ptr) |fn_ptr| {
                    const zone_call_jit_fn = P.begin(@src(), "VmRuntime.step.jump.call_jit_fn");
                    defer zone_call_jit_fn.end();

                    var ip: i64 = -1;
                    var sp = @as(i64, @intCast(stack.items.len));

                    // std.log.info("BEF: {s}, {d} {any} ({d}) {any}", .{ label, bp.*, stack.items[0..@intCast(sp)], self.pc, self.memory[0..100] });
                    fn_ptr((&stack.items).ptr, &sp, bp, &ip, self.memory.ptr);
                    // std.log.info("AFT: {s}, {d} {any} ({d}) {any}", .{ label, bp.*, stack.items[0..@intCast(sp)], ip, self.memory[0..100] });

                    // epilogue here
                    if (ip != -1) {
                        self.pc = @intCast(ip);
                    }

                    stack.shrinkAndFree(@intCast(sp));

                    return ControlFlow.Continue;
                } else {
                    self.pc = target;
                }
            },
            .jump_ifzero => |label| {
                const cond = stack.pop();
                if (cond == 0) {
                    self.pc = (try VmRuntime.find_label(program, label)).?;
                }

                self.pc += 1;
            },
            .jump_d => {
                unreachable;
            },
            .jump_ifzero_d => |c| {
                const cond = stack.pop();
                if (cond == 0) {
                    self.pc = c;
                } else {
                    self.pc += 1;
                }
            },
            .add => {
                const rhs = stack.pop();
                const lhs = stack.pop();
                try stack.append(lhs + rhs);

                self.pc += 1;
            },
            .add_di => |add_di| {
                const rhs = add_di.imm;
                const lhs = stack.items[try get_address_on_stack(stack, bp, add_di.lhs)];

                if (add_di.target) |t| {
                    stack.items[try get_address_on_stack(stack, bp, t)] = lhs + rhs;
                } else {
                    try stack.append(lhs + rhs);
                }

                self.pc += 1;
            },
            .madd_d => |madd_d| {
                const rhs = stack.items[try get_address_on_stack(stack, bp, madd_d.rhs)];
                const lhs = stack.items[try get_address_on_stack(stack, bp, madd_d.lhs)];
                const base = stack.items[try get_address_on_stack(stack, bp, madd_d.base)];
                if (madd_d.target) |t| {
                    stack.items[try get_address_on_stack(stack, bp, t)] = lhs * rhs + base;
                } else {
                    try stack.append(lhs * rhs + base);
                }

                self.pc += 1;
            },
            .sub => {
                const rhs = stack.pop();
                const lhs = stack.pop();
                try stack.append(lhs - rhs);

                self.pc += 1;
            },
            .mul => {
                const rhs = stack.pop();
                const lhs = stack.pop();
                try stack.append(lhs * rhs);

                self.pc += 1;
            },
            .call_d => |addr| {
                self.pc = addr;
            },
            .call => |label| {
                const entry = try self.hot_spot_labels.getOrPut(label);
                if (self.enable_jit and entry.found_existing) {
                    entry.value_ptr.* += 1;

                    if (entry.value_ptr.* > 3) {
                        var fn_ptr: jit.CompiledFn = undefined;

                        if (self.jit_cache.get(label)) |f| {
                            fn_ptr = f;
                        } else {
                            const zone = P.begin(@src(), "VmRuntime.step.call.jitCompile");
                            defer zone.end();

                            const call_block_start = try VmRuntime.find_label(program, label);

                            const end_label = try std.fmt.allocPrint(self.allocator, "end_of_{s}", .{label});
                            defer self.allocator.free(end_label);
                            const call_block_end = try VmRuntime.find_label(program, end_label);

                            var ir_block_list = std.ArrayList(ast.Instruction).init(self.allocator);
                            defer ir_block_list.deinit();

                            try ir_block_list.appendSlice(program[call_block_start.?..call_block_end.?]);
                            const ir_block = ir_block_list.items;

                            var vmc = vm.VmCompiler.init(self.allocator);
                            defer vmc.deinit();

                            try vmc.resolveIrLabels(ir_block, null);

                            var params = std.ArrayList([]const u8).init(self.allocator);
                            defer params.deinit();

                            var runtime = jit.JitRuntime.init(self.allocator);
                            const f = runtime.compile(ir_block, false) catch |err| {
                                std.debug.print("JIT compile error, fallback to VM execution: {any}\n", .{err});

                                unreachable;
                            };

                            try self.jit_cache.put(label, f);

                            fn_ptr = f;
                        }

                        var ip: i64 = -1;
                        var sp = @as(i64, @intCast(stack.items.len));

                        fn_ptr((&stack.items).ptr, &sp, bp, &ip, self.memory.ptr);

                        // epilogue here
                        self.pc += 1;
                        std.debug.assert(ip == -1);

                        stack.shrinkAndFree(@intCast(sp));

                        return ControlFlow.Continue;
                    }
                } else {
                    entry.value_ptr.* = 1;
                }

                self.pc = (try VmRuntime.find_label(program, label)).?;
            },
            .get_local_d => |k| {
                const value = stack.items[try get_address_on_stack(stack, bp, k)];
                try stack.append(value);
                self.pc += 1;
            },
            .set_local_d => |k| {
                const value = stack.pop();
                stack.items[try get_address_on_stack(stack, bp, k)] = value;
                self.pc += 1;
            },
            .label => {
                self.pc += 1;
            },
            .get_pc => {
                try stack.append(@intCast(self.pc));
                self.pc += 1;
            },
            .get_bp => {
                try stack.append(@intCast(bp.*));
                self.pc += 1;
            },
            .set_bp => {
                std.debug.assert(stack.items.len > 0);

                const value = stack.pop();
                bp.* = @intCast(value);
                self.pc += 1;
            },
            .get_sp => {
                try stack.append(@intCast(stack.items.len));
                self.pc += 1;
            },
            .set_sp => {
                const value = stack.pop();
                stack.shrinkAndFree(@intCast(value));
                self.pc += 1;
            },
            .div => {
                const rhs = stack.pop();
                const lhs = stack.pop();
                try stack.append(@divTrunc(lhs, rhs));

                self.pc += 1;
            },
            .mod => {
                const rhs = stack.pop();
                const lhs = stack.pop();
                try stack.append(@mod(lhs, rhs));

                self.pc += 1;
            },
            .lt => {
                const rhs = stack.pop();
                const lhs = stack.pop();
                if (lhs < rhs) {
                    try stack.append(1);
                } else {
                    try stack.append(0);
                }
                self.pc += 1;
            },
            .lt_d => |lt_d| {
                const rhs = stack.items[try get_address_on_stack(stack, bp, lt_d.rhs)];
                const lhs = stack.items[try get_address_on_stack(stack, bp, lt_d.lhs)];
                if (lhs < rhs) {
                    try stack.append(1);
                } else {
                    try stack.append(0);
                }
                self.pc += 1;
            },
            .lte => {
                const rhs = stack.pop();
                const lhs = stack.pop();
                if (lhs <= rhs) {
                    try stack.append(1);
                } else {
                    try stack.append(0);
                }
                self.pc += 1;
            },
            .gt => {
                const rhs = stack.pop();
                const lhs = stack.pop();
                if (lhs > rhs) {
                    try stack.append(1);
                } else {
                    try stack.append(0);
                }
                self.pc += 1;
            },
            .gte => {
                const rhs = stack.pop();
                const lhs = stack.pop();
                if (lhs >= rhs) {
                    try stack.append(1);
                } else {
                    try stack.append(0);
                }
                self.pc += 1;
            },
            .load => |size| {
                const addr = stack.pop();

                const n = self.loadMemory(size, addr);
                try stack.append(n);

                self.pc += 1;
            },
            .store => |size| {
                const value = stack.pop();
                const addr = stack.pop();
                self.storeMemory(size, @intCast(addr), value);
                self.pc += 1;
            },
            .set_memory => |m| {
                const addr: usize = @intCast(stack.pop());
                const data = m.data;
                for (data, 0..) |d, i| {
                    self.memory[addr + i] = d;
                }
                self.hp = addr + data.len + 1;
                self.pc += 1;
            },
            .allocate_memory => {
                const size = stack.pop();
                try self.allocateMemory(stack, @intCast(size));
                self.pc += 1;
            },
            .set_cip => {
                unreachable;
            },
            .table_set => {
                const value = stack.pop();
                const key = stack.pop();
                const map = stack.pop();

                const key_str = try self.loadMemoryString(key);
                const data = try self.findMapEntry(map, key_str, 128);

                if (data.entry == null) {
                    try self.allocateMemory(stack, 16);
                    const entry_ptr = stack.pop();
                    self.storeMemory(8, @intCast(map + @as(i64, @intCast(data.index)) * 8), entry_ptr);

                    self.storeMemory(8, @intCast(entry_ptr), key);
                    self.storeMemory(8, @intCast(entry_ptr + 8), value);
                } else {
                    const entry_ptr = self.loadMemory(8, @as(i64, @intCast(map + @as(i64, @intCast(data.index)) * 8)));
                    self.storeMemory(8, @intCast(entry_ptr + 8), value);
                }

                self.pc += 1;
            },
            .table_get => {
                const key = stack.pop();
                const map = stack.pop();

                const key_str = try self.loadMemoryString(key);
                const data = try self.findMapEntry(map, key_str, 128);

                try stack.append(data.entry.?.value.i64_);
                self.pc += 1;
            },
            .allocate_vec => |size| {
                std.debug.assert(size == 8);
                try self.allocateVec(stack, size, 128);

                self.pc += 1;
            },
            .vec_get => {
                const index = stack.pop();
                const vec = stack.pop();

                const vecData = try self.loadVecData(vec);
                const value = self.loadMemory(8, @as(i64, @intCast(vecData.array_ptr)) + index * 8);
                try stack.append(value);

                self.pc += 1;
            },
            .vec_set => {
                const value = stack.pop();
                const index = stack.pop();
                const vec = stack.pop();

                const vecData = try self.loadVecData(vec);
                std.debug.assert(index < vecData.len);
                self.storeMemory(8, vecData.array_ptr + @as(usize, @intCast(index)) * 8, value);

                self.pc += 1;
            },
            .vec_push => {
                const value = stack.pop();
                const vec = stack.pop();

                const vecData = try self.loadVecData(vec);
                if (vecData.len + 1 < vecData.capacity) {
                    self.storeMemory(8, vecData.array_ptr + vecData.len * 8, value);
                    self.storeMemory(8, @intCast(vec + 8), @intCast(vecData.len + 1));
                } else {
                    try self.allocateVec(stack, 8, vecData.capacity * 2);

                    const array_ptr = stack.pop();
                    self.storeMemory(8, @intCast(vec), @intCast(array_ptr));
                    self.storeMemory(8, @intCast(vec + 8), @intCast(vecData.len + 1));
                    self.storeMemory(8, @intCast(vec + 16), @intCast(vecData.capacity * 2));

                    for (0..vecData.len) |i| {
                        self.storeMemory(8, vecData.array_ptr + i * 8, self.loadMemory(8, vec + @as(i64, @intCast(i)) * 8));
                    }
                    self.storeMemory(8, vecData.array_ptr + vecData.len * 8, value);

                    const newVecData = try self.loadVecData(vec);
                    std.debug.assert(newVecData.array_ptr == array_ptr);
                    std.debug.assert(newVecData.len == vecData.len + 1);
                    std.debug.assert(newVecData.capacity == vecData.capacity * 2);
                }

                self.pc += 1;
            },
            .set_hp => {
                const value = stack.pop();
                self.hp = @intCast(value);
                self.pc += 1;
            },
        }

        return ControlFlow.Continue;
    }

    fn loadMemory(self: *VmRuntime, size: u4, addr: i64) i64 {
        var n: i64 = 0;
        n |= @as(i64, @intCast(self.memory[@intCast(addr)]));
        if (size >= 2) {
            n |= @as(i64, @intCast(self.memory[@intCast(addr + 1)])) << 8;
        }
        if (size >= 3) {
            n |= @as(i64, @intCast(self.memory[@intCast(addr + 2)])) << 16;
        }
        if (size >= 4) {
            n |= @as(i64, @intCast(self.memory[@intCast(addr + 3)])) << 24;
        }
        if (size >= 5) {
            n |= @as(i64, @intCast(self.memory[@intCast(addr + 4)])) << 32;
        }
        if (size >= 6) {
            n |= @as(i64, @intCast(self.memory[@intCast(addr + 5)])) << 40;
        }
        if (size >= 7) {
            n |= @as(i64, @intCast(self.memory[@intCast(addr + 6)])) << 48;
        }
        if (size >= 8) {
            n |= @as(i64, @intCast(self.memory[@intCast(addr + 7)])) << 56;
        }

        return n;
    }

    fn allocateMemory(self: *VmRuntime, stack: *std.ArrayList(i64), size: usize) anyerror!void {
        const hp = self.hp;
        self.hp += size;

        try stack.append(@intCast(hp));
    }

    fn storeMemory(self: *VmRuntime, size: u4, addr: usize, value: i64) void {
        self.memory[@intCast(addr)] = @intCast(value & 0xff);
        if (size >= 2) {
            self.memory[@intCast(addr + 1)] = @intCast((value >> 8) & 0xff);
        }
        if (size >= 3) {
            self.memory[@intCast(addr + 2)] = @intCast((value >> 16) & 0xff);
        }
        if (size >= 4) {
            self.memory[@intCast(addr + 3)] = @intCast((value >> 24) & 0xff);
        }
        if (size >= 5) {
            self.memory[@intCast(addr + 4)] = @intCast((value >> 32) & 0xff);
        }
        if (size >= 6) {
            self.memory[@intCast(addr + 5)] = @intCast((value >> 40) & 0xff);
        }
        if (size >= 7) {
            self.memory[@intCast(addr + 6)] = @intCast((value >> 48) & 0xff);
        }
        if (size >= 8) {
            self.memory[@intCast(addr + 7)] = @intCast((value >> 56) & 0xff);
        }
    }

    fn findMapEntry(self: *VmRuntime, map: i64, key: []u8, capacity: usize) anyerror!struct {
        index: usize,
        entry: ?MapEntry,
    } {
        var index = (try hashMapKey(key)) % capacity;
        while (true) {
            const entry = try self.loadMapEntry(map, index);
            if (entry) |e| {
                if (std.mem.eql(u8, e.key, key)) {
                    return .{ .index = index, .entry = e };
                }
            } else {
                return .{ .index = index, .entry = null };
            }

            index = (index + 1) % capacity;
        }
    }

    fn loadMapEntry(self: *VmRuntime, map: i64, index: usize) anyerror!?MapEntry {
        const entry_ptr = self.loadMemory(8, map + @as(i64, @intCast(index)) * 8);
        const key_ptr = self.loadMemory(8, entry_ptr);
        const value_ptr = self.loadMemory(8, entry_ptr + 8);

        if (key_ptr == 0) {
            return null;
        } else {
            return MapEntry{
                .key = try self.loadMemoryString(key_ptr),
                .value = ast.Value{ .i64_ = value_ptr },
            };
        }
    }

    fn hashMapKey(str: []u8) !usize {
        var hash: usize = 2166136261;
        for (str) |c| {
            hash ^= @intCast(c);
            hash = @mulWithOverflow(hash, 16777619)[0];
        }

        return hash;
    }

    fn loadMemoryString(self: *VmRuntime, address: i64) anyerror![]u8 {
        var buffer = std.ArrayList(u8).init(self.arena_allocator.allocator());
        var index: usize = @intCast(address);
        while (self.memory[index] != 0) {
            try buffer.append(self.memory[index]);
            index += 1;
        }

        return buffer.items;
    }

    fn allocateVec(self: *VmRuntime, stack: *std.ArrayList(i64), size: u4, capacity: usize) anyerror!void {
        try self.allocateMemory(stack, 8 * 3);
        const entry_ptr = stack.pop();

        try self.allocateMemory(stack, @as(usize, size) * capacity);
        const array_ptr = stack.pop();

        self.storeMemory(8, @intCast(entry_ptr), array_ptr);
        self.storeMemory(8, @intCast(entry_ptr + 8), 0);
        self.storeMemory(8, @intCast(entry_ptr + 16), @intCast(capacity));

        try stack.append(entry_ptr);

        const vecData = try self.loadVecData(entry_ptr);
        std.debug.assert(vecData.array_ptr == array_ptr);
        std.debug.assert(vecData.len == 0);
        std.debug.assert(vecData.capacity == capacity);
    }

    fn loadVecData(self: *VmRuntime, address: i64) anyerror!VecData {
        const array_ptr = self.loadMemory(8, @intCast(address));
        const len = self.loadMemory(8, @intCast(address + 8));
        const capacity = self.loadMemory(8, @intCast(address + 16));

        return VecData{
            .array_ptr = @intCast(array_ptr),
            .len = @intCast(len),
            .capacity = @intCast(capacity),
        };
    }

    pub fn run(
        self: *VmRuntime,
        program: []ast.Instruction,
        stack: *std.ArrayList(i64),
        bp: *i64,
    ) anyerror!void {
        while (self.pc < program.len) {
            switch (try self.step(program, stack, bp)) {
                .Terminated => {
                    break;
                },
                else => {},
            }
        }
    }
};

test "vm.run" {
    const cases = comptime [_]struct {
        prog: []ast.Instruction,
        initial_stack: []i64 = &[_]i64{},
        initial_bp: i64 = 0,
        expected: []i64,
    }{
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
                .{ .jump_ifzero_d = 3 },
                .{ .push = 0x2 },
                .{ .push = 0x3 },
            }),
            .expected = @constCast(&[_]i64{ 0x2, 0x3 }),
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
    };

    for (cases) |case| {
        var stack = std.ArrayList(i64).init(std.testing.allocator);
        defer stack.deinit();

        var bp: i64 = 0;

        if (case.initial_stack.len > 0) {
            for (case.initial_stack) |item| {
                try stack.append(item);
            }
        }
        if (case.initial_bp > 0) {
            bp = case.initial_bp;
        }

        var vmr = VmRuntime.init(std.testing.allocator);
        try vmr.run(case.prog, &stack, &bp);

        try std.testing.expectEqualSlices(i64, case.expected, stack.items);
    }
}
