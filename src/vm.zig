const std = @import("std");

const ast = @import("ast.zig");
const jit = @import("jit.zig");

const ControlFlow = enum {
    Continue,
    Terminated,

    pub fn isTerminated(self: ControlFlow) bool {
        return self == ControlFlow.Terminated;
    }
};

pub const VmError = error{
    VariableNotFound,
    LabelNotFound,
};

// This is a compiler for AOT compilation
pub const Vm = struct {
    allocator: std.mem.Allocator,
    ast_arena_allocator: std.heap.ArenaAllocator,
    compiling_context: []const u8 = "",
    prng: std.Random.Xoshiro256,
    env: std.StringHashMap(i32),
    env_offset: i32 = 0,
    string_data: std.StringHashMap(i32),
    string_data_offset: usize = 0,

    pub fn init(allocator: std.mem.Allocator) Vm {
        const prng = std.rand.DefaultPrng.init(blk: {
            var seed: u64 = undefined;
            std.posix.getrandom(std.mem.asBytes(&seed)) catch unreachable;
            break :blk seed;
        });

        return Vm{
            .allocator = allocator,
            .ast_arena_allocator = std.heap.ArenaAllocator.init(allocator),
            .prng = prng,
            .env = std.StringHashMap(i32).init(allocator),
            .env_offset = 0,
            .string_data = std.StringHashMap(i32).init(allocator),
            .string_data_offset = 0,
        };
    }

    pub fn deinit(self: *Vm) void {
        self.ast_arena_allocator.deinit();
        self.env.deinit();
        self.string_data.deinit();
    }

    /// This function MUST NOT move the label positions.
    pub fn resolveIrLabels(
        self: *Vm,
        prog: []ast.Instruction,
        label_fallback_position: ?usize,
    ) anyerror!void {
        var labels = std.StringHashMap(usize).init(self.allocator);
        defer labels.deinit();

        for (prog, 0..) |inst, i| {
            switch (inst) {
                .label => |name| {
                    try labels.put(name, i);
                },
                else => {},
            }
        }

        for (prog, 0..) |inst, i| {
            switch (inst) {
                .jump => |label| {
                    var target: usize = undefined;
                    if (labels.get(label)) |t| {
                        target = t;
                    } else if (label_fallback_position) |fallback| {
                        target = fallback;
                    } else {
                        std.log.err("Label not found: {s}", .{label});
                        return error.LabelNotFound;
                    }
                    prog[i] = ast.Instruction{ .jump_d = target };
                },
                .jump_ifzero => |label| {
                    var target: usize = undefined;
                    if (labels.get(label)) |t| {
                        target = t;
                    } else if (label_fallback_position) |fallback| {
                        target = fallback;
                    } else {
                        std.log.err("Label not found: {s}", .{label});
                        return error.LabelNotFound;
                    }
                    prog[i] = ast.Instruction{ .jump_ifzero_d = target };
                },
                .label => {
                    prog[i] = ast.Instruction{ .nop = true };
                },
                .call => |label| {
                    var target: usize = undefined;
                    if (labels.get(label)) |t| {
                        target = t;
                    } else if (label_fallback_position) |fallback| {
                        target = fallback;
                    } else {
                        std.log.err("Label not found: {s}", .{label});
                        return error.LabelNotFound;
                    }
                    prog[i] = ast.Instruction{ .call_d = target };
                },
                else => {},
            }
        }
    }

    fn compileExprFromAst(self: *Vm, buffer: *std.ArrayList(ast.Instruction), expr: ast.Expression) anyerror!void {
        switch (expr) {
            .var_ => |v| {
                if (self.env.get(v)) |k| {
                    try buffer.append(ast.Instruction{ .get_local_d = k });
                } else {
                    std.log.err("Variable not found: {s}\n", .{v});
                    return error.VariableNotFound;
                }
            },
            .literal => |lit| {
                switch (lit) {
                    .number => |n| {
                        try buffer.append(ast.Instruction{ .push = @intCast(n) });
                    },
                    .boolean => |b| {
                        if (b) {
                            try buffer.append(ast.Instruction{ .push = 1 });
                        } else {
                            try buffer.append(ast.Instruction{ .push = 0 });
                        }
                    },
                    .string => |s| {
                        const i = self.string_data_offset;
                        try self.string_data.put(s, @intCast(i));
                        self.string_data_offset += s.len + 1; // +1 for null terminator

                        // address of the string
                        try buffer.append(ast.Instruction{ .push = @intCast(i) });
                    },
                }
            },
            .binop => |binop| {
                try self.compileExprFromAst(buffer, binop.lhs.*);
                try self.compileExprFromAst(buffer, binop.rhs.*);

                switch (binop.op) {
                    .plus => {
                        try buffer.append(ast.Instruction{ .add = true });
                    },
                    .minus => {
                        try buffer.append(ast.Instruction{ .sub = true });
                    },
                    .star => {
                        try buffer.append(ast.Instruction{ .mul = true });
                    },
                    .eqeq => {
                        try buffer.append(ast.Instruction{ .eq = true });
                    },
                    .percent => {
                        try buffer.append(ast.Instruction{ .mod = true });
                    },
                    .langle => {
                        try buffer.append(ast.Instruction{ .lt = true });
                    },
                    .lte => {
                        try buffer.append(ast.Instruction{ .lte = true });
                    },
                    .rangle => {
                        try buffer.append(ast.Instruction{ .gt = true });
                    },
                    .gte => {
                        try buffer.append(ast.Instruction{ .gte = true });
                    },
                    else => {
                        std.log.info("op: {}\n", .{binop.op});
                        unreachable;
                    },
                }
            },
            .call => |call| {
                // return value
                try buffer.append(ast.Instruction{ .push = -2 });

                // order from left to right
                for (call.args) |arg| {
                    try self.compileExprFromAst(buffer, arg);
                }

                // prologue
                try buffer.append(ast.Instruction{ .get_pc = true });
                try buffer.append(ast.Instruction{ .get_bp = true });
                try buffer.append(ast.Instruction{ .get_sp = true });
                try buffer.append(ast.Instruction{ .set_bp = true });

                // call
                try buffer.append(ast.Instruction{ .call = call.name });

                for (call.args) |_| {
                    try buffer.append(ast.Instruction{ .pop = true });
                }
            },
            .if_ => |if_| {
                try self.compileExprFromAst(buffer, if_.cond.*);

                const id = self.prng.random().int(u32);

                const label_ifthen = try std.fmt.allocPrint(self.ast_arena_allocator.allocator(), "if_then_{d}", .{id});
                const label_ifelse = try std.fmt.allocPrint(self.ast_arena_allocator.allocator(), "if_else_{d}", .{id});
                const label_ifend = try std.fmt.allocPrint(self.ast_arena_allocator.allocator(), "if_end_{d}", .{id});

                try buffer.append(ast.Instruction{ .jump_ifzero = label_ifelse });
                try buffer.append(ast.Instruction{ .label = label_ifthen });
                try self.compileBlockFromAst(buffer, if_.then_);
                try buffer.append(ast.Instruction{ .jump = label_ifend });
                try buffer.append(ast.Instruction{ .label = label_ifelse });
                try self.compileBlockFromAst(buffer, if_.else_);
                try buffer.append(ast.Instruction{ .label = label_ifend });
            },
            .block => {
                unreachable;
            },
            .index => |index| {
                const size = index.elem_type.size();

                try self.compileExprFromAst(buffer, index.lhs.*);
                try self.compileExprFromAst(buffer, index.rhs.*);

                try buffer.append(ast.Instruction{ .push = size });
                try buffer.append(ast.Instruction{ .mul = true });
                try buffer.append(ast.Instruction{ .add = true });

                try buffer.append(ast.Instruction{ .load = size });
            },
            .new => |new| {
                const array_size = new.array_size;
                try buffer.append(ast.Instruction{ .allocate_memory = array_size * 4 });

                std.debug.assert(new.initializers.len == 0);
            },
        }
    }

    fn compileLhsExprFromAst(self: *Vm, buffer: *std.ArrayList(ast.Instruction), expr: ast.Expression) anyerror!void {
        switch (expr) {
            .index => |index| {
                const size = index.elem_type.size();

                try self.compileExprFromAst(buffer, index.lhs.*);
                try self.compileExprFromAst(buffer, index.rhs.*);
                try buffer.append(ast.Instruction{ .push = size });
                try buffer.append(ast.Instruction{ .mul = true });
                try buffer.append(ast.Instruction{ .add = true });
            },
            else => {
                unreachable;
            },
        }
    }

    fn compileStatementFromAst(self: *Vm, buffer: *std.ArrayList(ast.Instruction), stmt: ast.Statement) anyerror!void {
        switch (stmt) {
            .let => |let| {
                const i = self.env_offset;
                try self.env.put(let.name, i);
                self.env_offset += 1;

                try self.compileExprFromAst(buffer, let.value);
            },
            .return_ => |val| {
                try self.compileExprFromAst(buffer, val);

                // epilogue
                if (self.env.get("return")) |k| {
                    try buffer.append(ast.Instruction{ .set_local_d = k });
                } else {
                    var keys = std.ArrayList([]const u8).init(self.allocator);
                    defer keys.deinit();

                    var iter = self.env.keyIterator();
                    while (iter.next()) |k| {
                        try keys.append(k.*);
                    }

                    std.log.err("Cannot find return value position in {any}", .{keys.items});
                    return error.VariableNotFound;
                }

                try buffer.append(ast.Instruction{ .get_bp = true });
                try buffer.append(ast.Instruction{ .set_sp = true });
                try buffer.append(ast.Instruction{ .set_bp = true });
                try buffer.append(ast.Instruction{ .ret = true });
            },
            .expr => |expr| {
                try self.compileExprFromAst(buffer, expr);
                try buffer.append(ast.Instruction{ .pop = true });
            },
            .if_ => |if_| {
                try self.compileExprFromAst(buffer, if_.cond.*);

                const id = self.prng.random().int(u32);

                const label_ifthen = try std.fmt.allocPrint(self.ast_arena_allocator.allocator(), "if_then_{d}", .{id});
                const label_ifelse = try std.fmt.allocPrint(self.ast_arena_allocator.allocator(), "if_else_{d}", .{id});
                const label_ifend = try std.fmt.allocPrint(self.ast_arena_allocator.allocator(), "if_end_{d}", .{id});

                try buffer.append(ast.Instruction{ .jump_ifzero = label_ifelse });
                try buffer.append(ast.Instruction{ .label = label_ifthen });
                try self.compileBlockFromAst(buffer, if_.then_);
                try buffer.append(ast.Instruction{ .jump = label_ifend });
                try buffer.append(ast.Instruction{ .label = label_ifelse });
                if (if_.else_) |else_| {
                    try self.compileBlockFromAst(buffer, else_);
                }
                try buffer.append(ast.Instruction{ .label = label_ifend });
            },
            .while_ => |while_| {
                const id = self.prng.random().int(u32);

                const label_whilecond = try std.fmt.allocPrint(self.ast_arena_allocator.allocator(), "while_cond_{d}", .{id});
                const label_whilebody = try std.fmt.allocPrint(self.ast_arena_allocator.allocator(), "while_body_{d}", .{id});
                const label_whileend = try std.fmt.allocPrint(self.ast_arena_allocator.allocator(), "while_end_{d}", .{id});

                try buffer.append(ast.Instruction{ .label = label_whilecond });
                try self.compileExprFromAst(buffer, while_.cond.*);
                try buffer.append(ast.Instruction{ .jump_ifzero = label_whileend });
                try buffer.append(ast.Instruction{ .label = label_whilebody });
                try self.compileBlockFromAst(buffer, while_.body);
                try buffer.append(ast.Instruction{ .jump = label_whilecond });
                try buffer.append(ast.Instruction{ .label = label_whileend });
            },
            .assign => |assign| {
                try self.compileExprFromAst(buffer, assign.rhs);

                switch (assign.lhs) {
                    .var_ => |name| {
                        if (self.env.get(name)) |k| {
                            try buffer.append(ast.Instruction{ .set_local_d = k });
                        } else {
                            std.log.err("Variable not found: {s}\n", .{name});
                            return error.VariableNotFound;
                        }
                    },
                    else => {
                        try self.compileLhsExprFromAst(buffer, assign.lhs);
                        try self.compileExprFromAst(buffer, assign.rhs);
                        try buffer.append(ast.Instruction{ .store = assign.type_.size() });
                    },
                }
            },
        }
    }

    fn compileBlockFromAst(self: *Vm, buffer: *std.ArrayList(ast.Instruction), block: ast.Block) anyerror!void {
        const offset = self.env_offset;

        for (block.statements) |stmt| {
            try self.compileStatementFromAst(buffer, stmt);
        }

        std.debug.assert(self.env_offset >= offset);
        for (0..@intCast(self.env_offset - offset)) |_| {
            try buffer.append(ast.Instruction{ .pop = true });
        }
        self.env_offset = offset;
    }

    pub fn compileBlock(
        self: *Vm,
        entrypoint: []const u8,
        body: ast.Block,
    ) anyerror![]ast.Instruction {
        self.compiling_context = entrypoint;

        var buffer = std.ArrayList(ast.Instruction).init(self.ast_arena_allocator.allocator());
        try self.compileBlockFromAst(&buffer, body);

        self.compiling_context = "";

        return buffer.items;
    }

    pub fn compile(
        self: *Vm,
        entrypoint: []const u8,
        module: ast.Module,
    ) anyerror![]ast.Instruction {
        self.env_offset = 0;
        self.env.clearAndFree();

        var progBuffer = std.ArrayList(ast.Instruction).init(self.ast_arena_allocator.allocator());
        defer progBuffer.deinit();

        try self.env.put("return", -3);

        try self.compileBlockFromAst(&progBuffer, ast.Block{
            .statements = @constCast(&[_]ast.Statement{
                .{
                    .return_ = .{
                        .call = .{
                            .name = entrypoint,
                            .args = &[_]ast.Expression{},
                        },
                    },
                },
            }),
            .expr = null,
        });

        for (module.decls) |decl| {
            switch (decl) {
                .fun => |f| {
                    self.env_offset = 0;
                    self.env.clearAndFree();

                    try progBuffer.append(ast.Instruction{ .label = f.name });

                    try self.env.put("return", -@as(i32, @intCast(f.params.len)) - 3);

                    // register names in the reverse order
                    for (f.params, 0..) |param, i| {
                        try self.env.put(param, -@as(i32, @intCast(f.params.len - i)) - 2);
                    }

                    self.compiling_context = f.name;

                    try progBuffer.appendSlice(try self.compileBlock(f.name, f.body));

                    self.compiling_context = "";

                    const end_of_f = try std.fmt.allocPrint(self.ast_arena_allocator.allocator(), "end_of_{s}", .{f.name});
                    try progBuffer.append(ast.Instruction{ .label = end_of_f });
                },
            }
        }

        var dataBuffer = std.ArrayList(ast.Instruction).init(self.ast_arena_allocator.allocator());

        var iter = self.string_data.keyIterator();
        while (iter.next()) |k| {
            const offset = self.string_data.get(k.*).?;
            try dataBuffer.append(ast.Instruction{ .set_memory = .{ .data = k.*, .offset = @intCast(offset) } });
        }

        try dataBuffer.appendSlice(progBuffer.items);

        return dataBuffer.items;
    }
};

pub const VmRuntimeError = error{
    LabelNotFound,
    AssertionFailed,
};

pub const VmRuntime = struct {
    pc: usize,
    envs: std.ArrayList(std.StringHashMap(i64)),
    enable_jit: bool,
    hot_spot_labels: std.StringHashMap(usize),
    traces: ?std.ArrayList(ast.Instruction),
    jit_cache: std.StringHashMap(jit.CompiledFn),
    allocator: std.mem.Allocator,
    memory: []u8,
    hp: usize = 0,

    pub fn init(allocator: std.mem.Allocator) VmRuntime {
        return VmRuntime{
            .pc = 0,
            .envs = std.ArrayList(std.StringHashMap(i64)).init(allocator),
            .hot_spot_labels = std.StringHashMap(usize).init(allocator),
            .traces = null,
            .jit_cache = std.StringHashMap(jit.CompiledFn).init(allocator),
            .enable_jit = true,
            .allocator = allocator,
            .memory = std.heap.page_allocator.alloc(u8, 1024 * 1024) catch unreachable,
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
                std.log.warn("Tracing limit exceeded\n", .{});

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
                const entry = try self.hot_spot_labels.getOrPutValue(label, 0);
                entry.value_ptr.* += 1;

                const pc = self.pc;

                const target = (try VmRuntime.find_label(program, label)).?;
                self.pc = target;

                if (self.jit_cache.contains(label)) {
                    // TODO
                } else if (entry.value_ptr.* >= 100) {
                    // When tracing is finished
                    if (self.traces) |traces| {
                        if (std.mem.eql(u8, traces.items[0].label, label)) {
                            // Only supports: [label, ..., jump label] fragment
                            std.debug.assert(std.mem.eql(u8, traces.items[0].label, label));
                            std.debug.assert(std.mem.eql(u8, traces.items[traces.items.len - 1].jump, label));

                            var vmc = Vm.init(self.allocator);
                            defer vmc.deinit();

                            var ir_block = std.ArrayList(ast.Instruction).init(self.allocator);
                            defer ir_block.deinit();

                            try ir_block.appendSlice(traces.items);

                            const fallback_position = ir_block.items.len;

                            // add fallback block (when label not found)
                            try ir_block.append(ast.Instruction{ .ret = true });

                            try vmc.resolveIrLabels(ir_block.items, fallback_position);

                            std.log.info("Tracing & compile finished, {d}", .{ir_block.items.len});

                            var runtime = jit.JitRuntime.init(self.allocator);
                            const f = try runtime.compile(ir_block.items);

                            try self.jit_cache.put(label, f);

                            self.traces.?.deinit();
                            self.traces = null;
                        }
                    } else {
                        // When jumping backwords
                        if (target < pc and pc - target >= 10) {
                            // start tracing
                            self.traces = std.ArrayList(ast.Instruction).init(self.allocator);
                        }
                    }
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

                    if (entry.value_ptr.* > 10) {
                        var fn_ptr: jit.CompiledFn = undefined;

                        if (self.jit_cache.get(label)) |f| {
                            fn_ptr = f;
                        } else {
                            const start = try std.time.Instant.now();

                            const call_block_start = try VmRuntime.find_label(program, label);

                            const end_label = try std.fmt.allocPrint(self.allocator, "end_of_{s}", .{label});
                            defer self.allocator.free(end_label);
                            const call_block_end = try VmRuntime.find_label(program, end_label);

                            var ir_block_list = std.ArrayList(ast.Instruction).init(self.allocator);
                            defer ir_block_list.deinit();

                            try ir_block_list.appendSlice(program[call_block_start.?..call_block_end.?]);
                            const ir_block = ir_block_list.items;

                            var vmc = Vm.init(self.allocator);
                            defer vmc.deinit();

                            try vmc.resolveIrLabels(ir_block, null);

                            var params = std.ArrayList([]const u8).init(self.allocator);
                            defer params.deinit();

                            var runtime = jit.JitRuntime.init(self.allocator);
                            const f = runtime.compile(ir_block) catch |err| {
                                std.debug.print("JIT compile error, fallback to VM execution: {any}\n", .{err});

                                unreachable;
                            };

                            const end = try std.time.Instant.now();
                            const elapsed: f64 = @floatFromInt(end.since(start));
                            std.debug.print("Compiled(jit) in {d:.3}ms {s}\n", .{ elapsed / std.time.ns_per_ms, label });

                            try self.jit_cache.put(label, f);

                            fn_ptr = f;
                        }

                        var sp = @as(i64, @intCast(stack.items.len));

                        fn_ptr((&stack.items).ptr, &sp, bp);

                        // epilogue here
                        self.pc += 1;

                        stack.shrinkAndFree(@intCast(sp));

                        return ControlFlow.Continue;
                    }
                } else {
                    entry.value_ptr.* = 1;
                }

                self.pc = (try VmRuntime.find_label(program, label)).?;
            },
            .get_local_d => |k| {
                const b: i32 = @intCast(bp.*);
                if (b + k >= stack.items.len) {
                    std.log.err("Invalid address: {d} at {any}\n", .{ b + k, stack.items });
                    return error.AssertionFailed;
                }

                const value = stack.items[@intCast(b + k)];
                try stack.append(value);
                self.pc += 1;
            },
            .set_local_d => |k| {
                const value = stack.pop();
                const b: i32 = @intCast(bp.*);
                if (b + k >= stack.items.len) {
                    std.log.err("Invalid address: {d} at {any}\n", .{ b + k, stack.items });
                    return error.AssertionFailed;
                }

                stack.items[@intCast(b + k)] = value;
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

                try stack.append(n);

                self.pc += 1;
            },
            .store => |size| {
                const value = stack.pop();
                const addr = stack.pop();
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
                self.pc += 1;
            },
            .set_memory => |m| {
                const addr = m.offset;
                const data = m.data;
                for (data, 0..) |d, i| {
                    self.memory[addr + i] = d;
                }
                self.hp = addr + data.len + 1;
                self.pc += 1;
            },
            .allocate_memory => |size| {
                const hp = self.hp;
                self.hp += size;

                try stack.append(@intCast(hp));
                self.pc += 1;
            },
        }

        return ControlFlow.Continue;
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
