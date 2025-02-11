const std = @import("std");

const ast = @import("ast.zig");

const ControlFlow = enum {
    Continue,
    Terminated,

    pub fn isTerminated(self: ControlFlow) bool {
        return self == ControlFlow.Terminated;
    }
};

pub const Vm = struct {
    allocator: std.mem.Allocator,
    ast_arena_allocator: std.heap.ArenaAllocator,
    compiling_context: []const u8 = "",
    prng: std.Random.Xoshiro256,

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
        };
    }

    pub fn deinit(self: *Vm) void {
        self.ast_arena_allocator.deinit();
    }

    pub fn resolveIrLabels(self: *Vm, prog: []ast.Instruction) anyerror!void {
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
                    prog[i] = ast.Instruction{ .jump_d = labels.get(label).? };
                },
                .jump_ifzero => |label| {
                    prog[i] = ast.Instruction{ .jump_ifzero_d = labels.get(label).? };
                },
                .label => {
                    prog[i] = ast.Instruction{ .nop = true };
                },
                else => {},
            }
        }
    }

    fn resolveLocalAddresses(self: *Vm, env: std.StringHashMap(i32), prog: []ast.Instruction) anyerror!void {
        var index: i32 = 0;
        var variables = std.StringHashMap(i32).init(self.allocator);
        defer variables.deinit();

        for (prog) |inst| {
            switch (inst) {
                .set_local => |name| {
                    if (variables.getKey(name)) |_| {} else {
                        try variables.put(name, index);
                        index += 1;
                    }
                },
                else => {},
            }
        }

        for (prog, 0..) |inst, i| {
            switch (inst) {
                .set_local => |name| {
                    if (env.get(name)) |k| {
                        prog[i] = ast.Instruction{ .set_local_d = k };
                    } else if (variables.get(name)) |k| {
                        prog[i] = ast.Instruction{ .set_local_d = k };
                    } else {
                        std.debug.print("Variable not found: {s}\n", .{name});
                        return error.CannotCompileToIr;
                    }
                },
                .get_local => |name| {
                    if (env.get(name)) |k| {
                        prog[i] = ast.Instruction{ .get_local_d = k };
                    } else if (variables.get(name)) |k| {
                        prog[i] = ast.Instruction{ .get_local_d = k };
                    } else {
                        std.debug.print("Variable not found: {s}\n", .{name});
                        return error.CannotCompileToIr;
                    }
                },
                else => {},
            }
        }
    }

    fn compileExprFromAst(self: *Vm, buffer: *std.ArrayList(ast.Instruction), expr: ast.Expression) anyerror!void {
        switch (expr) {
            .var_ => |v| {
                try buffer.append(ast.Instruction{ .get_local = v });
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
                    else => {
                        unreachable;
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
                if (std.mem.eql(u8, call.name, self.compiling_context)) {
                    // should use .call for this place
                    try buffer.append(ast.Instruction{ .call_d = 0 });
                } else {
                    try buffer.append(ast.Instruction{ .call = call.name });
                }

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
            else => {
                unreachable;
            },
        }
    }

    fn compileBlockFromAst(self: *Vm, buffer: *std.ArrayList(ast.Instruction), block: ast.Block) anyerror!void {
        for (block.statements) |stmt| {
            switch (stmt) {
                .let => |let| {
                    try self.compileExprFromAst(buffer, let.value);
                    try buffer.append(ast.Instruction{ .set_local = let.name });
                },
                .return_ => |val| {
                    try self.compileExprFromAst(buffer, val);

                    // epilogue
                    try buffer.append(ast.Instruction{ .set_local = "return" });

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
                    try self.compileExprFromAst(buffer, assign.value);
                    try buffer.append(ast.Instruction{ .set_local = assign.name });
                },
            }
        }
    }

    pub fn compile(
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

    pub fn compileModule(
        self: *Vm,
        entrypoint: []const u8,
        module: ast.Module,
    ) anyerror![]ast.Instruction {
        var buffer = std.ArrayList(ast.Instruction).init(self.ast_arena_allocator.allocator());
        try self.compileExprFromAst(&buffer, ast.Expression{
            .call = .{
                .name = entrypoint,
                .args = &[_]ast.Expression{},
            },
        });

        for (module.decls) |decl| {
            switch (decl) {
                .fun => |f| {
                    try buffer.append(ast.Instruction{ .label = f.name });

                    self.compiling_context = f.name;

                    try buffer.appendSlice(try self.compile(f.name, f.body));

                    self.compiling_context = "";
                },
            }
        }

        return buffer.items;
    }

    pub fn resolveLocals(self: *Vm, params: []const []const u8, prog: []ast.Instruction) anyerror!void {
        var env = std.StringHashMap(i32).init(self.allocator);
        defer env.deinit();

        const l: i32 = @intCast(params.len);
        try env.put("return", -2 - l - 1);

        for (0..params.len) |i| {
            const k: i32 = @intCast(params.len - i);
            try env.put(params[i], -2 - k);
        }

        try self.resolveLocalAddresses(env, prog);
    }
};

pub const VmRuntime = struct {
    target_label: ?[]const u8 = null,
    pc: usize = 0,

    pub fn init() VmRuntime {
        return VmRuntime{};
    }

    fn find_label(self: *VmRuntime, program: []ast.Instruction) anyerror!?usize {
        var count: usize = 0;
        while (self.target_label) |t| {
            count += 1;

            if (count > program.len) {
                return null;
            }

            switch (program[count]) {
                .label => |l| {
                    if (std.mem.eql(u8, l, t)) {
                        self.target_label = null;
                        return count;
                    }
                },
                else => {},
            }
        }

        return null;
    }

    pub fn step(
        self: *VmRuntime,
        program: []ast.Instruction,
        stack: *std.ArrayList(i64),
        bp: *i64,
        address_map: *std.StringHashMap(i64),
    ) anyerror!ControlFlow {
        if (self.pc >= program.len) {
            return ControlFlow.Terminated;
        }

        const inst = program[self.pc];

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
                self.target_label = label;
                self.pc = (try self.find_label(program)).?;
            },
            .jump_ifzero => |label| {
                const cond = stack.pop();
                if (cond == 0) {
                    self.target_label = label;
                    self.pc = (try self.find_label(program)).?;
                }

                self.pc += 1;
            },
            .jump_d => |c| {
                self.pc = c;
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
                self.target_label = label;
                self.pc = (try self.find_label(program)).?;
            },
            .get_local => |name| {
                const index = address_map.get(name).?;
                const b: i32 = @intCast(bp.*);
                const value = stack.items[@intCast(b + index)];
                try stack.append(value);
                self.pc += 1;
            },
            .set_local => |name| {
                const value = stack.pop();

                const result = try address_map.getOrPut(name);
                if (!result.found_existing) {
                    const i = stack.items.len;
                    const v = @as(i32, @intCast(i)) - @as(i32, @intCast(bp.*));
                    result.value_ptr.* = v;

                    try stack.append(0);
                    try address_map.put(name, @intCast(v));
                }

                const b: i32 = @intCast(bp.*);
                stack.items[@intCast(b + result.value_ptr.*)] = value;
                self.pc += 1;
            },
            .get_local_d => |k| {
                const b: i32 = @intCast(bp.*);
                const value = stack.items[@intCast(b + k)];
                try stack.append(value);
                self.pc += 1;
            },
            .set_local_d => |k| {
                const value = stack.pop();
                const b: i32 = @intCast(bp.*);
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
        }

        return ControlFlow.Continue;
    }

    pub fn run(
        self: *VmRuntime,
        program: []ast.Instruction,
        stack: *std.ArrayList(i64),
        bp: *i64,
        address_map: *std.StringHashMap(i64),
    ) anyerror!void {
        while (self.pc < program.len) {
            switch (try self.step(program, stack, bp, address_map)) {
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
        var address_map = std.StringHashMap(i64).init(std.testing.allocator);
        defer address_map.deinit();

        if (case.initial_stack.len > 0) {
            for (case.initial_stack) |item| {
                try stack.append(item);
            }
        }
        if (case.initial_bp > 0) {
            bp = case.initial_bp;
        }

        try Vm.run(case.prog, &stack, &bp, &address_map);

        try std.testing.expectEqualSlices(i64, case.expected, stack.items);
    }
}
