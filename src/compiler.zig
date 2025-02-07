const std = @import("std");

const ast = @import("ast.zig");
const lexer = @import("lexer.zig");
const parser = @import("parser.zig");
const jit = @import("jit.zig");

pub const EvalError = error{
    VariableNotFound,
    UnexpectedNilValue,
};

pub const CompilerError = error{
    CannotCompileToIr,
};

const Env = std.StringHashMap(ast.Value);

pub const Compiler = struct {
    env: Env,
    module: ?ast.Module,
    has_returned: bool,
    call_trace: std.StringHashMap(u32),
    ir_cache: std.StringHashMap([]ast.Instruction),
    jit_cache: std.StringHashMap(jit.CompiledFn),
    allocator: std.mem.Allocator,
    ast_arena_allocator: std.heap.ArenaAllocator,
    compiling_context: []const u8,
    prng: std.Random.Xoshiro256,

    pub fn init(allocator: std.mem.Allocator) Compiler {
        const prng = std.rand.DefaultPrng.init(blk: {
            var seed: u64 = undefined;
            std.posix.getrandom(std.mem.asBytes(&seed)) catch unreachable;
            break :blk seed;
        });

        return Compiler{
            .env = Env.init(allocator),
            .module = null,
            .has_returned = false,
            .call_trace = std.StringHashMap(u32).init(allocator),
            .ir_cache = std.StringHashMap([]ast.Instruction).init(allocator),
            .jit_cache = std.StringHashMap(jit.CompiledFn).init(allocator),
            .allocator = allocator,
            .ast_arena_allocator = std.heap.ArenaAllocator.init(allocator),
            .compiling_context = "",
            .prng = prng,
        };
    }

    pub fn deinit(self: *Compiler) void {
        self.env.deinit();
        self.call_trace.deinit();
        self.ir_cache.deinit();
        self.jit_cache.deinit();
        self.ast_arena_allocator.deinit();
    }

    fn evalExpr(self: *Compiler, str: []const u8) anyerror!ast.Value {
        var l = lexer.Lexer.init(self.allocator, str);
        defer l.deinit();

        const tokens = try l.run();
        var p = parser.Parser.init(self.allocator, tokens.items);
        defer p.deinit();

        const tree = try p.expr();

        return self.evalExprFromAst(tree);
    }

    fn evalBlock(self: *Compiler, str: []const u8) anyerror!?ast.Value {
        var l = lexer.Lexer.init(self.allocator, str);
        defer l.deinit();

        const tokens = try l.run();
        var p = parser.Parser.init(self.allocator, tokens.items);
        defer p.deinit();

        const tree = try p.block(null);

        return try self.evalBlockFromAst(tree);
    }

    pub fn evalModule(self: *Compiler, str: []const u8) anyerror!?ast.Value {
        var start = try std.time.Instant.now();

        var l = lexer.Lexer.init(self.allocator, str);
        defer l.deinit();

        const tokens = try l.run();

        var end = try std.time.Instant.now();
        std.debug.print("Lexer.run in {d:.3}ms\n", .{@as(f64, @floatFromInt(end.since(start))) / std.time.ns_per_ms});
        start = end;

        var p = parser.Parser.init(self.allocator, tokens.items);
        defer p.deinit();

        end = try std.time.Instant.now();
        std.debug.print("Parser.run in {d:.3}ms\n", .{@as(f64, @floatFromInt(end.since(start))) / std.time.ns_per_ms});
        start = end;

        const tree = try p.module();

        self.module = tree;

        const result = try self.evalModuleFromAst("main");

        end = try std.time.Instant.now();
        std.debug.print("Compiler.eval in {d:.3}ms\n", .{@as(f64, @floatFromInt(end.since(start))) / std.time.ns_per_ms});
        start = end;

        return result;
    }

    fn compileExprFromAst(self: *Compiler, buffer: *std.ArrayList(ast.Instruction), expr: ast.Expression) anyerror!void {
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
                    try buffer.append(ast.Instruction{ .call = 0 });
                } else {
                    return error.CannotCompileToIr;
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

    fn compileBlockFromAst(self: *Compiler, buffer: *std.ArrayList(ast.Instruction), block: ast.Block) anyerror!void {
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

    fn resolveIrLabels(self: *Compiler, prog: []ast.Instruction) anyerror!void {
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

    fn resolveLocalAddresses(self: *Compiler, env: std.StringHashMap(i32), prog: []ast.Instruction) anyerror!void {
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

    fn runVm(
        program: []ast.Instruction,
        stack: *std.ArrayList(i64),
        bp: *i64,
        address_map: *std.StringHashMap(i64),
    ) anyerror!void {
        var target_label: ?[]const u8 = null;

        var pc: usize = 0;

        while (pc < program.len) {
            const inst = program[pc];

            if (target_label) |t| {
                switch (inst) {
                    .label => |l| {
                        if (std.mem.eql(u8, l, t)) {
                            target_label = null;
                            pc += 1;
                            continue;
                        }
                    },
                    else => {
                        pc += 1;
                        continue;
                    },
                }
            }

            // std.debug.print("[{any},pc:{d},bp:{d}] {any}\n", .{ inst, pc, bp.*, stack.items });

            switch (inst) {
                .nop => {
                    pc += 1;
                },
                .push => |n| {
                    try stack.append(n);
                    pc += 1;
                },
                .pop => {
                    _ = stack.pop();
                    pc += 1;
                },
                .eq => {
                    const rhs = stack.pop();
                    const lhs = stack.pop();
                    if (lhs == rhs) {
                        try stack.append(1);
                    } else {
                        try stack.append(0);
                    }
                    pc += 1;
                },
                .ret => {
                    const p = stack.pop();
                    if (p == -1) {
                        return;
                    } else {
                        pc = @intCast(p + 5);
                    }
                },
                .jump => |label| {
                    target_label = label;
                    pc += 1;
                },
                .jump_ifzero => |label| {
                    const cond = stack.pop();
                    if (cond == 0) {
                        target_label = label;
                    }

                    pc += 1;
                },
                .jump_d => |c| {
                    pc = c;
                },
                .jump_ifzero_d => |c| {
                    const cond = stack.pop();
                    if (cond == 0) {
                        pc = c;
                    } else {
                        pc += 1;
                    }
                },
                .add => {
                    const rhs = stack.pop();
                    const lhs = stack.pop();
                    try stack.append(lhs + rhs);

                    pc += 1;
                },
                .sub => {
                    const rhs = stack.pop();
                    const lhs = stack.pop();
                    try stack.append(lhs - rhs);

                    pc += 1;
                },
                .mul => {
                    const rhs = stack.pop();
                    const lhs = stack.pop();
                    try stack.append(lhs * rhs);

                    pc += 1;
                },
                .call => |addr| {
                    pc = addr;
                },
                .get_local => |name| {
                    const index = address_map.get(name).?;
                    const b: i32 = @intCast(bp.*);
                    const value = stack.items[@intCast(b + index)];
                    try stack.append(value);
                    pc += 1;
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
                    pc += 1;
                },
                .get_local_d => |k| {
                    const b: i32 = @intCast(bp.*);
                    const value = stack.items[@intCast(b + k)];
                    try stack.append(value);
                    pc += 1;
                },
                .set_local_d => |k| {
                    const value = stack.pop();
                    const b: i32 = @intCast(bp.*);
                    stack.items[@intCast(b + k)] = value;
                    pc += 1;
                },
                .label => {
                    pc += 1;
                },
                .get_pc => {
                    try stack.append(@intCast(pc));
                    pc += 1;
                },
                .get_bp => {
                    try stack.append(@intCast(bp.*));
                    pc += 1;
                },
                .set_bp => {
                    const value = stack.pop();
                    bp.* = @intCast(value);
                    pc += 1;
                },
                .get_sp => {
                    try stack.append(@intCast(stack.items.len));
                    pc += 1;
                },
                .set_sp => {
                    const value = stack.pop();
                    stack.shrinkAndFree(@intCast(value));
                    pc += 1;
                },
                .mod => {
                    const rhs = stack.pop();
                    const lhs = stack.pop();
                    try stack.append(@mod(lhs, rhs));

                    pc += 1;
                },
                .lt => {
                    const rhs = stack.pop();
                    const lhs = stack.pop();
                    if (lhs < rhs) {
                        try stack.append(1);
                    } else {
                        try stack.append(0);
                    }
                    pc += 1;
                },
                .lte => {
                    const rhs = stack.pop();
                    const lhs = stack.pop();
                    if (lhs <= rhs) {
                        try stack.append(1);
                    } else {
                        try stack.append(0);
                    }
                    pc += 1;
                },
            }
        }
    }

    fn evalExprFromAst(self: *Compiler, expr: ast.Expression) anyerror!ast.Value {
        switch (expr) {
            .var_ => |v| {
                if (self.env.get(v)) |value| {
                    return value;
                } else {
                    std.debug.print("Variable not found: {s}\n", .{v});
                    return error.VariableNotFound;
                }
            },
            .literal => |lit| {
                switch (lit) {
                    .number => |n| {
                        return ast.Value{ .i64_ = @intCast(n) };
                    },
                    .boolean => |b| {
                        return ast.Value{ .bool_ = b };
                    },
                    else => {
                        unreachable;
                    },
                }
            },
            .binop => |binop| {
                switch (binop.op) {
                    .plus => {
                        const lhs = try self.evalExprFromAst(binop.lhs.*);
                        const rhs = try self.evalExprFromAst(binop.rhs.*);
                        return ast.Value{ .i64_ = try lhs.asI64() + try rhs.asI64() };
                    },
                    .minus => {
                        const lhs = try self.evalExprFromAst(binop.lhs.*);
                        const rhs = try self.evalExprFromAst(binop.rhs.*);
                        return ast.Value{ .i64_ = try lhs.asI64() - try rhs.asI64() };
                    },
                    .star => {
                        const lhs = try self.evalExprFromAst(binop.lhs.*);
                        const rhs = try self.evalExprFromAst(binop.rhs.*);
                        return ast.Value{ .i64_ = try lhs.asI64() * try rhs.asI64() };
                    },
                    .eqeq => {
                        return ast.Value{ .bool_ = std.meta.eql(try self.evalExprFromAst(binop.lhs.*), try self.evalExprFromAst(binop.rhs.*)) };
                    },
                    .langle => {
                        const lhs = try self.evalExprFromAst(binop.lhs.*);
                        const rhs = try self.evalExprFromAst(binop.rhs.*);
                        return ast.Value{ .bool_ = try lhs.asI64() < try rhs.asI64() };
                    },
                    .rangle => {
                        const lhs = try self.evalExprFromAst(binop.lhs.*);
                        const rhs = try self.evalExprFromAst(binop.rhs.*);
                        return ast.Value{ .bool_ = try lhs.asI64() > try rhs.asI64() };
                    },
                    .lte => {
                        const lhs = try self.evalExprFromAst(binop.lhs.*);
                        const rhs = try self.evalExprFromAst(binop.rhs.*);
                        return ast.Value{ .bool_ = try lhs.asI64() <= try rhs.asI64() };
                    },
                    .gte => {
                        const lhs = try self.evalExprFromAst(binop.lhs.*);
                        const rhs = try self.evalExprFromAst(binop.rhs.*);
                        return ast.Value{ .bool_ = try lhs.asI64() >= try rhs.asI64() };
                    },
                    .percent => {
                        const lhs = try self.evalExprFromAst(binop.lhs.*);
                        const rhs = try self.evalExprFromAst(binop.rhs.*);
                        return ast.Value{ .i64_ = @mod(try lhs.asI64(), try rhs.asI64()) };
                    },
                    else => {
                        unreachable;
                    },
                }
            },
            .call => |call| {
                var args = std.ArrayList(ast.Value).init(self.ast_arena_allocator.allocator());
                defer args.deinit();

                for (call.args) |arg| {
                    try args.append(try self.evalExprFromAst(arg));
                }

                return try self.callFunction(call.name, args.items);
            },
            .if_ => |if_| {
                const cond = try self.evalExprFromAst(if_.cond.*);
                if (try cond.asBool()) {
                    return try self.evalBlockFromAst(if_.then_);
                } else {
                    return try self.evalBlockFromAst(if_.else_);
                }
            },
            else => {
                unreachable;
            },
        }
    }

    fn evalBlockFromAst(self: *Compiler, block: ast.Block) anyerror!ast.Value {
        for (block.statements) |stmt| {
            switch (stmt) {
                .let => |let| {
                    try self.env.put(let.name, try self.evalExprFromAst(let.value));
                },
                .return_ => |val| {
                    self.has_returned = true;
                    return try self.evalExprFromAst(val);
                },
                .if_ => |if_| {
                    const cond = try self.evalExprFromAst(if_.cond.*);
                    if (try cond.asBool()) {
                        const result = try self.evalBlockFromAst(if_.then_);
                        if (self.has_returned) {
                            return result;
                        }
                    } else {
                        if (if_.else_) |else_| {
                            const result = try self.evalBlockFromAst(else_);
                            if (self.has_returned) {
                                return result;
                            }
                        }
                    }
                },
                .expr => |expr| {
                    _ = try self.evalExprFromAst(expr);
                },
                .assign => |assign| {
                    try self.env.put(assign.name, try self.evalExprFromAst(assign.value));
                },
                .while_ => |while_| {
                    while (true) {
                        const cond = try self.evalExprFromAst(while_.cond.*);
                        if (!try cond.asBool()) {
                            break;
                        }

                        const result = try self.evalBlockFromAst(while_.body);
                        if (self.has_returned) {
                            return result;
                        }
                    }
                },
            }
        }

        if (block.expr) |expr| {
            return try self.evalExprFromAst(expr.*);
        }

        return ast.Value{ .nil = true };
    }

    fn evalModuleFromAst(self: *Compiler, entrypoint: []const u8) anyerror!ast.Value {
        return self.callFunction(entrypoint, &[_]ast.Value{});
    }

    const enable_native_jit = true;

    fn callIrFunction(self: *Compiler, name: []const u8, params: []const []const u8, ir: []ast.Instruction, args: []ast.Value) anyerror!ast.Value {
        var stack = std.ArrayList(i64).init(self.allocator);
        defer stack.deinit();

        var address_map = std.StringHashMap(i64).init(self.allocator);
        defer address_map.deinit();

        const l: i32 = @intCast(args.len);
        try address_map.put("return", -2 - l - 1);
        try stack.append(-2); // return value

        for (args, 0..) |arg, i| {
            const k: i32 = @intCast(args.len - i);
            try address_map.put(params[i], -2 - k);
            try stack.append(try arg.asI64());
        }

        try stack.append(-1); // pc
        try stack.append(0); // bp

        if (enable_native_jit) {
            const start = try std.time.Instant.now();

            var bp = @as(i64, @intCast(stack.items.len));

            var runtime = jit.JitRuntime.init(self.allocator);
            const fn_ptr = runtime.compile(ir) catch |err| {
                std.debug.print("JIT compile error, fallback to VM execution: {any}\n", .{err});

                try Compiler.runVm(ir, &stack, &bp, &address_map);
                return ast.Value{ .i64_ = stack.items[0] };
            };

            const end = try std.time.Instant.now();
            const elapsed: f64 = @floatFromInt(end.since(start));
            std.debug.print("Compiled(jit) in {d:.3}ms {s}\n", .{ elapsed / std.time.ns_per_ms, name });

            var sp = @as(i64, @intCast(stack.items.len));
            fn_ptr((&stack.items).ptr, &sp, &bp);

            try self.jit_cache.put(name, fn_ptr);

            return ast.Value{ .i64_ = stack.items[0] };
        } else {
            var bp = stack.items.len;
            try Compiler.runVm(ir, &stack, &bp, &address_map);
            return ast.Value{ .i64_ = stack.items[0] };
        }
    }

    fn callFunction(self: *Compiler, name: []const u8, args: []ast.Value) anyerror!ast.Value {
        var params: []const []const u8 = undefined;
        var body: ast.Block = undefined;
        for (self.module.?.decls) |decl| {
            switch (decl) {
                .fun => |f| {
                    if (std.mem.eql(u8, f.name, name)) {
                        params = f.params;
                        body = f.body;
                    }
                },
            }
        }

        if (self.jit_cache.get(name)) |fn_ptr| {
            std.debug.print("Using JIT cache: {s} {any}\n", .{ name, args });

            var stack = std.ArrayList(i64).init(self.allocator);
            defer stack.deinit();

            var address_map = std.StringHashMap(i32).init(self.allocator);
            defer address_map.deinit();

            const l: i32 = @intCast(args.len);
            try address_map.put("return", -2 - l - 1);
            try stack.append(-2); // return value

            for (args, 0..) |arg, i| {
                const k: i32 = @intCast(args.len - i);
                try address_map.put(params[i], -2 - k);
                try stack.append(try arg.asI64());
            }

            try stack.append(-1); // pc
            try stack.append(0); // bp

            var sp = @as(i64, @intCast(stack.items.len));
            var bp = @as(i64, @intCast(stack.items.len));
            fn_ptr((&stack.items).ptr, &sp, &bp);

            return ast.Value{ .i64_ = stack.items[0] };
        }

        if (self.ir_cache.get(name)) |prog| {
            std.debug.print("Using IR cache: {s} {any}\n", .{ name, args });

            const value = self.callIrFunction(name, params, prog, args);
            return value;
        }

        if (self.call_trace.get(name)) |count| {
            if (count == 5) {
                const start = try std.time.Instant.now();
                std.debug.print("Start compiling: {s}\n", .{name});

                self.compiling_context = name;

                var env = std.StringHashMap(i32).init(self.allocator);
                defer env.deinit();

                const l: i32 = @intCast(args.len);
                try env.put("return", -2 - l - 1);

                for (args, 0..) |_, i| {
                    const k: i32 = @intCast(args.len - i);
                    try env.put(params[i], -2 - k);
                }

                var buffer = std.ArrayList(ast.Instruction).init(self.ast_arena_allocator.allocator());
                try self.compileBlockFromAst(&buffer, body);
                try self.resolveIrLabels(buffer.items);
                try self.resolveLocalAddresses(env, buffer.items);
                const ir = buffer.items;

                self.compiling_context = "";

                const end = try std.time.Instant.now();
                const elapsed: f64 = @floatFromInt(end.since(start));
                std.debug.print("Compiled in {d:.3}ms: {s}\n", .{ elapsed / std.time.ns_per_ms, name });

                // for (ir) |inst| {
                //     std.debug.print("{any}\n", .{inst});
                // }

                const value = self.callIrFunction(name, params, ir, args);
                try self.ir_cache.put(name, ir);

                return value;
            }

            try self.call_trace.put(name, count + 1);
        } else {
            try self.call_trace.put(name, 1);
        }

        const envCloned = try self.env.clone();

        for (args, 0..) |arg, i| {
            try self.env.put(params[i], arg);
        }

        const value = try self.evalBlockFromAst(body);
        self.has_returned = false;

        self.env.clearAndFree();
        self.env = envCloned;

        return value;
    }
};

test {
    _ = @import("lexer.zig");
    _ = @import("parser.zig");
    _ = @import("jit.zig");
}

test "compiler.parse_err" {
    const cases = comptime [_]struct {
        program: []const u8,
        err: ?anyerror,
    }{
        .{
            .program =
            \\fun f(x) do
            \\  let y = 10;
            \\
            \\  return x + y;
            \\end
            \\
            \\fun main() do
            \\  return f(3);
            \\end
            ,
            .err = null,
        },
        .{
            .program =
            \\fun f(x) do
            \\  if (x == 0) do
            \\    return 1;
            \\  end
            \\end
            ,
            .err = null,
        },
    };

    for (cases) |case| {
        var l = lexer.Lexer.init(std.testing.allocator, case.program);
        defer l.deinit();

        const tokens = try l.run();
        var p = parser.Parser.init(std.testing.allocator, tokens.items);
        defer p.deinit();

        if (case.err != null) {
            _ = p.module() catch |err| {
                try std.testing.expectEqual(err, case.err);
            };
        } else {
            _ = p.module() catch |err| {
                std.debug.print("Unexpected error: {any}\n", .{err});
                try std.testing.expect(false);
            };
        }
    }
}

test "compiler.parseStatement" {
    const cases = comptime [_]struct {
        program: []const u8,
        expr: ast.Statement,
    }{
        .{
            .program =
            \\if (x == 1) do
            \\  return 1;
            \\end
            ,
            .expr = ast.Statement{ .if_ = .{
                .cond = @constCast(&ast.Expression{ .binop = .{
                    .op = ast.Operator.eqeq,
                    .lhs = @constCast(&ast.Expression{ .var_ = "x" }),
                    .rhs = @constCast(&ast.Expression{ .literal = ast.Literal{ .number = 1 } }),
                } }),
                .then_ = .{
                    .statements = @constCast(&[_]ast.Statement{
                        ast.Statement{ .return_ = ast.Expression{ .literal = ast.Literal{ .number = 1 } } },
                    }),
                    .expr = null,
                },
                .else_ = null,
            } },
        },
        .{
            .program =
            \\if (x == 1) do
            \\  return 1;
            \\else
            \\  return 2;
            \\end
            ,
            .expr = ast.Statement{ .if_ = .{
                .cond = @constCast(&ast.Expression{ .binop = .{
                    .op = ast.Operator.eqeq,
                    .lhs = @constCast(&ast.Expression{ .var_ = "x" }),
                    .rhs = @constCast(&ast.Expression{ .literal = ast.Literal{ .number = 1 } }),
                } }),
                .then_ = .{
                    .statements = @constCast(&[_]ast.Statement{
                        ast.Statement{ .return_ = ast.Expression{ .literal = ast.Literal{ .number = 1 } } },
                    }),
                    .expr = null,
                },
                .else_ = ast.Block{
                    .statements = @constCast(&[_]ast.Statement{
                        ast.Statement{ .return_ = ast.Expression{ .literal = ast.Literal{ .number = 2 } } },
                    }),
                    .expr = null,
                },
            } },
        },
    };

    for (cases) |case| {
        var l = lexer.Lexer.init(std.testing.allocator, case.program);
        defer l.deinit();

        const tokens = try l.run();
        var p = parser.Parser.init(std.testing.allocator, tokens.items);
        defer p.deinit();

        const e = try p.statement();

        try std.testing.expectEqualDeep(case.expr, e);
    }
}

test "compiler.evalExpr" {
    const cases = comptime [_]struct {
        program: []const u8,
        expected: ast.Value,
    }{
        .{ .program = "1 + 2 * 3 + 4", .expected = ast.Value{ .i64_ = 11 } },
        .{ .program = "10 - 4", .expected = ast.Value{ .i64_ = 6 } },
        .{ .program = "1 + 2 == 3", .expected = ast.Value{ .bool_ = true } },
    };

    for (cases) |case| {
        var c = Compiler.init(std.testing.allocator);
        defer c.deinit();

        try std.testing.expectEqual(case.expected, try c.evalExpr(case.program));
    }
}

test "compiler.evalBlock" {
    const cases = comptime [_]struct {
        program: []const u8,
        expected: i64,
    }{
        .{
            .program =
            \\let x = 1;
            \\let y = 2;
            \\x + y
            ,
            .expected = 3,
        },
        .{
            .program =
            \\let x = 1;
            \\
            \\let y = if (x == 1) do
            \\  3
            \\else
            \\  4
            \\end;
            \\
            \\y
            ,
            .expected = 3,
        },
        .{
            .program =
            \\let x = 2;
            \\if (x == 2) do
            \\  return 5;
            \\end
            \\
            \\return 0;
            ,
            .expected = 5,
        },
        .{
            .program =
            \\let x = 2;
            \\x = 5;
            \\x
            ,
            .expected = 5,
        },
    };

    for (cases) |case| {
        var c = Compiler.init(std.testing.allocator);
        defer c.deinit();

        try std.testing.expectEqual(ast.Value{ .i64_ = case.expected }, try c.evalBlock(case.program));
    }
}

test "compiler.evalModule" {
    const cases = comptime [_]struct {
        program: []const u8,
        expected: i64,
    }{
        .{
            .program =
            \\fun f(x) do
            \\  let y = 10;
            \\
            \\  return x + y;
            \\end
            \\
            \\fun main() do
            \\  return f(3);
            \\end
            ,
            .expected = 13,
        },
        .{
            .program =
            \\fun sum(x) do
            \\  if (x == 0) do
            \\    return 0;
            \\  end
            \\
            \\  return sum(x - 1) + x;
            \\end
            \\
            \\fun main() do
            \\  return sum(10);
            \\end
            ,
            .expected = 55,
        },
        .{
            .program =
            \\fun main() do
            \\  let s = 0;
            \\  let n = 0;
            \\  while (n < 10) do
            \\    s = s + n;
            \\    n = n + 1;
            \\  end
            \\
            \\  return s;
            \\end
            ,
            .expected = 45,
        },
        .{
            .program =
            \\fun fib(n) do
            \\  if (n == 0) do
            \\    return 1;
            \\  end
            \\  if (n == 1) do
            \\    return 1;
            \\  end
            \\
            \\  return fib(n - 1) + fib(n - 2);
            \\end
            \\
            \\fun main() do
            \\  return fib(15);
            \\end
            ,
            .expected = 987,
        },
        // .{
        //     .program =
        //     \\fun is_prime(n) do
        //     \\  if (n < 2) do
        //     \\    return false;
        //     \\  end
        //     \\
        //     \\  let i = 2;
        //     \\  while (i * i <= n) do
        //     \\    if (n % i == 0) do
        //     \\      return false;
        //     \\    end
        //     \\    i = i + 1;
        //     \\  end
        //     \\
        //     \\  return true;
        //     \\end
        //     \\
        //     \\fun main() do
        //     \\  let n = 1000;
        //     \\  let sum = 0;
        //     \\
        //     \\  while (n > 0) do
        //     \\    n = n - 1;
        //     \\
        //     \\    if (is_prime(n)) do
        //     \\      sum = sum + n;
        //     \\    end
        //     \\  end
        //     \\
        //     \\  return sum;
        //     \\end
        //     ,
        //     .expected = 76127,
        // },
    };

    for (cases) |case| {
        var c = Compiler.init(std.testing.allocator);
        defer c.deinit();

        try std.testing.expectEqual(ast.Value{ .i64_ = case.expected }, try c.evalModule(case.program));
    }
}

test "compiler.runVm" {
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
                .{ .call = 0 },
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

        try Compiler.runVm(case.prog, &stack, &bp, &address_map);

        try std.testing.expectEqualSlices(i64, case.expected, stack.items);
    }
}
