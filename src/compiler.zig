const std = @import("std");

const ast = @import("ast.zig");
const lexer = @import("lexer.zig");
const parser = @import("parser.zig");
const vm = @import("vm.zig");
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
    vmc: vm.Vm,
    enable_jit: bool,

    pub fn init(allocator: std.mem.Allocator) Compiler {
        return Compiler{
            .env = Env.init(allocator),
            .module = null,
            .has_returned = false,
            .call_trace = std.StringHashMap(u32).init(allocator),
            .ir_cache = std.StringHashMap([]ast.Instruction).init(allocator),
            .jit_cache = std.StringHashMap(jit.CompiledFn).init(allocator),
            .allocator = allocator,
            .ast_arena_allocator = std.heap.ArenaAllocator.init(allocator),
            .vmc = vm.Vm.init(allocator),
            .enable_jit = false,
        };
    }

    pub fn deinit(self: *Compiler) void {
        self.env.deinit();
        self.call_trace.deinit();
        self.ir_cache.deinit();
        self.jit_cache.deinit();
        self.ast_arena_allocator.deinit();
        self.vmc.deinit();
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

    pub fn compileInIr(self: *Compiler, str: []const u8) anyerror![]ast.Instruction {
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

        const ir = try self.vmc.compileModule("main", tree);

        end = try std.time.Instant.now();
        std.debug.print("Compiler.compile in {d:.3}ms\n", .{@as(f64, @floatFromInt(end.since(start))) / std.time.ns_per_ms});
        start = end;

        return ir;
    }

    pub fn startVm(self: *Compiler, ir: []ast.Instruction) anyerror!ast.Value {
        var stack = std.ArrayList(i64).init(self.allocator);
        defer stack.deinit();

        var address_map = std.StringHashMap(i64).init(self.allocator);
        defer address_map.deinit();

        try address_map.put("return", -2 - 1);
        try stack.append(-2); // return value
        try stack.append(-1); // pc
        try stack.append(0); // bp

        var bp: i64 = @intCast(stack.items.len);

        var vmr = vm.VmRuntime.init(self.allocator);
        defer vmr.deinit();

        try vmr.run(ir, &stack, &bp, &address_map);

        return ast.Value{ .i64_ = stack.items[0] };
    }

    pub fn evalModuleWithIr(self: *Compiler, str: []const u8) anyerror!ast.Value {
        const ir = try self.compileInIr(str);
        return try self.startVm(ir);
    }

    pub fn evalModule(self: *Compiler, str: []const u8, options: struct { enable_jit: bool }) anyerror!?ast.Value {
        self.enable_jit = options.enable_jit;

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

        if (self.enable_jit) {
            const start = try std.time.Instant.now();

            var bp = @as(i64, @intCast(stack.items.len));

            var runtime = jit.JitRuntime.init(self.allocator);
            const fn_ptr = runtime.compile(ir) catch |err| {
                std.debug.print("JIT compile error, fallback to VM execution: {any}\n", .{err});

                var vmr = vm.VmRuntime.init(self.allocator);
                defer vmr.deinit();

                try vmr.run(ir, &stack, &bp, &address_map);
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
            var bp: i64 = @intCast(stack.items.len);

            var vmr = vm.VmRuntime.init(self.allocator);
            defer vmr.deinit();

            try vmr.run(ir, &stack, &bp, &address_map);
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

            return try self.callIrFunction(name, params, prog, args);
        }

        if (self.call_trace.get(name)) |count| {
            if (count == 5) {
                const start = try std.time.Instant.now();
                std.debug.print("Start compiling: {s}\n", .{name});

                var vmc = vm.Vm.init(self.allocator);
                defer vmc.deinit();
                const ir = try vmc.compile(name, body);
                try vmc.resolveIrLabels(ir);
                try vmc.resolveLocals(params, ir);

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

        self.env.deinit();
        self.env = envCloned;

        return value;
    }
};

test {
    _ = @import("lexer.zig");
    _ = @import("parser.zig");
    _ = @import("vm.zig");
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
        // .{
        //     .program =
        //     \\fun sum(x) do
        //     \\  if (x == 0) do
        //     \\    return 0;
        //     \\  end
        //     \\
        //     \\  return sum(x - 1) + x;
        //     \\end
        //     \\
        //     \\fun main() do
        //     \\  return sum(10);
        //     \\end
        //     ,
        //     .expected = 55,
        // },
        // .{
        //     .program =
        //     \\fun main() do
        //     \\  let s = 0;
        //     \\  let n = 0;
        //     \\  while (n < 10) do
        //     \\    s = s + n;
        //     \\    n = n + 1;
        //     \\  end
        //     \\
        //     \\  return s;
        //     \\end
        //     ,
        //     .expected = 45,
        // },
        // .{
        //     .program =
        //     \\fun fib(n) do
        //     \\  if (n == 0) do
        //     \\    return 1;
        //     \\  end
        //     \\  if (n == 1) do
        //     \\    return 1;
        //     \\  end
        //     \\
        //     \\  return fib(n - 1) + fib(n - 2);
        //     \\end
        //     \\
        //     \\fun main() do
        //     \\  return fib(15);
        //     \\end
        //     ,
        //     .expected = 987,
        // },
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

        try std.testing.expectEqual(ast.Value{ .i64_ = case.expected }, try c.evalModule(case.program, .{ .enable_jit = false }));
        try std.testing.expectEqual(ast.Value{ .i64_ = case.expected }, try c.evalModule(case.program, .{ .enable_jit = true }));
        try std.testing.expectEqual(ast.Value{ .i64_ = case.expected }, try c.evalModuleWithIr(case.program));
    }
}
