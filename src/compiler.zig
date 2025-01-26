const std = @import("std");

const ast = @import("ast.zig");
const lexer = @import("lexer.zig");
const parser = @import("parser.zig");

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
    allocator: std.mem.Allocator,
    ast_arena_allocator: std.heap.ArenaAllocator,

    pub fn init(allocator: std.mem.Allocator) Compiler {
        return Compiler{
            .env = Env.init(allocator),
            .module = null,
            .has_returned = false,
            .call_trace = std.StringHashMap(u32).init(allocator),
            .ir_cache = std.StringHashMap([]ast.Instruction).init(allocator),
            .allocator = allocator,
            .ast_arena_allocator = std.heap.ArenaAllocator.init(allocator),
        };
    }

    pub fn deinit(self: *Compiler) void {
        self.env.deinit();
        self.call_trace.deinit();
        self.ir_cache.deinit();
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
        var l = lexer.Lexer.init(self.allocator, str);
        defer l.deinit();

        const tokens = try l.run();
        var p = parser.Parser.init(self.allocator, tokens.items);
        defer p.deinit();

        const tree = try p.module();

        self.module = tree;

        return try self.evalModuleFromAst("main");
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
                        return error.CannotCompileToIr;
                    },
                    .eqeq => {
                        try buffer.append(ast.Instruction{ .eq = true });
                    },
                    else => {
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

                try buffer.append(ast.Instruction{ .jump_ifzero = "if_else" });
                try buffer.append(ast.Instruction{ .label = "if_then" });
                try self.compileBlockFromAst(buffer, if_.then_);
                try buffer.append(ast.Instruction{ .jump = "if_end" });
                try buffer.append(ast.Instruction{ .label = "if_else" });
                try self.compileBlockFromAst(buffer, if_.else_);
                try buffer.append(ast.Instruction{ .label = "if_end" });
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

                    try buffer.append(ast.Instruction{ .jump_ifzero = "if_else" });
                    try buffer.append(ast.Instruction{ .label = "if_then" });
                    try self.compileBlockFromAst(buffer, if_.then_);
                    try buffer.append(ast.Instruction{ .jump = "if_end" });
                    try buffer.append(ast.Instruction{ .label = "if_else" });
                    if (if_.else_) |else_| {
                        try self.compileBlockFromAst(buffer, else_);
                    }
                    try buffer.append(ast.Instruction{ .label = "if_end" });
                },
            }
        }
    }

    fn runVm(
        program: []ast.Instruction,
        current_context: []const u8,
        stack: *std.ArrayList(i32),
        bp: *usize,
        address_map: std.StringHashMap(i32),
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
                .call => |name| {
                    if (std.mem.eql(u8, name, current_context)) {
                        pc = 0;
                        continue;
                    }

                    std.debug.print("Calling function: {s}\n", .{name});
                    return error.CannotCompileToIr;
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
                    const index = address_map.get(name).?;
                    const b: i32 = @intCast(bp.*);
                    stack.items[@intCast(b + index)] = value;
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
                        return ast.Value{ .i32_ = @intCast(n) };
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
                        return ast.Value{ .i32_ = try lhs.asI32() + try rhs.asI32() };
                    },
                    .minus => {
                        const lhs = try self.evalExprFromAst(binop.lhs.*);
                        const rhs = try self.evalExprFromAst(binop.rhs.*);
                        return ast.Value{ .i32_ = try lhs.asI32() - try rhs.asI32() };
                    },
                    .star => {
                        const lhs = try self.evalExprFromAst(binop.lhs.*);
                        const rhs = try self.evalExprFromAst(binop.rhs.*);
                        return ast.Value{ .i32_ = try lhs.asI32() * try rhs.asI32() };
                    },
                    .eqeq => {
                        return ast.Value{ .bool_ = std.meta.eql(try self.evalExprFromAst(binop.lhs.*), try self.evalExprFromAst(binop.rhs.*)) };
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

    fn callIrFunction(
        self: *Compiler,
        name: []const u8,
        params: []const []const u8,
        ir: []ast.Instruction,
        args: []ast.Value,
    ) anyerror!ast.Value {
        var stack = std.ArrayList(i32).init(self.allocator);
        defer stack.deinit();

        var address_map = std.StringHashMap(i32).init(self.allocator);
        defer address_map.deinit();

        const l: i32 = @intCast(args.len);
        try address_map.put("return", -2 - l - 1);
        try stack.append(-2); // return value

        for (args, 0..) |arg, i| {
            const k: i32 = @intCast(args.len - i);
            try address_map.put(params[i], -2 - k);
            try stack.append(try arg.asI32());
        }

        try stack.append(-1); // pc
        try stack.append(0); // bp

        var bp = stack.items.len;

        try Compiler.runVm(
            ir,
            name,
            &stack,
            &bp,
            address_map,
        );

        return ast.Value{ .i32_ = stack.items[0] };
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

        if (self.ir_cache.get(name)) |prog| {
            const value = self.callIrFunction(name, params, prog, args);
            return value;
        }

        if (self.call_trace.get(name)) |count| {
            if (count == 5) {
                const start = try std.time.Instant.now();
                std.debug.print("Start compiling: {s}\n", .{name});

                var buffer = std.ArrayList(ast.Instruction).init(self.ast_arena_allocator.allocator());
                try self.compileBlockFromAst(&buffer, body);
                const ir = buffer.items;

                const end = try std.time.Instant.now();
                const elapsed: f64 = @floatFromInt(end.since(start));
                std.debug.print("Compiled in {d:.3}ms: {s}\n", .{ elapsed / std.time.ns_per_ms, name });

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
        .{ .program = "1 + 2 * 3 + 4", .expected = ast.Value{ .i32_ = 11 } },
        .{ .program = "10 - 4", .expected = ast.Value{ .i32_ = 6 } },
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
        expected: i32,
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
    };

    for (cases) |case| {
        var c = Compiler.init(std.testing.allocator);
        defer c.deinit();

        try std.testing.expectEqual(ast.Value{ .i32_ = case.expected }, try c.evalBlock(case.program));
    }
}

test "compiler.evalModule" {
    const cases = comptime [_]struct {
        program: []const u8,
        expected: i32,
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
    };

    for (cases) |case| {
        var c = Compiler.init(std.testing.allocator);
        defer c.deinit();

        try std.testing.expectEqual(ast.Value{ .i32_ = case.expected }, try c.evalModule(case.program));
    }
}
