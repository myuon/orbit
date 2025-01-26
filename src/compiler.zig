const std = @import("std");

const ast = @import("ast.zig");
const lexer = @import("lexer.zig");
const parser = @import("parser.zig");

const Allocator = std.heap.page_allocator;

pub const EvalError = error{
    VariableNotFound,
    UnexpectedNilValue,
};

pub const Compiler = struct {
    const Env = std.StringHashMap(ast.Value);

    env: Env,
    module: ?ast.Module,
    has_returned: bool,

    pub fn init() Compiler {
        return Compiler{
            .env = Env.init(Allocator),
            .module = null,
            .has_returned = false,
        };
    }

    fn evalExpr(self: *Compiler, str: []const u8) anyerror!ast.Value {
        var l = lexer.Lexer{ .source = str, .position = 0 };
        const tokens = try l.run();
        var p = parser.Parser{ .tokens = tokens.items, .position = 0 };
        const tree = try p.expr();

        return self.evalExprFromAst(tree);
    }

    fn evalBlock(self: *Compiler, str: []const u8) anyerror!?ast.Value {
        var l = lexer.Lexer{ .source = str, .position = 0 };
        const tokens = try l.run();
        var p = parser.Parser{ .tokens = tokens.items, .position = 0 };
        const tree = try p.block(null);

        return try self.evalBlockFromAst(tree);
    }

    pub fn evalModule(self: *Compiler, str: []const u8) anyerror!?ast.Value {
        var l = lexer.Lexer{ .source = str, .position = 0 };
        const tokens = try l.run();
        var p = parser.Parser{ .tokens = tokens.items, .position = 0 };
        const tree = try p.module();

        self.module = tree;

        return try self.evalModuleFromAst("main");
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
                var args = std.ArrayList(ast.Value).init(Allocator);
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

        const envCloned = try self.env.clone();

        for (args, 0..) |arg, i| {
            try self.env.put(params[i], arg);
        }

        const value = try self.evalBlockFromAst(body);
        self.has_returned = false;
        self.env = envCloned;

        std.debug.print("Calling function: {s}({any}) -> {any}\n", .{ name, args, value });

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
        var l = lexer.Lexer{ .source = case.program, .position = 0 };
        const tokens = try l.run();
        var p = parser.Parser{ .tokens = tokens.items, .position = 0 };

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
        var l = lexer.Lexer{ .source = case.program, .position = 0 };
        const tokens = try l.run();
        var p = parser.Parser{ .tokens = tokens.items, .position = 0 };
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
        var c = Compiler.init();
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
        var c = Compiler.init();
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
        var c = Compiler.init();
        try std.testing.expectEqual(ast.Value{ .i32_ = case.expected }, try c.evalModule(case.program));
    }
}
