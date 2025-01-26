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
    env: std.StringHashMap(usize),
    module: ?ast.Module,

    pub fn init() Compiler {
        return Compiler{ .env = std.StringHashMap(usize).init(Allocator), .module = null };
    }

    fn evalExpr(self: *Compiler, str: []const u8) anyerror!usize {
        var l = lexer.Lexer{ .source = str, .position = 0 };
        const tokens = try l.run();
        var p = parser.Parser{ .tokens = tokens.items, .position = 0 };
        const tree = try p.expr();

        return self.evalExprFromAst(tree);
    }

    fn evalBlock(self: *Compiler, str: []const u8) anyerror!?usize {
        var l = lexer.Lexer{ .source = str, .position = 0 };
        const tokens = try l.run();
        var p = parser.Parser{ .tokens = tokens.items, .position = 0 };
        const tree = try p.block(null);

        return try self.evalBlockFromAst(tree);
    }

    pub fn evalModule(self: *Compiler, str: []const u8) anyerror!?usize {
        var l = lexer.Lexer{ .source = str, .position = 0 };
        const tokens = try l.run();
        var p = parser.Parser{ .tokens = tokens.items, .position = 0 };
        const tree = try p.module();

        self.module = tree;

        return try self.evalModuleFromAst("main");
    }

    fn evalExprFromAst(self: *Compiler, expr: ast.Expression) anyerror!usize {
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
                        return n;
                    },
                    else => {
                        unreachable;
                    },
                }
            },
            .binop => |binop| {
                switch (binop.op) {
                    .plus => {
                        return try self.evalExprFromAst(binop.lhs.*) + try self.evalExprFromAst(binop.rhs.*);
                    },
                    .minus => {
                        return try self.evalExprFromAst(binop.lhs.*) - try self.evalExprFromAst(binop.rhs.*);
                    },
                    .star => {
                        return try self.evalExprFromAst(binop.lhs.*) * try self.evalExprFromAst(binop.rhs.*);
                    },
                    .eqeq => {
                        if (try self.evalExprFromAst(binop.lhs.*) == try self.evalExprFromAst(binop.rhs.*)) {
                            return 1;
                        } else {
                            return 0;
                        }
                    },
                    else => {
                        unreachable;
                    },
                }
            },
            .call => |call| {
                var args = std.ArrayList(usize).init(Allocator);
                defer args.deinit();

                for (call.args) |arg| {
                    try args.append(try self.evalExprFromAst(arg));
                }

                const r = try self.callFunction(call.name, args.items);
                if (r) |value| {
                    return value;
                } else {
                    return error.UnexpectedNilValue;
                }
            },
            .if_ => |if_| {
                if (try self.evalExprFromAst(if_.cond.*) == 1) {
                    return try self.evalBlockFromAst(if_.then_) orelse 0;
                } else {
                    if (if_.else_) |else_| {
                        return try self.evalBlockFromAst(else_) orelse 0;
                    }

                    return error.UnexpectedNilValue;
                }
            },
            else => {
                unreachable;
            },
        }
    }

    fn evalBlockFromAst(self: *Compiler, block: ast.Block) anyerror!?usize {
        for (block.statements) |stmt| {
            switch (stmt) {
                .let => |let| {
                    try self.env.put(let.name, try self.evalExprFromAst(let.value));
                },
                .return_ => |val| {
                    return try self.evalExprFromAst(val);
                },
                .expr => |expr| {
                    _ = try self.evalExprFromAst(expr);
                },
            }
        }

        if (block.expr) |expr| {
            return try self.evalExprFromAst(expr.*);
        }

        return null;
    }

    fn evalModuleFromAst(self: *Compiler, entrypoint: []const u8) anyerror!?usize {
        return self.callFunction(entrypoint, &[_]usize{});
    }

    fn callFunction(self: *Compiler, name: []const u8, args: []usize) anyerror!?usize {
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

        for (args, 0..) |arg, i| {
            try self.env.put(params[i], arg);
        }

        return try self.evalBlockFromAst(body);
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

test "compiler.parseExpression" {
    const cases = comptime [_]struct {
        program: []const u8,
        expr: ast.Expression,
    }{
        .{
            .program =
            \\if (x == 1) do
            \\  return 1;
            \\end
            ,
            .expr = ast.Expression{ .if_ = .{
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
            .expr = ast.Expression{ .if_ = .{
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
        const e = try p.expr();

        try std.testing.expectEqualDeep(case.expr, e);
    }
}

test "compiler.evalExpr" {
    const cases = comptime [_]struct {
        program: []const u8,
        expected: usize,
    }{
        .{ .program = "1 + 2 * 3 + 4", .expected = 11 },
        .{ .program = "10 - 4", .expected = 6 },
        .{ .program = "1 + 2 == 3", .expected = 1 },
    };

    for (cases) |case| {
        var c = Compiler.init();
        try std.testing.expectEqual(case.expected, try c.evalExpr(case.program));
    }
}

test "compiler.evalBlock" {
    const cases = comptime [_]struct {
        program: []const u8,
        expected: usize,
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
    };

    for (cases) |case| {
        var c = Compiler.init();
        try std.testing.expectEqual(case.expected, try c.evalBlock(case.program));
    }
}

test "compiler.evalModule" {
    const cases = comptime [_]struct {
        program: []const u8,
        expected: usize,
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
    };

    for (cases) |case| {
        var c = Compiler.init();
        try std.testing.expectEqual(case.expected, try c.evalModule(case.program));
    }
}
