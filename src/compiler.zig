const std = @import("std");

const ast = @import("ast.zig");
const lexer = @import("lexer.zig");
const parser = @import("parser.zig");

const Allocator = std.heap.page_allocator;

const EvalError = error{
    VariableNotFound,
};

const Compiler = struct {
    env: std.StringHashMap(usize),

    fn init() Compiler {
        return Compiler{ .env = std.StringHashMap(usize).init(Allocator) };
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
        const tree = try p.block();

        return try self.evalBlockFromAst(tree);
    }

    fn evalExprFromAst(self: *Compiler, expr: ast.Expression) anyerror!usize {
        switch (expr) {
            .var_ => |v| {
                if (self.env.get(v)) |value| {
                    return value;
                } else {
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
                    .star => {
                        return try self.evalExprFromAst(binop.lhs.*) * try self.evalExprFromAst(binop.rhs.*);
                    },
                    else => {
                        unreachable;
                    },
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
                .return_ => |_| {
                    unreachable;
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
};

test {
    _ = @import("lexer.zig");
    _ = @import("parser.zig");
}

test "compiler.evalExpr" {
    var c = Compiler.init();

    try std.testing.expectEqual(11, try c.evalExpr("1 + 2 * 3 + 4"));
}

test "compiler.evalBlock" {
    var c = Compiler.init();

    try std.testing.expectEqual(3, try c.evalBlock(
        \\let x = 1;
        \\let y = 2;
        \\x + y
    ));
}
