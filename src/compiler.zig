const std = @import("std");

const ast = @import("ast.zig");
const lexer = @import("lexer.zig");
const parser = @import("parser.zig");

const Compiler = struct {
    fn eval(self: *Compiler, str: []const u8) anyerror!usize {
        var l = lexer.Lexer{ .source = str, .position = 0 };
        const tokens = try l.run();
        var p = parser.Parser{ .tokens = tokens.items, .position = 0 };
        const tree = try p.expr();

        return self.evalFromAst(tree);
    }

    fn evalFromAst(self: *Compiler, expr: ast.Expression) usize {
        switch (expr) {
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
                        return self.evalFromAst(binop.lhs.*) + self.evalFromAst(binop.rhs.*);
                    },
                    .star => {
                        return self.evalFromAst(binop.lhs.*) * self.evalFromAst(binop.rhs.*);
                    },
                    else => {
                        unreachable;
                    },
                }
            },
        }
    }
};

test {
    _ = @import("lexer.zig");
    _ = @import("parser.zig");
}

test "compiler" {
    var c = Compiler{};

    try std.testing.expectEqual(11, c.eval("1 + 2 * 3 + 4"));
}
