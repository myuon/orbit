const std = @import("std");
const ast = @import("ast.zig");

const Allocator = std.heap.page_allocator;

const Parser = struct {
    tokens: []ast.Token,
    position: usize,

    fn peek(self: *Parser) ?ast.Token {
        if (self.position >= self.tokens.len) {
            return null;
        }

        return self.tokens[self.position];
    }

    fn consume(self: *Parser) ?ast.Token {
        if (self.peek()) |token| {
            self.position += 1;
            return token;
        } else {
            return null;
        }
    }

    fn expr(self: *Parser) anyerror!ast.Expression {
        return self.expr1();
    }

    fn expr1(self: *Parser) anyerror!ast.Expression {
        var current = try self.expr2();

        while (self.peek()) |token| {
            switch (token) {
                .keyword => |op| {
                    switch (op) {
                        .plus => {
                            _ = self.consume();

                            const rhs = try Allocator.create(ast.Expression);
                            rhs.* = try self.expr2();

                            const newCurrent = try Allocator.create(ast.Expression);
                            newCurrent.* = current;

                            current = ast.Expression{ .binop = .{
                                .op = .plus,
                                .lhs = newCurrent,
                                .rhs = rhs,
                            } };
                        },
                        else => {
                            break;
                        },
                    }
                },
                else => {
                    break;
                },
            }
        }

        return current;
    }

    fn expr2(self: *Parser) anyerror!ast.Expression {
        var current = try self.expr3();

        while (self.peek()) |token| {
            switch (token) {
                .keyword => |op| {
                    switch (op) {
                        .star => {
                            _ = self.consume();

                            const rhs = try Allocator.create(ast.Expression);
                            rhs.* = try self.expr3();

                            const newCurrent = try Allocator.create(ast.Expression);
                            newCurrent.* = current;

                            current = ast.Expression{ .binop = .{
                                .op = .star,
                                .lhs = newCurrent,
                                .rhs = rhs,
                            } };
                        },
                        else => {
                            break;
                        },
                    }
                },
                else => {
                    break;
                },
            }
        }

        return current;
    }

    fn expr3(self: *Parser) !ast.Expression {
        if (self.literal()) |lit| {
            return ast.Expression{ .literal = lit };
        }

        unreachable;
    }

    fn literal(self: *Parser) ?ast.Literal {
        if (self.peek()) |token| {
            switch (token) {
                .number => |n| {
                    _ = self.consume();

                    return ast.Literal{ .number = n };
                },
                else => {
                    return null;
                },
            }
        }

        return null;
    }
};

test "parser" {
    var tokens = [_]ast.Token{
        .{ .number = 1 },
        .{ .keyword = ast.Operator.plus },
        .{ .number = 2 },
        .{ .keyword = ast.Operator.star },
        .{ .number = 3 },
        .{ .keyword = ast.Operator.plus },
        .{ .number = 4 },
    };

    var parser = Parser{ .tokens = tokens[0..], .position = 0 };
    const expr = parser.expr();

    try std.testing.expectEqualDeep(ast.Expression{
        .binop = .{
            .op = ast.Operator.plus,
            .lhs = @constCast(&ast.Expression{
                .binop = .{
                    .op = ast.Operator.plus,
                    .lhs = @constCast(&ast.Expression{
                        .literal = ast.Literal{ .number = 1 },
                    }),
                    .rhs = @constCast(&ast.Expression{
                        .binop = .{
                            .op = ast.Operator.star,
                            .lhs = @constCast(&ast.Expression{
                                .literal = ast.Literal{ .number = 2 },
                            }),
                            .rhs = @constCast(&ast.Expression{
                                .literal = ast.Literal{ .number = 3 },
                            }),
                        },
                    }),
                },
            }),
            .rhs = @constCast(&ast.Expression{
                .literal = ast.Literal{ .number = 4 },
            }),
        },
    }, expr);
}
