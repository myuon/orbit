const std = @import("std");
const ast = @import("ast.zig");

const Allocator = std.heap.page_allocator;

pub const ParserError = error{
    UnexpectedEos,
    UnexpectedToken,
};

pub const Parser = struct {
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

    fn expect(self: *Parser, keyword: ast.Operator) ParserError!void {
        if (self.peek()) |token| {
            switch (token) {
                .keyword => |op| {
                    if (op == keyword) {
                        _ = self.consume();
                    } else {
                        return error.UnexpectedToken;
                    }
                },
                else => {
                    return error.UnexpectedToken;
                },
            }
        } else {
            return error.UnexpectedEos;
        }
    }

    fn expect_ident(self: *Parser) ParserError![]const u8 {
        if (self.peek()) |token| {
            switch (token) {
                .ident => |ident| {
                    _ = self.consume();
                    return ident;
                },
                else => {
                    return error.UnexpectedToken;
                },
            }
        } else {
            return error.UnexpectedEos;
        }
    }

    pub fn block(self: *Parser) anyerror!ast.Block {
        var statements = std.ArrayList(ast.Statement).init(Allocator);
        statements.deinit();

        while (self.peek() != null) {
            const s = try self.statement();
            switch (s) {
                .expr => {
                    if (self.peek()) |token| {
                        if (token.keyword == ast.Operator.semicolon) {
                            try self.expect(ast.Operator.semicolon);
                            try statements.append(s);
                            continue;
                        } else {
                            const e = try Allocator.create(ast.Expression);
                            e.* = s.expr;

                            return ast.Block{ .statements = statements.items, .expr = e };
                        }
                    } else {
                        const e = try Allocator.create(ast.Expression);
                        e.* = s.expr;

                        return ast.Block{ .statements = statements.items, .expr = e };
                    }
                },
                else => {
                    try statements.append(s);
                },
            }

            if (self.peek()) |token| {
                if (token.keyword == ast.Operator.semicolon) {
                    try self.expect(ast.Operator.semicolon);
                    continue;
                }
            } else {
                break;
            }
        }

        return ast.Block{ .statements = statements.items, .expr = null };
    }

    fn statement(self: *Parser) anyerror!ast.Statement {
        if (self.peek()) |token| {
            switch (token) {
                .keyword => |op| {
                    switch (op) {
                        .let => {
                            try self.expect(ast.Operator.let);
                            const name = try self.expect_ident();
                            try self.expect(ast.Operator.eq);
                            const value = try self.expr();

                            return ast.Statement{ .let = .{ .name = name, .value = value } };
                        },
                        else => {
                            return error.UnexpectedToken;
                        },
                    }
                },
                else => {
                    const e = try self.expr();

                    return ast.Statement{ .expr = e };
                },
            }
        }

        return error.UnexpectedEos;
    }

    pub fn expr(self: *Parser) anyerror!ast.Expression {
        if (self.peek()) |token| {
            switch (token) {
                .keyword => |op| {
                    switch (op) {
                        .do => {
                            try self.expect(ast.Operator.do);
                            const b = try self.block();
                            try self.expect(ast.Operator.end);

                            return ast.Expression{ .block = b };
                        },
                        else => {},
                    }
                },
                else => {},
            }
        }

        return self.expr1();
    }

    fn expr1(self: *Parser) anyerror!ast.Expression {
        var current = try self.expr2();

        while (self.peek()) |token| {
            switch (token) {
                .keyword => |op| {
                    switch (op) {
                        .plus => {
                            try self.expect(ast.Operator.plus);

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

        if (self.peek()) |token| {
            switch (token) {
                .ident => |ident| {
                    _ = self.consume();

                    return ast.Expression{ .var_ = ident };
                },
                else => {},
            }
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
