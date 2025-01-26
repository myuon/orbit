const std = @import("std");
const ast = @import("ast.zig");

pub const ParserError = error{
    UnexpectedEos,
    UnexpectedToken,
};

pub const Parser = struct {
    tokens: []ast.Token,
    position: usize,
    ast_arena_allocator: std.heap.ArenaAllocator,

    pub fn init(allocator: std.mem.Allocator, tokens: []ast.Token) Parser {
        return Parser{
            .tokens = tokens,
            .position = 0,
            .ast_arena_allocator = std.heap.ArenaAllocator.init(allocator),
        };
    }

    pub fn deinit(self: *Parser) void {
        self.ast_arena_allocator.deinit();
    }

    fn can_peek(self: *Parser) bool {
        return self.position < self.tokens.len;
    }

    fn peek(self: *Parser) ?ast.Token {
        if (self.can_peek()) {
            return self.tokens[self.position];
        } else {
            return null;
        }
    }

    fn consume(self: *Parser) ?ast.Token {
        if (self.peek()) |token| {
            self.position += 1;
            return token;
        } else {
            return null;
        }
    }

    fn is_next(self: *Parser, keyword: ast.Operator) bool {
        if (self.peek()) |token| {
            switch (token) {
                .keyword => |op| {
                    if (op == keyword) {
                        return true;
                    } else {
                        return false;
                    }
                },
                else => {
                    return false;
                },
            }
        }

        return false;
    }

    fn expect(self: *Parser, keyword: ast.Operator) ParserError!void {
        if (self.is_next(keyword)) {
            _ = self.consume();
        } else {
            std.debug.print("unexpected token: want {any} but got {any}\n", .{ keyword, self.tokens[self.position..] });
            return ParserError.UnexpectedToken;
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

    pub fn module(self: *Parser) anyerror!ast.Module {
        var decls = std.ArrayList(ast.Decl).init(self.ast_arena_allocator.allocator());
        while (self.peek() != null) {
            const d = try self.decl();
            try decls.append(d);
        }

        return ast.Module{ .decls = decls.items };
    }

    fn decl(self: *Parser) anyerror!ast.Decl {
        if (self.peek()) |token| {
            switch (token) {
                .keyword => |op2| {
                    switch (op2) {
                        .fun => {
                            try self.expect(ast.Operator.fun);

                            const name = try self.expect_ident();

                            try self.expect(ast.Operator.lparen);
                            var args = std.ArrayList([]const u8).init(self.ast_arena_allocator.allocator());
                            while (self.can_peek()) {
                                if (self.is_next(ast.Operator.rparen)) {
                                    break;
                                }

                                const arg = try self.expect_ident();
                                try args.append(arg);

                                if (self.is_next(ast.Operator.comma)) {
                                    try self.expect(ast.Operator.comma);
                                } else {
                                    break;
                                }
                            }
                            try self.expect(ast.Operator.rparen);

                            try self.expect(ast.Operator.do);
                            const body = try self.block(null);
                            try self.expect(ast.Operator.end);

                            return ast.Decl{ .fun = .{
                                .name = name,
                                .params = args.items,
                                .body = body,
                            } };
                        },
                        else => {
                            std.debug.print("unexpected token: {any}\n", .{self.tokens[self.position..]});
                            return error.UnexpectedToken;
                        },
                    }
                },
                else => {
                    std.debug.print("unexpected token: {any}\n", .{self.tokens[self.position..]});
                    return error.UnexpectedToken;
                },
            }
        }

        return error.UnexpectedToken;
    }

    pub fn block(self: *Parser, endOp: ?ast.Operator) anyerror!ast.Block {
        var statements = std.ArrayList(ast.Statement).init(self.ast_arena_allocator.allocator());

        while (self.can_peek()) {
            if (self.is_next(ast.Operator.end) or (endOp != null and self.is_next(endOp.?))) {
                break;
            }

            const s = try self.statement();
            switch (s) {
                .expr => |se| {
                    if (self.is_next(ast.Operator.semicolon)) {
                        try self.expect(ast.Operator.semicolon);
                        try statements.append(s);
                        continue;
                    } else {
                        switch (se) {
                            // セミコロン不要なもの
                            .if_ => {
                                try statements.append(s);
                                continue;
                            },
                            else => {
                                const e = try self.ast_arena_allocator.allocator().create(ast.Expression);
                                e.* = s.expr;

                                return ast.Block{ .statements = statements.items, .expr = e };
                            },
                        }
                    }
                },
                else => {
                    try statements.append(s);
                },
            }

            if (self.is_next(ast.Operator.semicolon)) {
                try self.expect(ast.Operator.semicolon);
                continue;
            }
        }

        return ast.Block{ .statements = statements.items, .expr = null };
    }

    pub fn statement(self: *Parser) anyerror!ast.Statement {
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
                        .return_ => {
                            try self.expect(ast.Operator.return_);
                            const value = try self.expr();

                            return ast.Statement{ .return_ = value };
                        },
                        .if_ => {
                            try self.expect(ast.Operator.if_);

                            try self.expect(ast.Operator.lparen);
                            const cond = try self.ast_arena_allocator.allocator().create(ast.Expression);
                            cond.* = try self.expr();
                            try self.expect(ast.Operator.rparen);

                            try self.expect(ast.Operator.do);
                            const then_ = try self.block(ast.Operator.else_);

                            if (self.is_next(ast.Operator.else_)) {
                                try self.expect(ast.Operator.else_);
                                const else_ = try self.block(null);
                                try self.expect(ast.Operator.end);

                                return ast.Statement{ .if_ = .{
                                    .cond = cond,
                                    .then_ = then_,
                                    .else_ = else_,
                                } };
                            } else {
                                try self.expect(ast.Operator.end);
                                return ast.Statement{ .if_ = .{
                                    .cond = cond,
                                    .then_ = then_,
                                    .else_ = null,
                                } };
                            }
                        },
                        .while_ => {
                            const cond = try self.ast_arena_allocator.allocator().create(ast.Expression);

                            try self.expect(ast.Operator.while_);

                            try self.expect(ast.Operator.lparen);
                            cond.* = try self.expr();
                            try self.expect(ast.Operator.rparen);

                            try self.expect(ast.Operator.do);
                            const body = try self.block(null);
                            try self.expect(ast.Operator.end);

                            return ast.Statement{ .while_ = .{
                                .cond = cond,
                                .body = body,
                            } };
                        },
                        else => {
                            unreachable;
                        },
                    }
                },
                else => {
                    const e = try self.expr();

                    if (self.is_next(ast.Operator.eq)) {
                        try self.expect(ast.Operator.eq);

                        switch (e) {
                            .var_ => |v| {
                                const value = try self.expr();
                                return ast.Statement{ .assign = .{
                                    .name = v,
                                    .value = value,
                                } };
                            },
                            else => {
                                std.debug.print("unexpected token: {any}\n", .{self.tokens[self.position..]});
                                return error.UnexpectedToken;
                            },
                        }
                    }

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
                            const b = try self.block(null);
                            try self.expect(ast.Operator.end);

                            return ast.Expression{ .block = b };
                        },
                        else => {},
                    }
                },
                else => {},
            }
        }

        return self.expr0();
    }

    fn expr0(self: *Parser) anyerror!ast.Expression {
        var current = try self.expr01();

        while (self.peek()) |token| {
            switch (token) {
                .keyword => |op| {
                    switch (op) {
                        .eqeq => {
                            try self.expect(ast.Operator.eqeq);

                            const rhs = try self.ast_arena_allocator.allocator().create(ast.Expression);
                            rhs.* = try self.expr01();

                            const newCurrent = try self.ast_arena_allocator.allocator().create(ast.Expression);
                            newCurrent.* = current;

                            current = ast.Expression{ .binop = .{
                                .op = .eqeq,
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

    fn expr01(self: *Parser) anyerror!ast.Expression {
        var current = try self.expr1();

        while (self.peek()) |token| {
            switch (token) {
                .keyword => |op| {
                    switch (op) {
                        .langle => {
                            try self.expect(ast.Operator.langle);

                            const rhs = try self.ast_arena_allocator.allocator().create(ast.Expression);
                            rhs.* = try self.expr1();

                            const newCurrent = try self.ast_arena_allocator.allocator().create(ast.Expression);
                            newCurrent.* = current;

                            current = ast.Expression{ .binop = .{
                                .op = .langle,
                                .lhs = newCurrent,
                                .rhs = rhs,
                            } };
                        },
                        .rangle => {
                            try self.expect(ast.Operator.rangle);

                            const rhs = try self.ast_arena_allocator.allocator().create(ast.Expression);
                            rhs.* = try self.expr1();

                            const newCurrent = try self.ast_arena_allocator.allocator().create(ast.Expression);
                            newCurrent.* = current;

                            current = ast.Expression{ .binop = .{
                                .op = .rangle,
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

    fn expr1(self: *Parser) anyerror!ast.Expression {
        var current = try self.expr2();

        while (self.peek()) |token| {
            switch (token) {
                .keyword => |op| {
                    switch (op) {
                        .plus => {
                            try self.expect(ast.Operator.plus);

                            const rhs = try self.ast_arena_allocator.allocator().create(ast.Expression);
                            rhs.* = try self.expr2();

                            const newCurrent = try self.ast_arena_allocator.allocator().create(ast.Expression);
                            newCurrent.* = current;

                            current = ast.Expression{ .binop = .{
                                .op = .plus,
                                .lhs = newCurrent,
                                .rhs = rhs,
                            } };
                        },
                        .minus => {
                            try self.expect(ast.Operator.minus);

                            const rhs = try self.ast_arena_allocator.allocator().create(ast.Expression);
                            rhs.* = try self.expr2();

                            const newCurrent = try self.ast_arena_allocator.allocator().create(ast.Expression);
                            newCurrent.* = current;

                            current = ast.Expression{ .binop = .{
                                .op = .minus,
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

                            const rhs = try self.ast_arena_allocator.allocator().create(ast.Expression);
                            rhs.* = try self.expr3();

                            const newCurrent = try self.ast_arena_allocator.allocator().create(ast.Expression);
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

    fn expr3(self: *Parser) anyerror!ast.Expression {
        if (self.literal()) |lit| {
            return ast.Expression{ .literal = lit };
        }

        if (self.peek()) |token| {
            switch (token) {
                .ident => |ident| {
                    _ = self.consume();

                    const current = try self.ast_arena_allocator.allocator().create(ast.Expression);
                    const identExpr = ast.Expression{ .var_ = ident };

                    if (self.is_next(ast.Operator.lparen)) {
                        try self.expect(ast.Operator.lparen);

                        var args = std.ArrayList(ast.Expression).init(self.ast_arena_allocator.allocator());
                        while (self.can_peek()) {
                            if (self.is_next(ast.Operator.rparen)) {
                                break;
                            }

                            const arg = try self.expr();
                            try args.append(arg);

                            if (self.is_next(ast.Operator.comma)) {
                                try self.expect(ast.Operator.comma);
                            } else {
                                break;
                            }
                        }
                        try self.expect(ast.Operator.rparen);

                        current.* = ast.Expression{ .call = .{
                            .name = ident,
                            .args = args.items,
                        } };
                    } else {
                        current.* = identExpr;
                    }

                    return current.*;
                },
                else => {},
            }
        }

        if (self.is_next(ast.Operator.if_)) {
            try self.expect(ast.Operator.if_);

            try self.expect(ast.Operator.lparen);
            const cond = try self.ast_arena_allocator.allocator().create(ast.Expression);
            cond.* = try self.expr();
            try self.expect(ast.Operator.rparen);

            try self.expect(ast.Operator.do);
            const then_ = try self.block(ast.Operator.else_);

            try self.expect(ast.Operator.else_);
            const else_ = try self.block(null);
            try self.expect(ast.Operator.end);

            return ast.Expression{ .if_ = .{
                .cond = cond,
                .then_ = then_,
                .else_ = else_,
            } };
        }

        std.debug.print("unexpected token: {any}\n", .{self.tokens[self.position..]});
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
    const cases = comptime [_]struct {
        tokens: []ast.Token,
        expected: ast.Expression,
    }{
        .{
            .tokens = @constCast(&[_]ast.Token{
                .{ .number = 1 },
                .{ .keyword = ast.Operator.plus },
                .{ .number = 2 },
                .{ .keyword = ast.Operator.star },
                .{ .number = 3 },
                .{ .keyword = ast.Operator.plus },
                .{ .number = 4 },
            }),
            .expected = ast.Expression{
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
            },
        },
    };

    for (cases) |case| {
        var parser = Parser.init(std.testing.allocator, case.tokens);
        defer parser.deinit();

        const expr = parser.expr();

        try std.testing.expectEqualDeep(case.expected, expr);
    }
}
