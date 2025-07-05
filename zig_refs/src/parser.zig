const std = @import("std");
const ast = @import("ast.zig");
const utils = @import("utils.zig");

pub const ParserError = error{
    UnexpectedEos,
    UnexpectedToken,
};

pub const Parser = struct {
    tokens: []utils.Positioned(ast.Token),
    position: usize,
    ast_arena_allocator: std.heap.ArenaAllocator,

    pub fn init(allocator: std.mem.Allocator, tokens: []utils.Positioned(ast.Token)) Parser {
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
            return self.tokens[self.position].data;
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
            std.debug.print("unexpected token: want {} but got {any}\n", .{ keyword, self.tokens[self.position..] });
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
                    std.log.err("unexpected token: want ident but got {any}\n", .{self.tokens[self.position..]});
                    return error.UnexpectedToken;
                },
            }
        } else {
            return error.UnexpectedEos;
        }
    }

    fn expect_number(self: *Parser) ParserError!u64 {
        if (self.peek()) |token| {
            switch (token) {
                .number => |n| {
                    _ = self.consume();
                    return n;
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

        const type_defs = ast.TypeDefs.init(self.ast_arena_allocator.allocator());

        return ast.Module{
            .decls = decls.items,
            .type_defs = type_defs,
        };
    }

    fn decl(self: *Parser) anyerror!ast.Decl {
        if (self.peek()) |token| {
            switch (token) {
                .keyword => |op| {
                    switch (op) {
                        .fun => {
                            return try self.parseFunDecl();
                        },
                        .let => {
                            try self.expect(ast.Operator.let);

                            const name = try self.expect_ident();

                            var value: ?ast.Expression = null;
                            if (self.is_next(ast.Operator.eq)) {
                                try self.expect(ast.Operator.eq);
                                value = try self.expr();
                            }
                            try self.expect(ast.Operator.semicolon);

                            return ast.Decl{ .let = .{
                                .name = name,
                                .value = value,
                            } };
                        },
                        .type_ => {
                            try self.expect(ast.Operator.type_);

                            const name = try self.expect_ident();

                            var params = std.ArrayList([]const u8).init(self.ast_arena_allocator.allocator());
                            if (self.is_next(ast.Operator.lparen)) {
                                try self.expect(ast.Operator.lparen);
                                while (!self.is_next(ast.Operator.rparen)) {
                                    const param = try self.expect_ident();
                                    try params.append(param);
                                    try self.expect(ast.Operator.colon);
                                    try self.expect(ast.Operator.type_);

                                    if (self.is_next(ast.Operator.comma)) {
                                        try self.expect(ast.Operator.comma);
                                    } else {
                                        break;
                                    }
                                }
                                try self.expect(ast.Operator.rparen);
                            }

                            try self.expect(ast.Operator.eq);

                            try self.expect(ast.Operator.struct_);
                            try self.expect(ast.Operator.lbrace);

                            var fields = std.ArrayList(ast.StructField).init(self.ast_arena_allocator.allocator());
                            var methods = std.ArrayList(ast.Decl).init(self.ast_arena_allocator.allocator());

                            while (true) {
                                if (self.is_next(ast.Operator.rbrace)) {
                                    break;
                                }

                                if (self.is_next(ast.Operator.fun)) {
                                    const method = try self.parseFunDecl();
                                    try methods.append(method);
                                    continue;
                                }

                                const field_name = try self.expect_ident();
                                try self.expect(ast.Operator.colon);
                                const field_type = try self.type_();

                                try fields.append(.{ .name = field_name, .type_ = field_type });

                                if (self.is_next(ast.Operator.comma)) {
                                    try self.expect(ast.Operator.comma);
                                    continue;
                                } else {
                                    break;
                                }
                            }
                            try self.expect(ast.Operator.rbrace);

                            var extends = std.ArrayList(ast.ExtendField).init(self.ast_arena_allocator.allocator());
                            if (self.is_next(ast.Operator.extends)) {
                                try self.expect(ast.Operator.extends);
                                try self.expect(ast.Operator.lbrace);
                                while (!self.is_next(ast.Operator.rbrace)) {
                                    try self.expect(ast.Operator.dot);
                                    const field_name = try self.expect_ident();
                                    try self.expect(ast.Operator.eq);
                                    const field_type = try self.type_();
                                    try extends.append(.{ .name = field_name, .type_ = field_type });

                                    if (self.is_next(ast.Operator.comma)) {
                                        try self.expect(ast.Operator.comma);
                                    } else {
                                        break;
                                    }
                                }
                                try self.expect(ast.Operator.rbrace);
                            }

                            try self.expect(ast.Operator.semicolon);

                            return ast.Decl{ .type_ = .{
                                .name = name,
                                .params = params.items,
                                .fields = fields.items,
                                .methods = methods.items,
                                .extends = extends.items,
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

    fn parseFunDecl(self: *Parser) anyerror!ast.Decl {
        try self.expect(ast.Operator.fun);

        const name = try self.expect_ident();

        try self.expect(ast.Operator.lparen);
        var args = std.ArrayList(ast.FunParam).init(self.ast_arena_allocator.allocator());
        while (self.can_peek()) {
            if (self.is_next(ast.Operator.rparen)) {
                break;
            }

            const arg = try self.expect_ident();
            var t = ast.Type{ .unknown = true };
            if (self.is_next(ast.Operator.colon)) {
                try self.expect(ast.Operator.colon);
                t = try self.type_();
            }
            try args.append(.{ .name = arg, .type_ = t });

            if (self.is_next(ast.Operator.comma)) {
                try self.expect(ast.Operator.comma);
            } else {
                break;
            }
        }
        try self.expect(ast.Operator.rparen);

        var result_type = ast.Type{ .unknown = true };
        if (self.is_next(ast.Operator.colon)) {
            try self.expect(ast.Operator.colon);
            result_type = try self.type_();
        }

        try self.expect(ast.Operator.do);
        const body = try self.block(null);
        self.expect(ast.Operator.end) catch |err| {
            std.log.err("Error: {any} (next: {any})\n    {s}:{}", .{ err, self.peek(), @src().file, @src().line });
            return error.UnexpectedToken;
        };

        return ast.Decl{ .fun = .{
            .name = name,
            .params = args.items,
            .result_type = result_type,
            .body = body,
        } };
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

                            var t = ast.Type{ .unknown = true };
                            if (self.is_next(ast.Operator.colon)) {
                                try self.expect(ast.Operator.colon);
                                t = try self.type_();
                            }

                            try self.expect(ast.Operator.eq);
                            const value = try self.expr();

                            return ast.Statement{ .let = .{ .name = name, .value = value, .type_ = t } };
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
                            std.log.err("unexpected token: want statement but got {any}\n", .{self.tokens[self.position..]});
                            unreachable;
                        },
                    }
                },
                else => {
                    const e = try self.expr();

                    if (self.is_next(ast.Operator.eq)) {
                        try self.expect(ast.Operator.eq);

                        const rhs = try self.expr();

                        return ast.Statement{
                            .assign = .{
                                .type_ = ast.Type{ .unknown = true },
                                .lhs = e,
                                .rhs = rhs,
                            },
                        };
                    } else if (self.is_next(ast.Operator.push)) {
                        try self.expect(ast.Operator.push);

                        const rhs = try self.expr();

                        return ast.Statement{
                            .push = .{
                                .type_ = ast.Type{ .unknown = true },
                                .lhs = e,
                                .rhs = rhs,
                            },
                        };
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

        return self.operators();
    }

    const ParseOperatorType = enum {
        op,
        project,
        call,
        index,
        as,
    };

    const ParseOperator = union(ParseOperatorType) {
        op: ast.Operator,
        project: bool,
        call: bool,
        index: bool,
        as: bool,
    };

    fn operators(self: *Parser) anyerror!ast.Expression {
        const table = @constCast(&[_][]ParseOperator{
            @constCast(&[_]ParseOperator{
                .{ .op = .eqeq },
            }),
            @constCast(&[_]ParseOperator{
                .{ .op = .langle },
                .{ .op = .rangle },
                .{ .op = .lte },
                .{ .op = .gte },
            }),
            @constCast(&[_]ParseOperator{
                .{ .op = .plus },
                .{ .op = .minus },
            }),
            @constCast(&[_]ParseOperator{
                .{ .op = .star },
                .{ .op = .percent },
            }),
            @constCast(&[_]ParseOperator{
                .{ .project = true },
                .{ .call = true },
                .{ .index = true },
                .{ .as = true },
            }),
        });

        return self.operatorN(table, 0);
    }

    fn operatorN(self: *Parser, table: [][]ParseOperator, n: usize) anyerror!ast.Expression {
        if (n == table.len) {
            return self.operatorBase();
        }

        var current = try self.operatorN(table, n + 1);

        while (self.peek()) |_| {
            var found = false;

            for (table[n]) |target| {
                switch (target) {
                    .op => |op| {
                        if (!self.is_next(op)) {
                            continue;
                        }

                        try self.expect(op);

                        const lhs = try self.ast_arena_allocator.allocator().create(ast.Expression);
                        lhs.* = current;

                        const rhs = try self.ast_arena_allocator.allocator().create(ast.Expression);
                        rhs.* = try self.operatorN(table, n + 1);

                        current = ast.Expression{ .binop = .{
                            .op = op,
                            .lhs = lhs,
                            .rhs = rhs,
                        } };
                        found = true;
                    },
                    .call => {
                        if (!self.is_next(ast.Operator.lparen)) {
                            continue;
                        }

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

                        const callee = try self.ast_arena_allocator.allocator().create(ast.Expression);
                        callee.* = current;

                        current = ast.Expression{ .call = .{
                            .label_prefix = null,
                            .callee = callee,
                            .args = args.items,
                        } };
                    },
                    .index => {
                        if (!self.is_next(ast.Operator.lbracket)) {
                            continue;
                        }

                        try self.expect(ast.Operator.lbracket);
                        const index = try self.expr();
                        try self.expect(ast.Operator.rbracket);

                        const lhs = try self.ast_arena_allocator.allocator().create(ast.Expression);
                        lhs.* = current;

                        const rhs = try self.ast_arena_allocator.allocator().create(ast.Expression);
                        rhs.* = index;

                        current = ast.Expression{ .index = .{
                            .type_ = ast.Type{ .unknown = true },
                            .elem_type = ast.Type{ .unknown = true },
                            .lhs = lhs,
                            .rhs = rhs,
                        } };
                    },
                    .project => {
                        if (!self.is_next(ast.Operator.dot)) {
                            continue;
                        }

                        try self.expect(ast.Operator.dot);

                        const lhs = try self.ast_arena_allocator.allocator().create(ast.Expression);
                        lhs.* = current;

                        const rhs = try self.expect_ident();

                        current = ast.Expression{ .project = .{
                            .index = -1,
                            .result_type = ast.Type{ .unknown = true },
                            .lhs = lhs,
                            .rhs = rhs,
                        } };
                    },
                    .as => {
                        if (!self.is_next(ast.Operator.as)) {
                            continue;
                        }

                        try self.expect(ast.Operator.as);

                        const lhs = try self.ast_arena_allocator.allocator().create(ast.Expression);
                        lhs.* = current;

                        const rhs = try self.type_();

                        current = ast.Expression{ .as = .{
                            .lhs = lhs,
                            .rhs = rhs,
                        } };
                    },
                }
            }

            if (!found) {
                break;
            }
        }

        return current;
    }

    fn operatorBase(self: *Parser) anyerror!ast.Expression {
        if (self.peek()) |token| {
            switch (token) {
                .number => |n| {
                    _ = try self.expect_number();

                    return ast.Expression{ .literal = ast.Literal{ .number = n } };
                },
                .string => |s| {
                    _ = self.consume();

                    return ast.Expression{ .literal = ast.Literal{ .string = s } };
                },
                .keyword => |op| {
                    switch (op) {
                        .true_ => {
                            _ = self.consume();

                            return ast.Expression{ .literal = ast.Literal{ .boolean = true } };
                        },
                        .false_ => {
                            _ = self.consume();

                            return ast.Expression{ .literal = ast.Literal{ .boolean = false } };
                        },
                        .new => {
                            try self.expect(ast.Operator.new);

                            const t = try self.type_();

                            try self.expect(ast.Operator.lbrace);

                            var initializers = std.ArrayList(ast.StructInitializer).init(self.ast_arena_allocator.allocator());

                            while (true) {
                                if (self.is_next(ast.Operator.rbrace)) {
                                    break;
                                }

                                try self.expect(ast.Operator.dot);
                                const field = try self.expect_ident();
                                try self.expect(ast.Operator.eq);
                                const e = try self.expr();

                                try initializers.append(.{ .field = field, .value = e });

                                if (self.is_next(ast.Operator.comma)) {
                                    try self.expect(ast.Operator.comma);
                                    continue;
                                } else {
                                    break;
                                }
                            }

                            try self.expect(ast.Operator.rbrace);

                            return ast.Expression{ .new = .{
                                .type_ = t,
                                .initializers = initializers.items,
                                .method_name = null,
                            } };
                        },
                        .lparen => {
                            try self.expect(ast.Operator.lparen);

                            const e = try self.expr();
                            try self.expect(ast.Operator.rparen);

                            return e;
                        },
                        .type_ => {
                            try self.expect(ast.Operator.type_);

                            const t = try self.type_();

                            return ast.Expression{ .type_ = t };
                        },
                        .sizeof => {
                            try self.expect(ast.Operator.sizeof);

                            const t = try self.type_();

                            return ast.Expression{ .sizeof = t };
                        },
                        else => {
                            std.log.err("unexpected token: want lparen but got {any} ({any})\n", .{ token, self.tokens[self.position..] });
                            unreachable;
                        },
                    }
                },
                .ident => |ident| {
                    _ = try self.expect_ident();

                    return ast.Expression{ .var_ = ident };
                },
            }
        }

        return error.UnexpectedEos;
    }

    fn type_(self: *Parser) anyerror!ast.Type {
        const token = self.consume().?;
        switch (token) {
            .ident => |current| {
                if (std.mem.eql(u8, current, "int")) {
                    return ast.Type{ .int = true };
                } else if (std.mem.eql(u8, current, "bool")) {
                    return ast.Type{ .bool_ = true };
                } else if (std.mem.eql(u8, current, "byte")) {
                    return ast.Type{ .byte = true };
                } else {
                    if (self.is_next(ast.Operator.lparen)) {
                        try self.expect(ast.Operator.lparen);

                        var params = std.ArrayList(ast.Type).init(self.ast_arena_allocator.allocator());
                        while (!self.is_next(ast.Operator.rparen)) {
                            const p = try self.type_();
                            try params.append(p);

                            if (self.is_next(ast.Operator.comma)) {
                                try self.expect(ast.Operator.comma);
                            } else {
                                break;
                            }
                        }
                        try self.expect(ast.Operator.rparen);

                        return ast.Type{ .ident = .{
                            .name = current,
                            .params = params.items,
                        } };
                    }

                    return ast.Type{ .ident = .{
                        .name = current,
                        .params = &[_]ast.Type{},
                    } };
                }
            },
            .keyword => |keyword| {
                switch (keyword) {
                    .struct_ => {
                        var fields = std.ArrayList(ast.StructField).init(self.ast_arena_allocator.allocator());

                        try self.expect(ast.Operator.lbrace);
                        while (!self.is_next(ast.Operator.rbrace)) {
                            const field = try self.expect_ident();
                            try self.expect(ast.Operator.colon);
                            const t = try self.type_();
                            try fields.append(.{ .name = field, .type_ = t });

                            if (self.is_next(ast.Operator.comma)) {
                                try self.expect(ast.Operator.comma);
                            } else {
                                break;
                            }
                        }
                        try self.expect(ast.Operator.rbrace);

                        return ast.Type{ .struct_ = fields.items };
                    },
                    .lbracket => {
                        try self.expect(ast.Operator.star);
                        try self.expect(ast.Operator.rbracket);

                        const t = try self.type_();

                        const ptr = try self.ast_arena_allocator.allocator().create(ast.Type);
                        ptr.* = t;

                        return ast.Type{ .ptr = .{ .type_ = ptr } };
                    },
                    .type_ => {
                        return ast.Type{ .type_ = true };
                    },
                    else => {
                        std.log.err("unexpected token: want type but got {any}\n", .{self.tokens[self.position..]});
                        unreachable;
                    },
                }
            },
            else => {
                std.log.err("unexpected token: want type but got {any}\n", .{self.tokens[self.position..]});
                unreachable;
            },
        }

        return error.UnexpectedEos;
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

    var posTokens = std.ArrayList(utils.Positioned(ast.Token)).init(std.testing.allocator);
    defer posTokens.deinit();

    for (cases) |case| {
        for (case.tokens) |token| {
            try posTokens.append(utils.Positioned(ast.Token){ .position = 0, .data = token });
        }
    }

    for (cases) |case| {
        var parser = Parser.init(std.testing.allocator, posTokens.items);
        defer parser.deinit();

        const expr = parser.expr();

        try std.testing.expectEqualDeep(case.expected, expr);
    }
}
