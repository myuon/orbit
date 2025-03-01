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

        return ast.Module{ .decls = decls.items };
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
                            try self.expect(ast.Operator.eq);
                            const type_value = try self.type_();
                            try self.expect(ast.Operator.semicolon);

                            return ast.Decl{ .type_ = .{
                                .name = name,
                                .type_ = type_value,
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
            var t: ?ast.Type = null;
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
                        .lte => {
                            try self.expect(ast.Operator.lte);

                            const rhs = try self.ast_arena_allocator.allocator().create(ast.Expression);
                            rhs.* = try self.expr1();

                            const newCurrent = try self.ast_arena_allocator.allocator().create(ast.Expression);
                            newCurrent.* = current;

                            current = ast.Expression{ .binop = .{
                                .op = .lte,
                                .lhs = newCurrent,
                                .rhs = rhs,
                            } };
                        },
                        .gte => {
                            try self.expect(ast.Operator.gte);

                            const rhs = try self.ast_arena_allocator.allocator().create(ast.Expression);
                            rhs.* = try self.expr1();

                            const newCurrent = try self.ast_arena_allocator.allocator().create(ast.Expression);
                            newCurrent.* = current;

                            current = ast.Expression{ .binop = .{
                                .op = .gte,
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
                        .percent => {
                            try self.expect(ast.Operator.percent);

                            const rhs = try self.ast_arena_allocator.allocator().create(ast.Expression);
                            rhs.* = try self.expr2();

                            const newCurrent = try self.ast_arena_allocator.allocator().create(ast.Expression);
                            newCurrent.* = current;

                            current = ast.Expression{ .binop = .{
                                .op = .percent,
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
                            try self.expect(ast.Operator.star);

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
        var current = try self.expr4();

        while (self.peek()) |token| {
            switch (token.keyword) {
                .lparen => {
                    try self.expect(ast.Operator.lparen);

                    var ident: []const u8 = undefined;
                    switch (current) {
                        .var_ => |v| {
                            ident = v;
                        },
                        else => {
                            const s = try std.json.stringifyAlloc(self.ast_arena_allocator.allocator(), current, .{});

                            std.log.err("unexpected token: want ident but got {s}\n", .{s});
                            unreachable;
                        },
                    }

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

                    current = ast.Expression{ .call = .{
                        .name = ident,
                        .args = args.items,
                    } };
                },
                .lbracket => {
                    try self.expect(ast.Operator.lbracket);
                    const index = try self.expr();
                    try self.expect(ast.Operator.rbracket);

                    const lhs = try self.ast_arena_allocator.allocator().create(ast.Expression);
                    lhs.* = current;

                    const rhs = try self.ast_arena_allocator.allocator().create(ast.Expression);
                    rhs.* = index;

                    current = ast.Expression{ .index = .{
                        .type_ = ast.Type{ .unknown = true },
                        .lhs = lhs,
                        .rhs = rhs,
                    } };
                },
                .dot => {
                    try self.expect(ast.Operator.dot);

                    const lhs = try self.ast_arena_allocator.allocator().create(ast.Expression);
                    lhs.* = current;

                    const rhs = try self.expect_ident();

                    current = ast.Expression{ .project = .{
                        .struct_ = ast.StructData{
                            .fields = &[_]ast.StructField{},
                            .methods = &[_]ast.Decl{},
                        },
                        .lhs = lhs,
                        .rhs = rhs,
                    } };
                },
                .as => {
                    try self.expect(ast.Operator.as);

                    const lhs = try self.ast_arena_allocator.allocator().create(ast.Expression);
                    lhs.* = current;

                    const rhs = try self.type_();

                    current = ast.Expression{ .as = .{
                        .lhs = lhs,
                        .rhs = rhs,
                    } };
                },
                else => {
                    break;
                },
            }
        }

        return current;
    }

    fn expr4(self: *Parser) anyerror!ast.Expression {
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
                            } };
                        },
                        .lparen => {
                            try self.expect(ast.Operator.lparen);

                            const e = try self.expr();
                            try self.expect(ast.Operator.rparen);

                            return e;
                        },
                        else => {
                            std.log.err("unexpected token: want lparen but got {any}\n", .{token});
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
                } else if (std.mem.eql(u8, current, "ptr")) {
                    try self.expect(ast.Operator.lparen);
                    const t = try self.type_();
                    try self.expect(ast.Operator.rparen);

                    const elem_type = try self.ast_arena_allocator.allocator().create(ast.Type);
                    elem_type.* = t;

                    return ast.Type{ .ptr = .{
                        .elem_type = elem_type,
                    } };
                } else if (std.mem.eql(u8, current, "array")) {
                    try self.expect(ast.Operator.lparen);
                    const t = try self.type_();
                    try self.expect(ast.Operator.comma);
                    const size = try self.expect_number();
                    try self.expect(ast.Operator.rparen);

                    const elem_type = try self.ast_arena_allocator.allocator().create(ast.Type);
                    elem_type.* = t;

                    return ast.Type{ .array = .{
                        .elem_type = elem_type,
                        .size = size,
                    } };
                } else if (std.mem.eql(u8, current, "slice")) {
                    try self.expect(ast.Operator.lparen);
                    const t = try self.type_();
                    try self.expect(ast.Operator.rparen);

                    const elem_type = try self.ast_arena_allocator.allocator().create(ast.Type);
                    elem_type.* = t;

                    return ast.Type{ .slice = .{ .elem_type = elem_type } };
                } else if (std.mem.eql(u8, current, "vec")) {
                    try self.expect(ast.Operator.lparen);
                    const t = try self.type_();
                    try self.expect(ast.Operator.rparen);

                    const elem_type = try self.ast_arena_allocator.allocator().create(ast.Type);
                    elem_type.* = t;

                    return ast.Type{ .vec = .{ .elem_type = elem_type } };
                } else if (std.mem.eql(u8, current, "map")) {
                    try self.expect(ast.Operator.lparen);
                    const k = try self.type_();
                    try self.expect(ast.Operator.comma);
                    const v = try self.type_();
                    try self.expect(ast.Operator.rparen);

                    const key_type = try self.ast_arena_allocator.allocator().create(ast.Type);
                    key_type.* = k;

                    const value_type = try self.ast_arena_allocator.allocator().create(ast.Type);
                    value_type.* = v;

                    return ast.Type{ .map = .{
                        .key_type = key_type,
                        .value_type = value_type,
                    } };
                } else {
                    const t = try self.ast_arena_allocator.allocator().create(ast.Type);

                    return ast.Type{ .ident = .{
                        .name = current,
                        .type_ = t,
                    } };
                }
            },
            .keyword => |keyword| {
                switch (keyword) {
                    .struct_ => {
                        var fields = std.ArrayList(ast.StructField).init(self.ast_arena_allocator.allocator());
                        var methods = std.ArrayList(ast.Decl).init(self.ast_arena_allocator.allocator());

                        try self.expect(ast.Operator.lbrace);

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

                        return ast.Type{
                            .struct_ = ast.StructData{
                                .fields = fields.items,
                                .methods = methods.items,
                            },
                        };
                    },
                    else => {
                        std.log.err("unexpected token: want type but got {any}\n", .{self.tokens[self.position..]});
                        unreachable;
                    },
                }
            },
            else => {
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

    for (cases) |case| {
        var parser = Parser.init(std.testing.allocator, case.tokens);
        defer parser.deinit();

        const expr = parser.expr();

        try std.testing.expectEqualDeep(case.expected, expr);
    }
}
