const std = @import("std");
const ast = @import("ast.zig");

pub const Desugarer = struct {
    arena_allocator: std.heap.ArenaAllocator,

    pub fn init(allocator: std.mem.Allocator) Desugarer {
        return Desugarer{ .arena_allocator = std.heap.ArenaAllocator.init(allocator) };
    }

    pub fn deinit(self: *@This()) void {
        self.arena_allocator.deinit();
    }

    fn desugarModule(self: *@This(), module: *ast.Module) anyerror!void {
        for (module.decls) |*decl| {
            try self.desugarDecl(decl);
        }
    }

    fn desugarDecl(self: *@This(), decl: *ast.Decl) anyerror!void {
        switch (decl.*) {
            .fun => |*fun| {
                try self.desugarBlock(&fun.body);
            },
            .let => |*let| {
                if (let.value) |*value| {
                    try self.desugarExpr(value);
                }
            },
            .type_ => {},
        }
    }

    fn desugarBlock(self: *@This(), block: *ast.Block) anyerror!void {
        for (block.statements) |*statement| {
            try self.desugarStatement(statement);
        }
        if (block.expr) |expr| {
            try self.desugarExpr(expr);
        }
    }

    fn desugarStatement(self: *@This(), statement: *ast.Statement) anyerror!void {
        switch (statement.*) {
            .let => |*let| {
                try self.desugarExpr(&let.value);
            },
            .return_ => |*return_| {
                try self.desugarExpr(return_);
            },
            .expr => |*expr| {
                try self.desugarExpr(expr);
            },
            .if_ => |*if_| {
                try self.desugarExpr(if_.cond);
                try self.desugarBlock(&if_.then_);
                if (if_.else_) |*else_| {
                    try self.desugarBlock(else_);
                }
            },
            .assign => |*assign| {
                try self.desugarExpr(&assign.rhs);

                switch (assign.lhs) {
                    .index => |index| {
                        switch (index.type_) {
                            .apply => |apply| {
                                if (std.mem.eql(u8, apply.name, "vec")) {
                                    switch (apply.params[0]) {
                                        .int => {
                                            const callee = try self.arena_allocator.allocator().create(ast.Expression);
                                            callee.* = .{
                                                .var_ = "set_vec_int",
                                            };

                                            var args = std.ArrayList(ast.Expression).init(self.arena_allocator.allocator());
                                            try args.append(index.lhs.*);
                                            try args.append(index.rhs.*);
                                            try args.append(assign.rhs);

                                            statement.* = .{
                                                .expr = .{
                                                    .call = .{
                                                        .callee = callee,
                                                        .args = args.items,
                                                        .type_ = null,
                                                        .label_prefix = "set_vec_int",
                                                    },
                                                },
                                            };
                                        },
                                        else => {
                                            unreachable;
                                        },
                                    }
                                } else {
                                    try self.desugarExpr(&assign.rhs);
                                }
                            },
                            else => {
                                try self.desugarExpr(&assign.rhs);
                            },
                        }
                    },
                    else => {
                        try self.desugarExpr(&assign.rhs);
                    },
                }
            },
            .push => |*push| {
                try self.desugarExpr(&push.lhs);
                try self.desugarExpr(&push.rhs);

                switch (push.type_) {
                    .apply => |apply| {
                        if (std.mem.eql(u8, apply.name, "vec")) {
                            switch (apply.params[0]) {
                                .int => {
                                    const callee = try self.arena_allocator.allocator().create(ast.Expression);
                                    callee.* = .{
                                        .var_ = "push_vec_int",
                                    };

                                    var args = std.ArrayList(ast.Expression).init(self.arena_allocator.allocator());
                                    try args.append(push.lhs);
                                    try args.append(push.rhs);

                                    statement.* = .{
                                        .expr = .{
                                            .call = .{
                                                .callee = callee,
                                                .args = args.items,
                                                .type_ = null,
                                                .label_prefix = "push_vec_int",
                                            },
                                        },
                                    };
                                },
                                else => {
                                    unreachable;
                                },
                            }
                        }
                    },
                    else => {
                        unreachable;
                    },
                }
            },
            .while_ => |*while_| {
                try self.desugarExpr(while_.cond);
                try self.desugarBlock(&while_.body);
            },
        }
    }

    fn desugarExpr(self: *@This(), expr: *ast.Expression) anyerror!void {
        switch (expr.*) {
            .var_ => {},
            .literal => {},
            .binop => |*binop| {
                try self.desugarExpr(binop.lhs);
                try self.desugarExpr(binop.rhs);
            },
            .block => |*block| {
                try self.desugarBlock(block);
            },
            .call => |*call| {
                try self.desugarExpr(call.callee);
                for (call.args) |*arg| {
                    try self.desugarExpr(arg);
                }
            },
            .if_ => |*if_| {
                try self.desugarExpr(if_.cond);
                try self.desugarBlock(&if_.then_);
                try self.desugarBlock(&if_.else_);
            },
            .index => |*index| {
                switch (index.type_) {
                    .apply => |apply| {
                        if (std.mem.eql(u8, apply.name, "vec")) {
                            switch (apply.params[0]) {
                                .int => {
                                    const callee = try self.arena_allocator.allocator().create(ast.Expression);
                                    callee.* = .{
                                        .var_ = "get_vec_int",
                                    };

                                    var args = std.ArrayList(ast.Expression).init(self.arena_allocator.allocator());
                                    try args.append(index.lhs.*);
                                    try args.append(index.rhs.*);

                                    expr.* = .{
                                        .call = .{
                                            .callee = callee,
                                            .args = args.items,
                                            .type_ = null,
                                            .label_prefix = "get_vec_int",
                                        },
                                    };
                                },
                                else => {
                                    unreachable;
                                },
                            }
                        } else {
                            try self.desugarExpr(index.lhs);
                            try self.desugarExpr(index.rhs);
                        }
                    },
                    else => {
                        try self.desugarExpr(index.lhs);
                        try self.desugarExpr(index.rhs);
                    },
                }
            },
            .new => {
                const new_expr = expr.new;
                switch (new_expr.type_) {
                    .apply => |apply| {
                        if (std.mem.eql(u8, apply.name, "vec")) {
                            const callee = try self.arena_allocator.allocator().create(ast.Expression);
                            callee.* = .{
                                .var_ = "new_vec",
                            };

                            var args = std.ArrayList(ast.Expression).init(self.arena_allocator.allocator());
                            try args.append(.{ .type_ = apply.params[0] });
                            try args.append(.{ .literal = .{ .number = 128 } });

                            expr.* = .{
                                .call = .{
                                    .callee = callee,
                                    .args = args.items,
                                    .type_ = null,
                                    .label_prefix = "new_vec",
                                },
                            };
                        } else if (std.mem.eql(u8, apply.name, "slice")) {
                            const callee = try self.arena_allocator.allocator().create(ast.Expression);
                            callee.* = .{
                                .var_ = "new_slice",
                            };

                            var args = std.ArrayList(ast.Expression).init(self.arena_allocator.allocator());
                            try args.append(.{ .type_ = apply.params[0] });
                            std.debug.assert(std.mem.eql(u8, new_expr.initializers[1].field, "len"));
                            try args.append(new_expr.initializers[1].value);

                            expr.* = .{
                                .call = .{
                                    .callee = callee,
                                    .args = args.items,
                                    .type_ = null,
                                    .label_prefix = "new_slice",
                                },
                            };
                        } else {
                            return;
                        }
                    },
                    .struct_ => {
                        return;
                    },
                    .ident => {
                        return;
                    },
                    else => {
                        std.log.err("Unexpected type in new expression: {any}", .{new_expr.type_});
                        return error.UnexpectedType;
                    },
                }
            },
            .project => |*project| {
                try self.desugarExpr(project.lhs);
            },
            .as => |*as| {
                try self.desugarExpr(as.lhs);
            },
            .type_ => {},
            .sizeof => {},
        }
    }

    pub fn execute(self: *@This(), module: *ast.Module) anyerror!void {
        try self.desugarModule(module);
    }
};
