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
            .type_ => |*type_| {
                for (type_.methods) |*method| {
                    try self.desugarDecl(method);
                }
            },
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
                                    const callee = try self.arena_allocator.allocator().create(ast.Expression);
                                    callee.* = .{
                                        .project = .{
                                            .index = -1,
                                            .result_type = apply.params[0],
                                            .lhs = index.lhs,
                                            .rhs = "set_v",
                                        },
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
                                                .type_ = .{ .apply = apply },
                                                .label_prefix = "set_v",
                                            },
                                        },
                                    };
                                } else if (std.mem.eql(u8, apply.name, "slice")) {
                                    const callee = try self.arena_allocator.allocator().create(ast.Expression);
                                    callee.* = .{
                                        .project = .{
                                            .index = -1,
                                            .result_type = apply.params[0],
                                            .lhs = index.lhs,
                                            .rhs = "set",
                                        },
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
                                                .type_ = .{ .apply = apply },
                                                .label_prefix = "set",
                                            },
                                        },
                                    };
                                } else {}
                            },
                            else => {},
                        }
                    },
                    else => {},
                }
            },
            .push => |*push| {
                try self.desugarExpr(&push.lhs);
                try self.desugarExpr(&push.rhs);

                switch (push.type_) {
                    .apply => |apply| {
                        if (std.mem.eql(u8, apply.name, "vec")) {
                            const lhs = try self.arena_allocator.allocator().create(ast.Expression);
                            lhs.* = push.lhs;

                            const callee = try self.arena_allocator.allocator().create(ast.Expression);
                            callee.* = .{
                                .project = .{
                                    .index = -1,
                                    .result_type = apply.params[0],
                                    .lhs = lhs,
                                    .rhs = "push_v",
                                },
                            };

                            var args = std.ArrayList(ast.Expression).init(self.arena_allocator.allocator());
                            try args.append(push.rhs);

                            statement.* = .{
                                .expr = .{
                                    .call = .{
                                        .callee = callee,
                                        .args = args.items,
                                        .type_ = .{ .apply = apply },
                                        .label_prefix = "push_v",
                                    },
                                },
                            };
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
                try self.desugarExpr(index.rhs);

                switch (index.type_) {
                    .apply => |apply| {
                        if (std.mem.eql(u8, apply.name, "vec")) {
                            const callee = try self.arena_allocator.allocator().create(ast.Expression);
                            callee.* = .{
                                .project = .{
                                    .index = -1,
                                    .result_type = apply.params[0],
                                    .lhs = index.lhs,
                                    .rhs = "get_v",
                                },
                            };

                            var args = std.ArrayList(ast.Expression).init(self.arena_allocator.allocator());
                            try args.append(index.rhs.*);

                            expr.* = .{
                                .call = .{
                                    .callee = callee,
                                    .args = args.items,
                                    .type_ = .{ .apply = apply },
                                    .label_prefix = "get_v",
                                },
                            };
                        } else if (std.mem.eql(u8, apply.name, "slice")) {
                            const callee = try self.arena_allocator.allocator().create(ast.Expression);
                            callee.* = .{
                                .project = .{
                                    .index = -1,
                                    .result_type = apply.params[0],
                                    .lhs = index.lhs,
                                    .rhs = "get",
                                },
                            };

                            var args = std.ArrayList(ast.Expression).init(self.arena_allocator.allocator());
                            try args.append(index.rhs.*);

                            expr.* = .{
                                .call = .{
                                    .callee = callee,
                                    .args = args.items,
                                    .type_ = .{ .apply = apply },
                                    .label_prefix = "get",
                                },
                            };
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
                            expr.new.method_name = "_new_v";
                        } else if (std.mem.eql(u8, apply.name, "slice")) {
                            expr.new.method_name = "_new";
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
