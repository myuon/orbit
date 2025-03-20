const std = @import("std");
const ast = @import("ast.zig");

const MonomorphizationTarget = struct {
    symbol: []const u8,
    args: []ast.Type,
};

pub const MonomorphizationError = error{
    SymbolNotFound,
};

pub const Monomorphization = struct {
    arena_allocator: std.heap.ArenaAllocator,
    stack: std.ArrayList(MonomorphizationTarget),
    visited: std.StringHashMap(bool),
    assignments: ast.Assignments,

    pub fn init(allocator: std.mem.Allocator) Monomorphization {
        const arena_allocator = std.heap.ArenaAllocator.init(allocator);

        return Monomorphization{
            .arena_allocator = arena_allocator,
            .stack = std.ArrayList(MonomorphizationTarget).init(allocator),
            .visited = std.StringHashMap(bool).init(allocator),
            .assignments = ast.Assignments.init(allocator),
        };
    }

    pub fn deinit(self: *Monomorphization) void {
        self.stack.deinit();
        self.arena_allocator.deinit();
        self.visited.deinit();
        self.assignments.deinit();
    }

    fn monomorphExpression(self: *Monomorphization, target: MonomorphizationTarget, expr: ast.Expression) anyerror!ast.Expression {
        return switch (expr) {
            .var_ => expr,
            .literal => expr,
            .binop => |binop| {
                const lhs = try self.arena_allocator.allocator().create(ast.Expression);
                lhs.* = try self.monomorphExpression(target, binop.lhs.*);

                const rhs = try self.arena_allocator.allocator().create(ast.Expression);
                rhs.* = try self.monomorphExpression(target, binop.rhs.*);

                return ast.Expression{
                    .binop = .{
                        .op = binop.op,
                        .lhs = lhs,
                        .rhs = rhs,
                    },
                };
            },
            .block => |block| {
                var stmts = std.ArrayList(ast.Statement).init(self.arena_allocator.allocator());
                for (block.statements) |stmt| {
                    try stmts.append(try self.monomorphStatement(target, stmt));
                }

                return ast.Expression{ .block = ast.Block{
                    .statements = stmts.items,
                    .expr = null,
                } };
            },
            .call => |call| {
                const callee = try self.arena_allocator.allocator().create(ast.Expression);
                callee.* = try self.monomorphExpression(target, call.callee.*);

                var new_args = std.ArrayList(ast.Expression).init(self.arena_allocator.allocator());
                for (call.args) |arg| {
                    try new_args.append(try self.monomorphExpression(target, arg));
                }

                var new_call = ast.CallExpr{
                    .label_prefix = null,
                    .callee = callee,
                    .args = new_args.items,
                };

                var label = std.ArrayList(u8).init(self.arena_allocator.allocator());
                if (call.type_) |t| {
                    switch (t) {
                        .ident => |ident| {
                            try std.fmt.format(label.writer(), "{s}_", .{ident});
                        },
                        .apply => |apply| {
                            try std.fmt.format(label.writer(), "{s}", .{apply.name});
                            for (apply.params) |param| {
                                try std.fmt.format(label.writer(), "_{any}", .{param});
                            }
                            try std.fmt.format(label.writer(), "_", .{});
                        },
                        else => {},
                    }
                }
                try label.appendSlice(call.label_prefix.?);

                switch (call.callee.*) {
                    .var_ => |v| {
                        self.assignments.clearAndFree();

                        const argTypes = try new_call.getArgTypes(self.arena_allocator.allocator());

                        try self.stack.append(.{
                            .symbol = v,
                            .args = argTypes,
                        });

                        for (argTypes) |t| {
                            try std.fmt.format(label.writer(), "_{any}", .{t});
                        }
                    },
                    else => {},
                }

                new_call.label_prefix = label.items;

                return ast.Expression{
                    .call = new_call,
                };
            },
            .if_ => |if_| {
                const cond = try self.arena_allocator.allocator().create(ast.Expression);
                cond.* = try self.monomorphExpression(target, if_.cond.*);

                return ast.Expression{
                    .if_ = .{
                        .cond = cond,
                        .then_ = try self.monomorphBlock(target, if_.then_),
                        .else_ = try self.monomorphBlock(target, if_.else_),
                    },
                };
            },
            .index => |index| {
                const lhs = try self.arena_allocator.allocator().create(ast.Expression);
                lhs.* = try self.monomorphExpression(target, index.lhs.*);

                const rhs = try self.arena_allocator.allocator().create(ast.Expression);
                rhs.* = try self.monomorphExpression(target, index.rhs.*);

                return ast.Expression{
                    .index = .{
                        .type_ = index.type_,
                        .elem_type = index.elem_type,
                        .lhs = lhs,
                        .rhs = rhs,
                    },
                };
            },
            .new => |new_expr| {
                var new_initializers = std.ArrayList(ast.StructInitializer).init(self.arena_allocator.allocator());
                for (new_expr.initializers) |inite| {
                    try new_initializers.append(.{
                        .field = inite.field,
                        .value = try self.monomorphExpression(target, inite.value),
                    });
                }

                switch (new_expr.type_) {
                    .ident => |ident| {
                        try self.stack.append(.{
                            .symbol = ident,
                            .args = &[_]ast.Type{},
                        });
                    },
                    .apply => |apply| {
                        try self.stack.append(.{
                            .symbol = apply.name,
                            .args = apply.params,
                        });
                    },
                    else => {},
                }

                return ast.Expression{
                    .new = .{
                        .type_ = new_expr.type_,
                        .initializers = new_initializers.items,
                    },
                };
            },
            .project => |project| {
                const lhs = try self.arena_allocator.allocator().create(ast.Expression);
                lhs.* = try self.monomorphExpression(target, project.lhs.*);

                return ast.Expression{
                    .project = .{
                        .index = project.index,
                        .result_type = project.result_type,
                        .lhs = lhs,
                        .rhs = project.rhs,
                    },
                };
            },
            .as => |as_expr| {
                const lhs = try self.arena_allocator.allocator().create(ast.Expression);
                lhs.* = try self.monomorphExpression(target, as_expr.lhs.*);

                return ast.Expression{
                    .as = .{
                        .lhs = lhs,
                        .rhs = as_expr.rhs,
                    },
                };
            },
            .type_ => |t| {
                return ast.Expression{
                    .type_ = try t.applyAssignments(self.arena_allocator.allocator(), self.assignments),
                };
            },
            .sizeof => |t| {
                return ast.Expression{
                    .sizeof = try t.applyAssignments(self.arena_allocator.allocator(), self.assignments),
                };
            },
        };
    }

    fn monomorphStatement(self: *Monomorphization, target: MonomorphizationTarget, stmt: ast.Statement) anyerror!ast.Statement {
        return switch (stmt) {
            .let => |let_stmt| {
                return ast.Statement{
                    .let = .{
                        .name = let_stmt.name,
                        .value = try self.monomorphExpression(target, let_stmt.value),
                    },
                };
            },
            .return_ => |return_expr| {
                return ast.Statement{
                    .return_ = try self.monomorphExpression(target, return_expr),
                };
            },
            .expr => |expr| {
                return ast.Statement{
                    .expr = try self.monomorphExpression(target, expr),
                };
            },
            .if_ => |if_stmt| {
                const cond = try self.arena_allocator.allocator().create(ast.Expression);
                cond.* = try self.monomorphExpression(target, if_stmt.cond.*);

                var else_: ?ast.Block = null;
                if (if_stmt.else_) |block| {
                    else_ = try self.monomorphBlock(target, block);
                }

                return ast.Statement{
                    .if_ = .{
                        .cond = cond,
                        .then_ = try self.monomorphBlock(target, if_stmt.then_),
                        .else_ = else_,
                    },
                };
            },
            .assign => |assign| {
                return ast.Statement{
                    .assign = .{
                        .type_ = assign.type_,
                        .lhs = try self.monomorphExpression(target, assign.lhs),
                        .rhs = try self.monomorphExpression(target, assign.rhs),
                    },
                };
            },
            .push => |push| {
                return ast.Statement{
                    .push = .{
                        .type_ = push.type_,
                        .lhs = try self.monomorphExpression(target, push.lhs),
                        .rhs = try self.monomorphExpression(target, push.rhs),
                    },
                };
            },
            .while_ => |while_stmt| {
                const cond = try self.arena_allocator.allocator().create(ast.Expression);
                cond.* = try self.monomorphExpression(target, while_stmt.cond.*);

                return ast.Statement{
                    .while_ = .{
                        .cond = cond,
                        .body = try self.monomorphBlock(target, while_stmt.body),
                    },
                };
            },
        };
    }

    fn monomorphBlock(self: *Monomorphization, target: MonomorphizationTarget, block: ast.Block) anyerror!ast.Block {
        var stmts = std.ArrayList(ast.Statement).init(self.arena_allocator.allocator());

        for (block.statements) |stmt| {
            try stmts.append(try self.monomorphStatement(target, stmt));
        }

        return ast.Block{
            .statements = stmts.items,
            .expr = null,
        };
    }

    fn monomorphDeclInModule(self: *Monomorphization, target: MonomorphizationTarget, module: ast.Module) anyerror!ast.Decl {
        for (module.decls) |decl| {
            switch (decl) {
                .fun => |fun| {
                    if (!std.mem.eql(u8, fun.name, target.symbol)) {
                        continue;
                    }

                    var type_params = std.ArrayList([]const u8).init(self.arena_allocator.allocator());
                    for (fun.params) |param| {
                        switch (param.type_) {
                            .type_ => {
                                try type_params.append(param.name);
                            },
                            else => {
                                break;
                            },
                        }
                    }

                    for (0..type_params.items.len) |i| {
                        try self.assignments.put(type_params.items[i], target.args[i]);
                    }

                    var name = std.ArrayList(u8).init(self.arena_allocator.allocator());
                    try name.appendSlice(fun.name);
                    for (target.args) |arg| {
                        try std.fmt.format(name.writer(), "_{s}", .{try arg.applyAssignments(self.arena_allocator.allocator(), self.assignments)});
                    }

                    var params = std.ArrayList(ast.FunParam).init(self.arena_allocator.allocator());
                    for (fun.params) |param| {
                        try params.append(.{
                            .name = param.name,
                            .type_ = try param.type_.applyAssignments(self.arena_allocator.allocator(), self.assignments),
                        });
                    }

                    const block = try self.monomorphBlock(target, fun.body);

                    return ast.Decl{
                        .fun = .{
                            .name = name.items,
                            .params = params.items,
                            .result_type = try fun.result_type.applyAssignments(self.arena_allocator.allocator(), self.assignments),
                            .body = block,
                        },
                    };
                },
                .let => |let| {
                    const symbol = let.name;
                    // Always compile the global-let
                    if (!self.visited.contains(symbol)) {
                        try self.stack.append(.{
                            .symbol = symbol,
                            .args = &[_]ast.Type{},
                        });
                    }

                    if (!std.mem.eql(u8, let.name, target.symbol)) {
                        continue;
                    }

                    var value: ?ast.Expression = null;
                    if (let.value) |v| {
                        value = try self.monomorphExpression(target, v);
                    }

                    return ast.Decl{
                        .let = .{
                            .name = let.name,
                            .value = value,
                        },
                    };
                },
                .type_ => |type_| {
                    if (!std.mem.eql(u8, type_.name, target.symbol)) {
                        continue;
                    }

                    std.debug.assert(type_.params.len == target.args.len);
                    for (0..type_.params.len) |i| {
                        try self.assignments.put(type_.params[i], target.args[i]);
                    }

                    var name = std.ArrayList(u8).init(self.arena_allocator.allocator());
                    try name.appendSlice(type_.name);
                    for (0..type_.params.len) |i| {
                        try std.fmt.format(name.writer(), "_{s}", .{target.args[i]});
                    }

                    var fields = std.ArrayList(ast.StructField).init(self.arena_allocator.allocator());
                    for (type_.fields) |field| {
                        try fields.append(.{
                            .name = field.name,
                            .type_ = try field.type_.applyAssignments(self.arena_allocator.allocator(), self.assignments),
                        });
                    }

                    var methods = std.ArrayList(ast.Decl).init(self.arena_allocator.allocator());
                    for (type_.methods) |method| {
                        var method_name = std.ArrayList(u8).init(self.arena_allocator.allocator());
                        try method_name.appendSlice(name.items);
                        try std.fmt.format(method_name.writer(), "_{s}", .{method.fun.name});

                        var params = std.ArrayList(ast.FunParam).init(self.arena_allocator.allocator());
                        for (method.fun.params) |param| {
                            try params.append(.{
                                .name = param.name,
                                .type_ = try param.type_.applyAssignments(self.arena_allocator.allocator(), self.assignments),
                            });
                        }

                        const block = try self.monomorphBlock(target, method.fun.body);

                        try methods.append(ast.Decl{
                            .fun = .{
                                .name = method_name.items,
                                .params = params.items,
                                .result_type = method.fun.result_type,
                                .body = block,
                            },
                        });
                    }

                    return ast.Decl{
                        .type_ = .{
                            .name = name.items,
                            .params = &[_][]const u8{},
                            .methods = methods.items,
                            .fields = fields.items,
                            .extends = type_.extends,
                        },
                    };
                },
            }
        }

        std.log.err("Symbol not found: {s}", .{target.symbol});
        return MonomorphizationError.SymbolNotFound;
    }

    fn monomorph(self: *Monomorphization, module: *ast.Module) anyerror!void {
        var decls = std.ArrayList(ast.Decl).init(self.arena_allocator.allocator());

        while (self.stack.popOrNull()) |target| {
            const targetKey = try std.fmt.allocPrint(self.arena_allocator.allocator(), "{any}", .{target});
            if (self.visited.contains(targetKey)) {
                continue;
            }

            try self.visited.put(targetKey, true);

            if (std.mem.eql(u8, target.symbol, "map")) {
                continue;
            } else if (std.mem.eql(u8, target.symbol, "vec")) {
                continue;
            } else if (std.mem.eql(u8, target.symbol, "slice")) {
                continue;
            } else if (std.mem.eql(u8, target.symbol, "array")) {
                continue;
            }

            try decls.append(try self.monomorphDeclInModule(target, module.*));
        }

        module.decls = decls.items;
    }

    pub fn execute(self: *Monomorphization, entrypoint: []const u8, module: *ast.Module) anyerror!void {
        try self.stack.append(.{
            .symbol = entrypoint,
            .args = &[_]ast.Type{},
        });

        // Implicitly called builtin symbols
        try self.stack.append(.{ .symbol = "push_vec_int", .args = &[_]ast.Type{} });
        try self.stack.append(.{ .symbol = "allocate_memory", .args = &[_]ast.Type{} });
        try self.stack.append(.{ .symbol = "hp", .args = &[_]ast.Type{} });

        try self.monomorph(module);
    }
};
