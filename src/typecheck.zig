const std = @import("std");

const ast = @import("ast.zig");
const P = @import("profiler");

pub const TypecheckerError = error{
    VariableNotFound,
    UnexpectedType,
};

pub const Typechecker = struct {
    env: std.StringHashMap(ast.Type),
    arena_allocator: std.heap.ArenaAllocator,
    return_type: ?ast.Type,

    pub fn init(allocator: std.mem.Allocator) Typechecker {
        return Typechecker{
            .env = std.StringHashMap(ast.Type).init(allocator),
            .arena_allocator = std.heap.ArenaAllocator.init(allocator),
            .return_type = null,
        };
    }

    pub fn deinit(self: *Typechecker) void {
        self.env.deinit();
        self.arena_allocator.deinit();
    }

    fn assertType(expect: ast.Type, actual: ast.Type) anyerror!ast.Type {
        switch (actual) {
            .unknown => {
                return expect;
            },
            else => {},
        }

        switch (expect) {
            .bool_ => {
                switch (actual) {
                    .bool_ => {},
                    else => {
                        std.log.err("Expected bool, got {any}\n", .{actual});
                        return TypecheckerError.UnexpectedType;
                    },
                }
            },
            .byte => {
                switch (actual) {
                    .byte => {},
                    else => {
                        std.log.err("Expected byte, got {any}\n", .{actual});
                        return TypecheckerError.UnexpectedType;
                    },
                }
            },
            .int => {
                switch (actual) {
                    .int => {},
                    else => {
                        std.log.err("Expected int, got {any}\n", .{actual});
                        return TypecheckerError.UnexpectedType;
                    },
                }
            },
            .array => {
                switch (actual) {
                    .array => {
                        if (expect.array.size != actual.array.size) {
                            std.log.err("Expected array of size {any}, got {any}\n", .{ expect.array.size, actual.array.size });
                            return TypecheckerError.UnexpectedType;
                        }
                        _ = try assertType(expect.array.elem_type.*, actual.array.elem_type.*);
                    },
                    else => {
                        std.log.err("Expected array, got {any}\n", .{actual});
                        return TypecheckerError.UnexpectedType;
                    },
                }
            },
            .slice => {
                switch (actual) {
                    .slice => _ = try assertType(expect.slice.elem_type.*, actual.slice.elem_type.*),
                    else => {
                        std.log.err("Expected slice, got {any}\n", .{actual});
                        return TypecheckerError.UnexpectedType;
                    },
                }
            },
            .vec => {
                switch (actual) {
                    .vec => _ = try assertType(expect.vec.elem_type.*, actual.vec.elem_type.*),
                    else => {
                        std.log.err("Expected vec, got {any}\n", .{actual});
                        return TypecheckerError.UnexpectedType;
                    },
                }
            },
            .map => {
                switch (actual) {
                    .map => {
                        _ = try assertType(expect.map.key_type.*, actual.map.key_type.*);
                        _ = try assertType(expect.map.value_type.*, actual.map.value_type.*);
                    },
                    else => {
                        std.log.err("Expected map, got {any}\n", .{actual});
                        return TypecheckerError.UnexpectedType;
                    },
                }
            },
            .fun => {
                switch (actual) {
                    .fun => {
                        if (expect.fun.params.len != actual.fun.params.len) {
                            std.log.err("Expected function with {any} parameters, got {any}\n", .{ expect.fun.params.len, actual.fun.params.len });
                            return TypecheckerError.UnexpectedType;
                        }
                        for (expect.fun.params, 0..) |param, i| {
                            _ = try assertType(param, actual.fun.params[i]);
                        }
                        _ = try assertType(expect.fun.return_type.*, actual.fun.return_type.*);
                    },
                    else => {
                        std.log.err("Expected function, got {any}\n", .{actual});
                        return TypecheckerError.UnexpectedType;
                    },
                }
            },
            .struct_ => |expect_data| {
                switch (actual) {
                    .struct_ => |actual_data| {
                        if (expect_data.fields.len != actual_data.fields.len) {
                            std.log.err("Expected struct with {any} fields, got {any}\n", .{ expect_data.fields.len, actual_data.fields.len });
                            return TypecheckerError.UnexpectedType;
                        }
                        for (expect_data.fields, 0..) |field, i| {
                            _ = assertType(field.type_, actual_data.fields[i].type_) catch |err| {
                                std.log.err("Error in field {s}: {}\n   {s}:{}", .{ field.name, err, @src().file, @src().line });
                                return err;
                            };
                        }
                    },
                    else => {
                        std.log.err("Expected struct, got {any}\n", .{actual});
                        return TypecheckerError.UnexpectedType;
                    },
                }
            },
            .ptr => |ptr| {
                switch (actual) {
                    .ptr => _ = try assertType(ptr.elem_type.*, actual.ptr.elem_type.*),
                    else => {
                        std.log.err("Expected ptr, got {any}\n", .{actual});
                        return TypecheckerError.UnexpectedType;
                    },
                }
            },
            .unknown => {
                return actual;
            },
        }

        return expect;
    }

    fn typecheckExpr(self: *Typechecker, expr: *ast.Expression) anyerror!ast.Type {
        switch (expr.*) {
            .var_ => |var_| {
                return self.env.get(var_) orelse {
                    std.log.err("Variable not found: {s}\n", .{var_});
                    return error.VariableNotFound;
                };
            },
            .literal => |lit| {
                switch (lit) {
                    .number => {
                        return ast.Type{ .int = true };
                    },
                    .boolean => {
                        return ast.Type{ .bool_ = true };
                    },
                    .string => {
                        const elem = try self.arena_allocator.allocator().create(ast.Type);
                        elem.* = ast.Type{ .byte = true };

                        return ast.Type{ .ptr = .{ .elem_type = elem } };
                    },
                }
            },
            .binop => |binop| {
                const lhs = try self.typecheckExpr(binop.lhs);
                const rhs = try self.typecheckExpr(binop.rhs);

                switch (binop.op) {
                    .eqeq, .langle, .lte, .rangle, .gte => {
                        _ = try assertType(lhs, rhs);

                        return ast.Type{ .bool_ = true };
                    },
                    .plus, .minus, .star, .percent => {
                        _ = try assertType(lhs, rhs);
                        _ = try assertType(lhs, ast.Type{ .int = true });

                        return lhs;
                    },
                    else => {
                        unreachable;
                    },
                }

                return lhs;
            },
            .block => {
                unreachable;
            },
            .call => |call| {
                const fun_type = self.env.get(call.name) orelse {
                    std.log.err("Function not found: {s}\n", .{call.name});
                    return error.VariableNotFound;
                };

                switch (fun_type) {
                    .fun => |fun| {
                        if (fun.params.len != call.args.len) {
                            std.log.err("Function {s} expects {d} arguments, got {d}\n", .{ call.name, fun.params.len, call.args.len });
                            return error.UnexpectedType;
                        }

                        for (fun.params, 0..) |param, i| {
                            const arg = try self.typecheckExpr(&call.args[i]);
                            _ = assertType(param, arg) catch |err| {
                                std.log.err("Error in argument {any} ({any}, {any}): {}\n   {s}:{}", .{ call.args[i], param, arg, err, @src().file, @src().line });
                                return err;
                            };
                        }

                        return fun.return_type.*;
                    },
                    else => unreachable,
                }
            },
            .if_ => {
                unreachable;
            },
            .index => |index| {
                const lhs = index.lhs;
                const lhs_type = try self.typecheckExpr(lhs);

                const key_type = try lhs_type.getIndexType();
                const value_type = try lhs_type.getValueType();

                const rhs = index.rhs;
                const rhs_type = try self.typecheckExpr(rhs);

                _ = try assertType(key_type, rhs_type);

                expr.*.index.type_ = lhs_type;

                return value_type;
            },
            .new => |new| {
                var structData: ast.StructData = undefined;
                switch (new.type_) {
                    .struct_ => |d| {
                        structData = d;
                    },
                    .slice => {
                        var fields = std.ArrayList(ast.StructField).init(self.arena_allocator.allocator());
                        try fields.append(ast.StructField{
                            .name = "len",
                            .type_ = ast.Type{ .int = true },
                        });

                        structData = ast.StructData{
                            .fields = fields.items,
                        };
                    },
                    else => {},
                }

                for (new.initializers, 0..) |initializer, k| {
                    const field_type = try structData.getFieldType(initializer.field);

                    var value = initializer.value;
                    const t = try self.typecheckExpr(&value);

                    _ = try assertType(field_type, t);
                    expr.new.initializers[k].value = value;
                }

                return new.type_;
            },
            .project => |project| {
                const lhs = project.lhs;
                const lhs_type = try self.typecheckExpr(lhs);

                const structData = try lhs_type.getStructData();

                const field = project.rhs;
                const field_type = try structData.getFieldType(field);

                expr.project.struct_ = structData;

                return field_type;
            },
            .as => |as| {
                // var lhs = as.lhs.*;
                // const lhs_type = try self.typecheckExpr(&lhs);
                // switch (as.rhs) {
                //     .ptr => {
                //         switch (lhs_type) {
                //             .int => {},
                //             else => {
                //                 unreachable;
                //             },
                //         }
                //     },
                //     else => {
                //         unreachable;
                //     },
                // }

                return as.rhs;
            },
        }
    }

    fn typecheckStatement(self: *Typechecker, stmt: *ast.Statement) anyerror!void {
        switch (stmt.*) {
            .let => |let| {
                var value = let.value;

                const t = self.typecheckExpr(&value) catch |err| {
                    std.log.err("Error in expression {any}: {}\n   {s}:{}", .{ value, err, @src().file, @src().line });
                    return err;
                };
                try self.env.put(let.name, t);

                stmt.let.value = value;
            },
            .return_ => |ret| {
                var e = ret;
                const t = try self.typecheckExpr(&e);

                self.return_type.? = try assertType(self.return_type.?, t);

                stmt.*.return_ = e;
            },
            .expr => |expr| {
                var e = expr;
                _ = self.typecheckExpr(&e) catch |err| {
                    std.log.err("Error in expression {any}: {}\n   {s}:{}", .{ e, err, @src().file, @src().line });
                    return err;
                };
            },
            .if_ => |if_| {
                const cond_type = try self.typecheckExpr(if_.cond);
                _ = try assertType(ast.Type{ .bool_ = true }, cond_type);

                var then_ = if_.then_;

                try self.typecheckBlock(&then_);
                if (if_.else_) |else_| {
                    var e = else_;
                    try self.typecheckBlock(&e);
                }
            },
            .assign => |assign| {
                var lhs = assign.lhs;
                const lhs_type = try self.typecheckExpr(&lhs);

                var rhs = assign.rhs;
                const rhs_type = try self.typecheckExpr(&rhs);

                _ = try assertType(lhs_type, rhs_type);

                stmt.*.assign.lhs = lhs;
                stmt.*.assign.rhs = rhs;
                stmt.*.assign.type_ = lhs_type;
            },
            .push => |push| {
                var lhs = push.lhs;
                const lhs_type = try self.typecheckExpr(&lhs);

                const value_type = try lhs_type.getValueType();

                var rhs = push.rhs;
                const rhs_type = try self.typecheckExpr(&rhs);

                _ = try assertType(value_type, rhs_type);

                stmt.*.push.lhs = lhs;
                stmt.*.push.rhs = rhs;
                stmt.*.push.type_ = lhs_type;
            },
            .while_ => |while_| {
                const cond_type = try self.typecheckExpr(while_.cond);
                _ = try assertType(ast.Type{ .bool_ = true }, cond_type);

                var body = while_.body;
                try self.typecheckBlock(&body);
            },
        }
    }

    fn typecheckBlock(self: *Typechecker, block: *ast.Block) anyerror!void {
        for (block.statements, 0..) |stmt, i| {
            var s = stmt;
            self.typecheckStatement(&s) catch |err| {
                std.log.err("Error in statement {any}: {}\n   {s}:{}", .{ s, err, @src().file, @src().line });
                return err;
            };

            block.statements[i] = s;
        }

        if (block.expr) |expr| {
            _ = try self.typecheckExpr(expr);
        }
    }

    pub fn typecheck(self: *Typechecker, module: *ast.Module) anyerror!void {
        const zone = P.begin(@src(), "Typechecker.typecheck");
        defer zone.end();

        for (module.decls, 0..) |decl, i| {
            switch (decl) {
                .fun => |fun| {
                    var params = std.ArrayList(ast.Type).init(self.arena_allocator.allocator());

                    for (fun.params) |param| {
                        var t = ast.Type{ .unknown = true };
                        if (param.type_) |pt| {
                            t = pt;
                        }

                        try self.env.put(param.name, t);
                        try params.append(t);
                    }

                    self.return_type = fun.result_type;

                    const return_type = try self.arena_allocator.allocator().create(ast.Type);
                    return_type.* = fun.result_type;

                    if (self.env.contains(fun.name)) {
                        std.log.err("Function {s} already defined\n", .{fun.name});
                        unreachable;
                    }
                    try self.env.put(fun.name, ast.Type{ .fun = .{
                        .params = params.items,
                        .return_type = return_type,
                    } });

                    var body = fun.body;

                    self.typecheckBlock(&body) catch |err| {
                        std.log.err("Error in function {s}: {}\n   {s}:{}", .{ fun.name, err, @src().file, @src().line });
                        return err;
                    };

                    return_type.* = self.return_type.?;

                    const t = ast.Type{ .fun = .{
                        .params = params.items,
                        .return_type = return_type,
                    } };
                    try self.env.put(fun.name, t);

                    self.return_type = null;

                    module.decls[i].fun.body = body;
                },
                .let => |let| {
                    const value = let.value;

                    // FIXME: support other types
                    var t: ast.Type = ast.Type{ .int = true };
                    if (value) |v| {
                        var v_ = v;
                        t = try self.typecheckExpr(&v_);
                    }
                    try self.env.put(let.name, t);
                },
            }
        }
    }
};
