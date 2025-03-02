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
    type_defs: ?ast.TypeDefs,

    pub fn init(allocator: std.mem.Allocator) Typechecker {
        return Typechecker{
            .env = std.StringHashMap(ast.Type).init(allocator),
            .arena_allocator = std.heap.ArenaAllocator.init(allocator),
            .return_type = null,
            .type_defs = null,
        };
    }

    pub fn deinit(self: *Typechecker) void {
        self.env.deinit();
        self.arena_allocator.deinit();
    }

    fn assertType(self: *Typechecker, expect: ast.Type, actual: ast.Type) anyerror!ast.Type {
        switch (actual) {
            .unknown => {
                return expect;
            },
            else => {},
        }
        if (std.meta.eql(expect, actual)) {
            return expect;
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
            .fun => {
                switch (actual) {
                    .fun => {
                        if (expect.fun.params.len != actual.fun.params.len) {
                            std.log.err("Expected function with {any} parameters, got {any}\n", .{ expect.fun.params.len, actual.fun.params.len });
                            return TypecheckerError.UnexpectedType;
                        }
                        for (expect.fun.params, 0..) |param, i| {
                            _ = try self.assertType(param, actual.fun.params[i]);
                        }
                        _ = try self.assertType(expect.fun.return_type.*, actual.fun.return_type.*);
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
                            _ = self.assertType(field.type_, actual_data.fields[i].type_) catch |err| {
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
            .ident => {
                switch (actual) {
                    .ident => {
                        if (!std.mem.eql(u8, expect.ident, actual.ident)) {
                            std.log.err("Expected ident {s}, got {s}\n", .{ expect.ident, actual.ident });
                            return TypecheckerError.UnexpectedType;
                        }
                    },
                    else => {
                        const expect_type = self.env.get(expect.ident) orelse {
                            std.log.err("Expected ident {s}, got {any}\n", .{ expect.ident, actual });
                            return TypecheckerError.UnexpectedType;
                        };

                        _ = try self.assertType(expect_type, actual);
                    },
                }
            },
            .apply => |eapply| {
                switch (actual) {
                    .apply => |aapply| {
                        if (std.mem.eql(u8, eapply.name, aapply.name)) {
                            if (eapply.params.len != aapply.params.len) {
                                std.log.err("Expected {any} parameters, got {any}\n", .{ eapply.params.len, aapply.params.len });
                                return TypecheckerError.UnexpectedType;
                            }

                            for (0..eapply.params.len) |i| {
                                _ = try self.assertType(eapply.params[i], aapply.params[i]);
                            }
                        }
                    },
                    else => {
                        std.log.err("Expected apply, got {any}\n", .{actual});
                        return TypecheckerError.UnexpectedType;
                    },
                }
            },
            .forall => {
                unreachable;
            },
            .unknown => {
                return actual;
            },
            .ptr => |ptr| {
                switch (actual) {
                    .ptr => |actual_ptr| {
                        const t = try self.arena_allocator.allocator().create(ast.Type);
                        t.* = try self.assertType(ptr.type_.*, actual_ptr.type_.*);

                        return ast.Type{ .ptr = .{ .type_ = t } };
                    },
                    else => {
                        std.log.err("Expected {any}, got {any}\n", .{ expect, actual });
                        return TypecheckerError.UnexpectedType;
                    },
                }
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
                        const t = try self.arena_allocator.allocator().create(ast.Type);
                        t.* = ast.Type{ .byte = true };

                        return ast.Type{ .ptr = .{ .type_ = t } };
                    },
                }
            },
            .binop => |binop| {
                const lhs = try self.typecheckExpr(binop.lhs);
                const rhs = try self.typecheckExpr(binop.rhs);

                switch (binop.op) {
                    .eqeq, .langle, .lte, .rangle, .gte => {
                        _ = try self.assertType(lhs, rhs);

                        return ast.Type{ .bool_ = true };
                    },
                    .plus, .minus, .star, .percent => {
                        _ = try self.assertType(lhs, rhs);
                        _ = try self.assertType(lhs, ast.Type{ .int = true });

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
                var fun_type: ast.Type = undefined;
                switch (call.callee.*) {
                    .var_ => |ident| {
                        fun_type = self.env.get(ident) orelse {
                            std.log.err("Function not found: {s}\n", .{ident});
                            return error.VariableNotFound;
                        };
                    },
                    .project => |project| {
                        const lhs_type = try self.typecheckExpr(project.lhs);

                        const method_type = try self.typecheckExpr(call.callee);
                        switch (method_type) {
                            .fun => |fun| {
                                _ = try self.assertType(fun.params[0], lhs_type);

                                fun_type = ast.Type{ .fun = .{
                                    .params = fun.params[1..],
                                    .return_type = fun.return_type,
                                } };
                            },
                            else => {
                                unreachable;
                            },
                        }
                    },
                    else => {
                        std.log.err("Expected ident, got {any}\n", .{call.callee});
                        return TypecheckerError.UnexpectedType;
                    },
                }

                switch (fun_type) {
                    .fun => |fun| {
                        if (fun.params.len != call.args.len) {
                            std.log.err("Function {any} expects {d} arguments, got {d}\n", .{ call.callee, fun.params.len, call.args.len });
                            return error.UnexpectedType;
                        }

                        for (fun.params, 0..) |param, i| {
                            const arg = try self.typecheckExpr(&call.args[i]);
                            _ = self.assertType(param, arg) catch |err| {
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

                _ = try self.assertType(key_type, rhs_type);

                expr.*.index.type_ = lhs_type;

                return value_type;
            },
            .new => |new| {
                var structData: ast.StructData = undefined;
                switch (new.type_) {
                    .struct_ => |d| {
                        structData = d;
                    },
                    .ident => |ident| {
                        // FIXME: check against initializers
                        return self.env.get(ident) orelse {
                            std.log.err("Struct not found: {s}\n", .{ident});
                            return error.VariableNotFound;
                        };
                    },
                    .apply => |apply| {
                        if (std.mem.eql(u8, apply.name, "vec")) {
                            apply.applied.* = new.type_;

                            return apply.applied.*;
                        } else if (std.mem.eql(u8, apply.name, "map")) {
                            apply.applied.* = new.type_;

                            return apply.applied.*;
                        }

                        const info = self.type_defs.?.get(apply.name) orelse {
                            std.log.err("Function not found: {s}\n", .{apply.name});
                            return error.VariableNotFound;
                        };

                        const gt = try self.arena_allocator.allocator().create(ast.Type);
                        gt.* = .{ .struct_ = .{ .fields = info.fields, .methods = info.methods } };

                        const generalized = ast.Type{ .forall = .{ .params = info.params, .type_ = gt } };
                        const applied = try generalized.applyTypes(self.arena_allocator.allocator(), apply.params);

                        structData = try applied.getStructData();

                        apply.applied.* = applied;

                        return applied;
                    },
                    else => {
                        std.log.err("Expected struct, got {any}\n", .{new.type_});
                        unreachable;
                    },
                }

                for (new.initializers, 0..) |initializer, k| {
                    const field_type = try structData.getFieldType(initializer.field);

                    var value = initializer.value;
                    const t = try self.typecheckExpr(&value);

                    _ = try self.assertType(field_type, t);
                    expr.new.initializers[k].value = value;
                }

                return new.type_;
            },
            .project => |project| {
                const lhs = project.lhs;
                const lhs_type = try self.typecheckExpr(lhs);

                var structData: ast.StructData = undefined;
                switch (lhs_type) {
                    .struct_ => |d| {
                        structData = d;
                    },
                    .ident => |ident| {
                        const info = self.type_defs.?.get(ident).?;

                        structData = .{ .fields = info.fields, .methods = info.methods };
                    },
                    else => {
                        std.log.err("Expected struct, got {any}\n", .{lhs_type});
                        unreachable;
                    },
                }
                expr.project.struct_ = structData;

                const field = project.rhs;

                if (structData.hasField(field)) {
                    return try structData.getFieldType(field);
                } else if (structData.hasMethod(field)) {
                    const method = structData.getMethod(field);

                    var params = std.ArrayList(ast.Type).init(self.arena_allocator.allocator());
                    for (method.params) |fp| {
                        try params.append(fp.type_.?);
                    }

                    const return_type = try self.arena_allocator.allocator().create(ast.Type);
                    return_type.* = method.result_type;

                    return ast.Type{ .fun = .{
                        .params = params.items,
                        .return_type = return_type,
                    } };
                }

                std.log.err("Expected field or method, got {any}\n", .{field});
                return TypecheckerError.UnexpectedType;
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

                self.return_type.? = try self.assertType(self.return_type.?, t);

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
                _ = try self.assertType(ast.Type{ .bool_ = true }, cond_type);

                var then_ = if_.then_;

                try self.typecheckBlock(&then_);
                if (if_.else_) |else_| {
                    var e = else_;
                    try self.typecheckBlock(&e);
                }
            },
            .assign => |assign| {
                var lhs = assign.lhs;
                const lhs_type = self.typecheckExpr(&lhs) catch |err| {
                    std.log.err("Error in expression {any}: {}\n   {s}:{}", .{ lhs, err, @src().file, @src().line });
                    return err;
                };

                var rhs = assign.rhs;
                const rhs_type = self.typecheckExpr(&rhs) catch |err| {
                    std.log.err("Error in expression {any}: {}\n   {s}:{}", .{ rhs, err, @src().file, @src().line });
                    return err;
                };

                _ = self.assertType(lhs_type, rhs_type) catch |err| {
                    std.log.err("Error in assignment {any} != {any}: {}\n   {s}:{}", .{ lhs_type, rhs_type, err, @src().file, @src().line });
                    return err;
                };

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

                _ = try self.assertType(value_type, rhs_type);

                stmt.*.push.lhs = lhs;
                stmt.*.push.rhs = rhs;
                stmt.*.push.type_ = lhs_type;
            },
            .while_ => |while_| {
                const cond_type = try self.typecheckExpr(while_.cond);
                _ = try self.assertType(ast.Type{ .bool_ = true }, cond_type);

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

    fn typecheckDecl(self: *Typechecker, module: *ast.Module, decl: *ast.Decl) anyerror!void {
        switch (decl.*) {
            .fun => |fun| {
                self.type_defs = module.type_defs;

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

                decl.fun.body = body;
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
            .type_ => |type_decl| {
                try self.env.put(type_decl.name, .{ .ident = type_decl.name });

                var str = try type_decl.type_.getStructData();
                for (0..str.methods.len) |i| {
                    try self.typecheckDecl(module, &str.methods[i]);
                }

                decl.*.type_.type_ = type_decl.type_;

                try module.type_defs.put(type_decl.name, .{
                    .name = type_decl.name,
                    .params = type_decl.params,
                    .fields = str.fields,
                    .methods = str.methods,
                    .extends = type_decl.extends,
                });
            },
        }
    }

    pub fn typecheck(self: *Typechecker, module: *ast.Module) anyerror!void {
        const zone = P.begin(@src(), "Typechecker.typecheck");
        defer zone.end();

        for (0..module.decls.len) |i| {
            try self.typecheckDecl(module, &module.decls[i]);
        }
    }
};
