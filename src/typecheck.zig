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
    prng: std.Random.Xoshiro256,
    context: ?[]const u8,
    assignments: std.StringHashMap(ast.Assignments),
    type_params_context: []const []const u8,
    self_object: ?struct { index: i32, expr: ast.Expression, type_: ast.Type },

    pub fn init(allocator: std.mem.Allocator) Typechecker {
        const prng = std.rand.DefaultPrng.init(blk: {
            var seed: u64 = undefined;
            std.posix.getrandom(std.mem.asBytes(&seed)) catch unreachable;
            break :blk seed;
        });

        return Typechecker{
            .env = std.StringHashMap(ast.Type).init(allocator),
            .arena_allocator = std.heap.ArenaAllocator.init(allocator),
            .return_type = null,
            .type_defs = null,
            .prng = prng,
            .context = null,
            .assignments = std.StringHashMap(ast.Assignments).init(allocator),
            .type_params_context = &[_][]const u8{},
            .self_object = null,
        };
    }

    pub fn deinit(self: *Typechecker) void {
        self.env.deinit();
        self.arena_allocator.deinit();
        self.assignments.deinit();
    }

    fn replace(self: *Typechecker, type_: ast.Type, name: []const u8, replacement: ast.Type, context: ?[]const u8) anyerror!ast.Type {
        return switch (type_) {
            .ident => |ident| {
                if (std.mem.eql(u8, ident, name)) {
                    if (context) |ctx| {
                        const entry = try self.assignments.getOrPut(ctx);
                        if (!entry.found_existing) {
                            entry.key_ptr.* = ctx;
                            entry.value_ptr.* = std.StringHashMap(ast.Type).init(self.arena_allocator.allocator());
                        }
                        try entry.value_ptr.*.put(name, replacement);
                    }
                    return replacement;
                }

                return type_;
            },
            .struct_ => |data| {
                var fields = std.ArrayList(ast.StructField).init(self.arena_allocator.allocator());
                for (data) |field| {
                    try fields.append(ast.StructField{
                        .name = field.name,
                        .type_ = try self.replace(field.type_, name, replacement, context),
                    });
                }

                return ast.Type{ .struct_ = fields.items };
            },
            .unknown => type_,
            .bool_ => type_,
            .byte => type_,
            .int => type_,
            .fun => unreachable,
            .apply => |apply| {
                var params = std.ArrayList(ast.Type).init(self.arena_allocator.allocator());
                for (apply.params) |param| {
                    try params.append(try self.replace(param, name, replacement, context));
                }

                return ast.Type{ .apply = .{
                    .name = apply.name,
                    .params = params.items,
                } };
            },
            .ptr => |ptr| {
                const t = try self.arena_allocator.allocator().create(ast.Type);
                t.* = try self.replace(ptr.type_.*, name, replacement, context);

                return ast.Type{ .ptr = .{ .type_ = t } };
            },
            .type_ => {
                return type_;
            },
        };
    }

    fn replaceMany(self: *Typechecker, type_: ast.Type, names: [][]const u8, types: []ast.Type, context: ?[]const u8) anyerror!ast.Type {
        var t = type_;

        for (names, types) |name, replacement| {
            t = try self.replace(t, name, replacement, context);
        }

        return t;
    }

    fn applyTypeDef(self: *Typechecker, def: ast.TypeDef, args: []ast.Type) anyerror!ast.TypeDef {
        if (def.params.len != args.len) {
            std.log.err("Expected {d} arguments, got {d} ({s}:{d})", .{ def.params.len, args.len, @src().file, @src().line });
            return error.UnexpectedType;
        }

        var fields = std.ArrayList(ast.StructField).init(self.arena_allocator.allocator());
        for (def.fields) |field| {
            var t = field.type_;
            for (def.params, 0..) |param, i| {
                t = try self.replace(t, param, args[i], def.name);
            }

            try fields.append(ast.StructField{
                .name = field.name,
                .type_ = t,
            });
        }

        var extends = std.ArrayList(ast.ExtendField).init(self.arena_allocator.allocator());
        for (def.extends) |extend| {
            var type_ = extend.type_;
            type_ = try self.replaceMany(type_, def.params, args, def.name);
            try extends.append(ast.ExtendField{
                .name = extend.name,
                .type_ = type_,
            });
        }

        return ast.TypeDef{
            .name = def.name,
            .params = &[_][]const u8{},
            .fields = fields.items,
            .extends = extends.items,
        };
    }

    fn getValueType(self: *Typechecker, type_: ast.Type) anyerror!ast.Type {
        switch (type_) {
            .apply => |apply| {
                if (std.mem.eql(u8, apply.name, "array")) {
                    return apply.params[0];
                } else if (std.mem.eql(u8, apply.name, "slice")) {
                    var def = self.type_defs.?.get(apply.name).?;
                    def = try self.applyTypeDef(def, apply.params);

                    for (def.extends) |extend| {
                        if (std.mem.eql(u8, extend.name, "value")) {
                            return extend.type_;
                        }
                    }

                    unreachable;
                } else if (std.mem.eql(u8, apply.name, "vec")) {
                    return apply.params[0];
                } else if (std.mem.eql(u8, apply.name, "map")) {
                    return apply.params[1];
                } else {
                    std.log.err("Expected array-like data structure, got {any} ({s}:{})\n", .{ type_, @src().file, @src().line });
                    return error.UnexpectedType;
                }
            },
            .ptr => |ptr| {
                return ptr.type_.*;
            },
            else => {
                std.log.err("Expected array-like data structure, got {any} ({s}:{})\n", .{ type_, @src().file, @src().line });
                return error.UnexpectedType;
            },
        }
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
                            _ = try self.assertType(param.type_, actual.fun.params[i].type_);
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
                        if (expect_data.len != actual_data.len) {
                            std.log.err("Expected struct with {any} fields, got {any}\n", .{ expect_data.len, actual_data.len });
                            return TypecheckerError.UnexpectedType;
                        }
                        for (expect_data, 0..) |field, i| {
                            _ = self.assertType(field.type_, actual_data[i].type_) catch |err| {
                                std.log.err("Error in field {s}: {}\n   {s}:{}", .{ field.name, err, @src().file, @src().line });
                                return err;
                            };
                        }
                    },
                    .ident => |ident| {
                        const def = self.type_defs.?.get(ident).?;

                        if (expect_data.len != def.fields.len) {
                            std.log.err("Expected struct with {any} fields, got {any}\n", .{ def.fields.len, expect_data.len });
                            return TypecheckerError.UnexpectedType;
                        }

                        for (0..def.fields.len) |i| {
                            _ = try self.assertType(def.fields[i].type_, expect_data[i].type_);
                        }
                    },
                    else => {
                        std.log.err("Expected struct, got {any} ({s}:{d})", .{ actual, @src().file, @src().line });
                        return TypecheckerError.UnexpectedType;
                    },
                }
            },
            .ident => {
                switch (actual) {
                    .ident => {
                        if (!std.mem.eql(u8, expect.ident, actual.ident)) {
                            std.log.err("Expected ident {s}, got {s} ({s}:{d})", .{ expect.ident, actual.ident, @src().file, @src().line });
                            return TypecheckerError.UnexpectedType;
                        }
                    },
                    else => {
                        const expect_type = self.env.get(expect.ident) orelse {
                            std.log.err("Expected ident {s}, got {any} ({s}:{d})", .{ expect.ident, actual, @src().file, @src().line });
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
                                std.log.err("Expected {any} parameters, got {any} ({s}:{d})", .{ eapply.params.len, aapply.params.len, @src().file, @src().line });
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
                        std.log.err("Expected {any}, got {any} ({s}:{d})", .{ expect, actual, @src().file, @src().line });
                        return TypecheckerError.UnexpectedType;
                    },
                }
            },
            .type_ => {
                return actual;
            },
        }

        return expect;
    }

    fn resolveFunType(
        self: *Typechecker,
        name: []const u8,
        _fun_type: ast.FunType,
        args: []ast.Expression,
        label: *std.ArrayList(u8),
    ) anyerror!ast.Type {
        var assignments = ast.Assignments.init(self.arena_allocator.allocator());
        var fun_type = _fun_type;

        var typeArgsIndex: usize = 0;
        for (fun_type.params, 0..) |param, i| {
            switch (param.type_) {
                .type_ => {
                    typeArgsIndex = i + 1;
                },
                else => {
                    break;
                },
            }
        }

        var arg_types = std.ArrayList(ast.Type).init(self.arena_allocator.allocator());
        for (fun_type.params[0..typeArgsIndex], 0..) |param, i| {
            switch (args[i]) {
                .type_ => |t| {
                    const arg_type = try self.typecheckExpr(&args[i]);
                    _ = try self.assertType(param.type_, arg_type);

                    try arg_types.append(t);
                    try assignments.put(param.name, arg_type);
                },
                else => {
                    std.log.err("Expected type, got {any}\n", .{args[i]});
                    unreachable;
                },
            }
        }
        // FIXME: implement HM type inference
        for (fun_type.params[typeArgsIndex..], 0..) |param, i| {
            const arg_type = try self.typecheckExpr(&args[i + typeArgsIndex]);
            _ = try self.assertType(try param.type_.applyAssignments(self.arena_allocator.allocator(), assignments), arg_type);

            // Adhoc type inference
            switch (arg_type) {
                .apply => |apply_arg| {
                    switch (param.type_) {
                        .apply => |apply_param| {
                            if (std.mem.eql(u8, apply_arg.name, apply_param.name)) {
                                for (0..apply_arg.params.len) |j| {
                                    try assignments.put(apply_param.params[j].ident, apply_arg.params[j]);
                                }
                            }
                        },
                        else => {},
                    }
                },
                else => {},
            }
        }

        for (arg_types.items, 0..) |arg_type, i| {
            switch (arg_type) {
                .unknown => {
                    arg_types.items[i] = assignments.get(fun_type.params[i].name).?;
                },
                else => {},
            }
        }

        try label.appendSlice(name);

        return try fun_type.return_type.applyAssignments(self.arena_allocator.allocator(), assignments);
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
                var name: []const u8 = undefined;
                switch (call.callee.*) {
                    .var_ => |ident| {
                        name = ident;
                    },
                    .project => |project| {
                        name = project.rhs;
                    },
                    else => {
                        std.log.err("Expected ident, got {any}\n", .{call.callee});
                        return TypecheckerError.UnexpectedType;
                    },
                }

                const fun_type = try self.typecheckExpr(call.callee);

                switch (fun_type) {
                    .fun => |fun| {
                        var args_list = std.ArrayList(ast.Expression).init(self.arena_allocator.allocator());
                        defer args_list.deinit();

                        if (self.self_object) |obj| {
                            // Recover type applications here
                            if (obj.index != -1) {
                                for (fun.params) |param| {
                                    if (std.mem.eql(u8, param.name, "self")) {
                                        break;
                                    }

                                    try args_list.append(.{ .type_ = .{ .unknown = true } });
                                }
                            }

                            try args_list.append(obj.expr);

                            expr.call.type_ = obj.type_;
                        }

                        try args_list.appendSlice(call.args);

                        const args = args_list.items;

                        if (fun.params.len != args.len) {
                            std.log.err("Function {any} expects {any} arguments, got {any} ({any})\n", .{ call.callee, fun.params, args, expr });
                            return error.UnexpectedType;
                        }

                        var call_label = std.ArrayList(u8).init(self.arena_allocator.allocator());

                        const result_type = try self.resolveFunType(
                            name,
                            fun,
                            args,
                            &call_label,
                        );

                        expr.call.label_prefix = call_label.items;

                        // Recover args
                        for (0..call.args.len) |rev_i| {
                            call.args[call.args.len - rev_i - 1] = args[args.len - rev_i - 1];
                        }

                        return result_type;
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

                const key_type = try lhs_type.getIndexType(self.type_defs.?);
                const value_type = try self.getValueType(lhs_type);

                const rhs = index.rhs;
                const rhs_type = try self.typecheckExpr(rhs);

                _ = try self.assertType(key_type, rhs_type);

                expr.*.index.type_ = lhs_type;
                expr.*.index.elem_type = value_type;

                return value_type;
            },
            .new => |new| {
                var def: ast.TypeDef = undefined;
                switch (new.type_) {
                    .struct_ => |d| {
                        def = .{
                            .name = "",
                            .params = &[_][]const u8{},
                            .fields = d,
                            .extends = &[_]ast.ExtendField{},
                        };
                    },
                    .ident => |ident| {
                        def = self.type_defs.?.get(ident) orelse {
                            std.log.err("Struct not found: {s}\n", .{ident});
                            return error.VariableNotFound;
                        };
                    },
                    .apply => |apply| {
                        if (std.mem.eql(u8, apply.name, "map")) {
                            return new.type_;
                        }

                        const d: ast.TypeDef = self.type_defs.?.get(apply.name) orelse {
                            std.log.err("Function not found: {s}\n   {s}:{d}", .{ apply.name, @src().file, @src().line });
                            return error.VariableNotFound;
                        };

                        def = try self.applyTypeDef(d, apply.params);
                    },
                    else => {
                        std.log.err("Expected struct, got {any}\n", .{new.type_});
                        unreachable;
                    },
                }

                const entry = try self.assignments.getOrPut(def.name);
                if (!entry.found_existing) {
                    entry.key_ptr.* = def.name;
                    entry.value_ptr.* = std.StringHashMap(ast.Type).init(self.arena_allocator.allocator());
                }

                for (new.initializers, 0..) |initializer, k| {
                    const field_type = try def.getFieldType(initializer.field);

                    var value = initializer.value;
                    const t = try self.typecheckExpr(&value);

                    _ = self.assertType(field_type, t) catch |err| {
                        std.log.err("Error in initializer .{s} = {any} : {any}: {}\n   {s}:{}", .{ initializer.field, initializer.value, t, err, @src().file, @src().line });
                        return err;
                    };
                    expr.new.initializers[k].value = value;
                }

                return expr.new.type_;
            },
            .project => |project| {
                const lhs = project.lhs;
                var lhs_type = try self.typecheckExpr(lhs);

                var def: ast.TypeDef = undefined;
                switch (lhs_type) {
                    .ident => |ident| {
                        def = self.type_defs.?.get(ident) orelse {
                            std.log.err("Struct not found: {s}\n", .{ident});
                            return error.VariableNotFound;
                        };
                    },
                    .struct_ => |fields| {
                        for (fields, 0..) |field, i| {
                            if (std.mem.eql(u8, field.name, project.rhs)) {
                                expr.project.index = @intCast(i);
                                expr.project.result_type = field.type_;

                                return field.type_;
                            }
                        }

                        std.log.err("Field not found: {s}\n", .{project.rhs});
                        return TypecheckerError.UnexpectedType;
                    },
                    .apply => |apply| {
                        var d: ast.TypeDef = self.type_defs.?.get(apply.name) orelse {
                            std.log.err("Function not found: {s}\n   {s}:{d}", .{ apply.name, @src().file, @src().line });
                            return error.VariableNotFound;
                        };

                        d = try self.applyTypeDef(d, apply.params);

                        def = d;
                    },
                    .type_ => {
                        var d = self.type_defs.?.get(lhs.type_.apply.name) orelse {
                            std.log.err("Type not found: {s}\n", .{lhs.type_.apply.name});
                            return error.VariableNotFound;
                        };

                        d = try self.applyTypeDef(d, lhs.type_.apply.params);

                        def = d;
                        lhs_type = lhs.type_;
                    },
                    else => {
                        const j = try std.json.stringifyAlloc(self.arena_allocator.allocator(), lhs_type, .{});
                        std.log.err("Expected struct, got {s}\n", .{j});
                        unreachable;
                    },
                }

                const entry = try self.assignments.getOrPut(def.name);
                if (!entry.found_existing) {
                    entry.key_ptr.* = def.name;
                    entry.value_ptr.* = std.StringHashMap(ast.Type).init(self.arena_allocator.allocator());
                }

                const field = project.rhs;

                if (def.hasField(field)) {
                    expr.project.index = @intCast(try def.getFieldOffset(project.rhs));
                    expr.project.result_type = try def.getFieldType(project.rhs);

                    return try def.getFieldType(field);
                } else {
                    const method_type = self.env.get(field).?;

                    var self_index: i32 = -1; // if -1, then the method is static
                    for (method_type.fun.params, 0..) |param, i| {
                        if (std.mem.eql(u8, param.name, "self")) {
                            self_index = @intCast(i);
                            break;
                        }
                    }

                    self.self_object = .{ .index = self_index, .expr = lhs.*, .type_ = lhs_type };

                    return method_type;
                }

                std.log.err("Expected field or method, got {any}\n", .{field});
                return TypecheckerError.UnexpectedType;
            },
            .as => |as| {
                _ = try self.typecheckExpr(as.lhs);
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
            .type_ => {
                return ast.Type{ .type_ = true };
            },
            .sizeof => {
                return ast.Type{ .int = true };
            },
        }
    }

    fn typecheckStatement(self: *Typechecker, stmt: *ast.Statement) anyerror!void {
        switch (stmt.*) {
            .let => |let| {
                const t = self.typecheckExpr(&stmt.let.value) catch |err| {
                    std.log.err("Error in expression {any}: {}\n   {s}:{}", .{ stmt.let.value, err, @src().file, @src().line });
                    return err;
                };
                try self.env.put(let.name, t);
            },
            .return_ => {
                const t = try self.typecheckExpr(&stmt.return_);
                self.return_type.? = try self.assertType(self.return_type.?, t);
            },
            .expr => {
                _ = self.typecheckExpr(&stmt.expr) catch |err| {
                    std.log.err("Error in expression {any}: {}\n   {s}:{}", .{ stmt.expr, err, @src().file, @src().line });
                    return err;
                };
            },
            .if_ => {
                const cond_type = try self.typecheckExpr(stmt.if_.cond);
                _ = try self.assertType(ast.Type{ .bool_ = true }, cond_type);

                try self.typecheckBlock(&stmt.if_.then_);

                if (stmt.if_.else_) |_| {
                    try self.typecheckBlock(&stmt.if_.else_.?);
                }
            },
            .assign => {
                const lhs_type = self.typecheckExpr(&stmt.assign.lhs) catch |err| {
                    std.log.err("Error in expression {any}: {}\n   {s}:{}", .{ stmt.assign.lhs, err, @src().file, @src().line });
                    return err;
                };
                switch (lhs_type) {
                    .type_ => {
                        std.log.err("stmt: {any}, lhs_type: {any}\n", .{ stmt.*, lhs_type });
                        unreachable;
                    },
                    else => {},
                }

                const rhs_type = self.typecheckExpr(&stmt.assign.rhs) catch |err| {
                    std.log.err("Error in expression {any}: {}\n   {s}:{}", .{ stmt.assign.rhs, err, @src().file, @src().line });
                    return err;
                };

                _ = self.assertType(lhs_type, rhs_type) catch |err| {
                    std.log.err("Error in assignment {any} != {any}: {}\n   {s}:{}", .{ lhs_type, rhs_type, err, @src().file, @src().line });
                    return err;
                };

                stmt.*.assign.type_ = lhs_type;
            },
            .push => {
                const lhs_type = try self.typecheckExpr(&stmt.push.lhs);
                const value_type = try self.getValueType(lhs_type);

                const rhs_type = try self.typecheckExpr(&stmt.push.rhs);

                _ = try self.assertType(value_type, rhs_type);

                stmt.*.push.type_ = lhs_type;
            },
            .while_ => {
                const cond_type = try self.typecheckExpr(stmt.while_.cond);
                _ = try self.assertType(ast.Type{ .bool_ = true }, cond_type);

                try self.typecheckBlock(&stmt.while_.body);
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

            self.self_object = null;
        }

        if (block.expr) |expr| {
            _ = try self.typecheckExpr(expr);
        }
    }

    fn typecheckDecl(self: *Typechecker, module: *ast.Module, decl: *ast.Decl) anyerror!void {
        switch (decl.*) {
            .fun => |fun| {
                self.type_defs = module.type_defs;

                for (self.type_params_context) |param| {
                    try self.env.put(param, .{ .type_ = true });
                }
                for (fun.params) |param| {
                    try self.env.put(param.name, param.type_);
                }

                self.return_type = fun.result_type;

                const return_type = try self.arena_allocator.allocator().create(ast.Type);
                return_type.* = fun.result_type;

                if (self.env.contains(fun.name)) {
                    std.log.err("Function {s} already defined\n", .{fun.name});
                    unreachable;
                }

                var params = std.ArrayList(ast.FunParam).init(self.arena_allocator.allocator());
                for (self.type_params_context) |param| {
                    try params.append(.{
                        .name = param,
                        .type_ = .{ .type_ = true },
                    });
                }
                for (fun.params) |param| {
                    try params.append(.{
                        .name = param.name,
                        .type_ = param.type_,
                    });
                }

                try self.env.put(fun.name, ast.Type{ .fun = .{
                    .params = params.items,
                    .return_type = return_type,
                    .context = self.context,
                } });

                var body = fun.body;
                self.typecheckBlock(&body) catch |err| {
                    std.log.err("Error in function {s}: {}\n   {s}:{}", .{ fun.name, err, @src().file, @src().line });
                    return err;
                };
                decl.fun.body = body;

                return_type.* = self.return_type.?;

                const t = ast.Type{ .fun = .{
                    .params = params.items,
                    .return_type = return_type,
                    .context = self.context,
                } };
                try self.env.put(fun.name, t);

                self.return_type = null;
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
            .type_ => |td| {
                self.context = td.name;
                defer self.context = null;

                try self.env.put(td.name, .{ .ident = td.name });

                const def = ast.TypeDef{
                    .name = td.name,
                    .params = td.params,
                    .fields = td.fields,
                    .extends = td.extends,
                };
                try module.type_defs.put(td.name, def);

                self.type_params_context = td.params;

                for (0..td.methods.len) |i| {
                    try self.typecheckDecl(module, &decl.type_.methods[i]);
                }

                self.type_params_context = &[_][]const u8{};

                var methodTypes = std.ArrayList(ast.MethodField).init(self.arena_allocator.allocator());
                for (td.methods) |method| {
                    try methodTypes.append(.{
                        .name = method.fun.name,
                        .type_params = td.params,
                        .params = method.fun.params,
                        .result_type = method.fun.result_type,
                    });
                }

                try module.type_defs.put(td.name, def);
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
