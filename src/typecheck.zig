const std = @import("std");

const ast = @import("ast.zig");

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
            .unknown => {
                return actual;
            },
        }

        return expect;
    }

    fn unwrapElementType(actual: ast.Type) anyerror!ast.Type {
        switch (actual) {
            .array => |array| {
                return array.elem_type.*;
            },
            .slice => |slice| {
                return slice.elem_type.*;
            },
            else => {
                std.log.err("Expected array, got {any}\n", .{actual});
                return TypecheckerError.UnexpectedType;
            },
        }
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

                        return ast.Type{ .slice = .{ .elem_type = elem } };
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
                            _ = try assertType(param, arg);
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

                const elem_type = try unwrapElementType(lhs_type);

                const rhs = index.rhs;
                const rhs_type = try self.typecheckExpr(rhs);

                _ = try assertType(ast.Type{ .int = true }, rhs_type);

                expr.*.index.elem_type = elem_type;

                return elem_type;
            },
            .new => |new| {
                for (new.initializers) |initializer| {
                    var inite = initializer;
                    const t = try self.typecheckExpr(&inite);

                    _ = try assertType(ast.Type{ .int = true }, t);
                }

                const elem = try self.arena_allocator.allocator().create(ast.Type);
                elem.* = ast.Type{ .int = true };

                return ast.Type{ .array = .{
                    .size = new.array_size,
                    .elem_type = elem,
                } };
            },
        }
    }

    fn typecheckStatement(self: *Typechecker, stmt: *ast.Statement) anyerror!void {
        switch (stmt.*) {
            .let => |let| {
                var value = let.value;

                const t = try self.typecheckExpr(&value);
                try self.env.put(let.name, t);
            },
            .return_ => |ret| {
                var e = ret;
                const t = try self.typecheckExpr(&e);

                self.return_type.? = try assertType(self.return_type.?, t);

                stmt.*.return_ = e;
            },
            .expr => |expr| {
                var e = expr;
                _ = try self.typecheckExpr(&e);
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
            try self.typecheckStatement(&s);

            block.statements[i] = s;
        }

        if (block.expr) |expr| {
            _ = try self.typecheckExpr(expr);
        }
    }

    pub fn typecheck(self: *Typechecker, module: *ast.Module) anyerror!void {
        for (module.decls, 0..) |decl, i| {
            switch (decl) {
                .fun => |fun| {
                    var params = std.ArrayList(ast.Type).init(self.arena_allocator.allocator());

                    for (fun.params) |param| {
                        // Assume all parameters are integers
                        try self.env.put(param, ast.Type{ .int = true });
                        try params.append(ast.Type{ .int = true });
                    }

                    self.return_type = ast.Type{ .unknown = true };

                    const return_type = try self.arena_allocator.allocator().create(ast.Type);
                    return_type.* = ast.Type{ .unknown = true };

                    if (self.env.contains(fun.name)) {
                        std.log.err("Function {s} already defined\n", .{fun.name});
                        unreachable;
                    }
                    try self.env.put(fun.name, ast.Type{ .fun = .{
                        .params = params.items,
                        .return_type = return_type,
                    } });

                    var body = fun.body;

                    try self.typecheckBlock(&body);

                    return_type.* = self.return_type.?;

                    const t = ast.Type{ .fun = .{
                        .params = params.items,
                        .return_type = return_type,
                    } };
                    try self.env.put(fun.name, t);

                    self.return_type = null;

                    module.decls[i].fun.body = body;
                },
            }
        }
    }
};
