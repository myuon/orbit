const std = @import("std");

pub const Operator = enum {
    eqeq,
    lte,
    gte,
    eq,
    colon,
    semicolon,
    dot,
    comma,
    lparen,
    rparen,
    langle,
    rangle,
    lbracket,
    rbracket,
    lbrace,
    rbrace,
    plus,
    minus,
    star,
    percent,
    push,
    let,
    do,
    end,
    fun,
    return_,
    if_,
    else_,
    while_,
    true_,
    false_,
    new,
    struct_,
    as,
    type_,
    extends,
    sizeof,
};

pub const TokenType = enum {
    keyword,
    number,
    ident,
    string,
};

pub const Token = union(TokenType) {
    keyword: Operator,
    number: u32,
    ident: []const u8,
    string: []const u8,

    pub fn format(
        self: Token,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        switch (self) {
            Token.keyword => {
                try std.fmt.format(writer, "{any}", .{self.keyword});
            },
            Token.number => {
                try std.fmt.format(writer, "{d}", .{self.number});
            },
            Token.ident => {
                try std.fmt.format(writer, "{s}", .{self.ident});
            },
            Token.string => {
                try std.fmt.format(writer, "{s}", .{self.string});
            },
        }
    }
};

pub const StructInitializer = struct {
    field: []const u8,
    value: Expression,
};

pub const NewExpr = struct {
    type_: Type,
    initializers: []StructInitializer,
};

pub const CallExpr = struct {
    label_prefix: ?[]const u8,
    callee: *Expression,
    args: []Expression,
    type_: ?Type = null,

    pub fn getArgTypes(self: CallExpr, allocator: std.mem.Allocator) anyerror![]Type {
        var result = std.ArrayList(Type).init(allocator);

        for (self.args) |arg| {
            switch (arg) {
                .type_ => |t| {
                    try result.append(t);
                },
                else => {
                    break;
                },
            }
        }

        return result.items;
    }
};

pub const ExpressionType = enum {
    var_,
    literal,
    binop,
    block,
    call,
    if_,
    index,
    new,
    project,
    as,
    type_,
    sizeof,
};

pub const Expression = union(ExpressionType) {
    var_: []const u8,
    literal: Literal,
    binop: struct {
        op: Operator,
        lhs: *Expression,
        rhs: *Expression,
    },
    block: Block,
    call: CallExpr,
    if_: struct {
        cond: *Expression,
        then_: Block,
        else_: Block,
    },
    index: struct {
        type_: Type,
        elem_type: Type,
        lhs: *Expression,
        rhs: *Expression,
    },
    new: NewExpr,
    project: struct {
        index: i32,
        result_type: Type,
        lhs: *Expression,
        rhs: []const u8,
    },
    as: struct {
        lhs: *Expression,
        rhs: Type,
    },
    type_: Type,
    sizeof: Type,

    pub fn format(
        self: Expression,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        switch (self) {
            .var_ => |var_| {
                try std.fmt.format(writer, "{s}", .{var_});
            },
            .literal => |literal| {
                try std.fmt.format(writer, "{any}", .{literal});
            },
            .binop => |binop| {
                try std.fmt.format(writer, "({any} {any} {any})", .{ binop.lhs.*, binop.op, binop.rhs.* });
            },
            .block => |block| {
                try std.fmt.format(writer, "{{", .{});
                for (block.statements) |statement| {
                    try std.fmt.format(writer, "{any}", .{statement});
                }
                try std.fmt.format(writer, "}}", .{});
            },
            .call => |call| {
                try std.fmt.format(writer, "{any}(", .{call.callee.*});
                for (call.args, 0..) |arg, i| {
                    try std.fmt.format(writer, "{any}", .{arg});
                    if (i < call.args.len - 1) {
                        try std.fmt.format(writer, ", ", .{});
                    }
                }
                try std.fmt.format(writer, ")", .{});
            },
            .if_ => |if_| {
                try std.fmt.format(writer, "if {any} {any} {any}", .{ if_.cond.*, if_.then_, if_.else_ });
            },
            .index => |index| {
                try std.fmt.format(writer, "{any}[{any}]", .{ index.lhs.*, index.rhs.* });
            },
            .new => |new| {
                try std.fmt.format(writer, "new {any}", .{new.type_});
                try std.fmt.format(writer, "{{", .{});
                for (new.initializers, 0..) |init, i| {
                    try std.fmt.format(writer, ".{s} = {any}", .{ init.field, init.value });
                    if (i < new.initializers.len - 1) {
                        try std.fmt.format(writer, ", ", .{});
                    }
                }
                try std.fmt.format(writer, "}}", .{});
            },
            .project => |project| {
                try std.fmt.format(writer, "{any}.{s}", .{ project.lhs.*, project.rhs });
            },
            .as => |as| {
                try std.fmt.format(writer, "{any} as {any}", .{ as.lhs.*, as.rhs });
            },
            .type_ => |type_| {
                try std.fmt.format(writer, "type {any}", .{type_});
            },
            .sizeof => |sizeof| {
                try std.fmt.format(writer, "sizeof {any}", .{sizeof});
            },
        }
    }
};

pub const LiteralType = enum {
    boolean,
    number,
    string,
};

pub const Literal = union(LiteralType) {
    boolean: bool,
    number: u32,
    string: []const u8,

    pub fn format(
        self: Literal,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        switch (self) {
            .boolean => {
                try std.fmt.format(writer, "{any}", .{self.boolean});
            },
            .number => {
                try std.fmt.format(writer, "{d}", .{self.number});
            },
            .string => {
                try std.fmt.format(writer, "\"{s}\"", .{self.string});
            },
        }
    }
};

pub const StatementType = enum {
    let,
    return_,
    expr,
    if_,
    assign,
    push,
    while_,
};

pub const Statement = union(StatementType) {
    let: struct {
        name: []const u8,
        value: Expression,
    },
    return_: Expression,
    expr: Expression,
    if_: struct {
        cond: *Expression,
        then_: Block,
        else_: ?Block,
    },
    assign: struct {
        type_: Type,
        lhs: Expression,
        rhs: Expression,
    },
    push: struct {
        type_: Type,
        lhs: Expression,
        rhs: Expression,
    },
    while_: struct {
        cond: *Expression,
        body: Block,
    },

    pub fn format(
        self: Statement,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        switch (self) {
            .let => |let| {
                try std.fmt.format(writer, "let {s} = {any}; ", .{ let.name, let.value });
            },
            .return_ => |return_| {
                try std.fmt.format(writer, "return {any}; ", .{return_});
            },
            .expr => |expr| {
                try std.fmt.format(writer, "{any}; ", .{expr});
            },
            .if_ => |if_| {
                try std.fmt.format(writer, "if {any} do\n", .{if_.cond});
                try std.fmt.format(writer, "{any}", .{if_.then_});
                if (if_.else_) |block| {
                    try std.fmt.format(writer, "else\n{any}", .{block});
                }
                try std.fmt.format(writer, "end", .{});
            },
            .assign => |assign| {
                try std.fmt.format(writer, "{any} = {any}; ", .{ assign.lhs, assign.rhs });
            },
            .push => |push| {
                try std.fmt.format(writer, "{any} << {any}; ", .{ push.lhs, push.rhs });
            },
            .while_ => |while_| {
                try std.fmt.format(writer, "while {any} do\n", .{while_.cond});
                try std.fmt.format(writer, "{any}", .{while_.body});
                try std.fmt.format(writer, "end", .{});
            },
        }
    }
};

pub const Block = struct {
    statements: []Statement,
    expr: ?*Expression,

    pub fn format(
        self: Block,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        for (self.statements) |statement| {
            try std.fmt.format(writer, "  {any}\n", .{statement});
        }
    }
};

pub const DeclType = enum {
    fun,
    let,
    type_,
};

pub const FunParam = struct {
    name: []const u8,
    type_: Type,
};

pub const ExtendField = struct {
    name: []const u8,
    type_: Type,
};

pub const MethodField = struct {
    name: []const u8,
    type_params: [][]const u8,
    params: []FunParam,
    result_type: Type,

    pub fn format(
        self: MethodField,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        try std.fmt.format(writer, "MethodField {{", .{});
        try std.fmt.format(writer, "name: {s}, ", .{self.name});
        try std.fmt.format(writer, "type_params: {s}, ", .{self.type_params});
        try std.fmt.format(writer, "params: {{", .{});
        for (self.params) |param| {
            try std.fmt.format(writer, "{s}: {any}, ", .{ param.name, param.type_ });
        }
        try std.fmt.format(writer, "}}, ", .{});
        try std.fmt.format(writer, "result_type: {any}", .{self.result_type});
        try std.fmt.format(writer, "}}", .{});
    }
};

pub const Decl = union(DeclType) {
    fun: struct {
        name: []const u8,
        type_params: [][]const u8,
        params: []FunParam,
        result_type: Type,
        body: Block,
    },
    let: struct {
        name: []const u8,
        value: ?Expression,
    },
    type_: struct {
        name: []const u8,
        params: [][]const u8,
        methods: []Decl,
        fields: []StructField,
        extends: []ExtendField,
    },

    pub fn format(
        self: Decl,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        switch (self) {
            .fun => |fun| {
                try std.fmt.format(writer, "fun {s}(", .{fun.name});
                for (fun.params, 0..) |param, i| {
                    try std.fmt.format(writer, "{s}: {any}", .{ param.name, param.type_ });
                    if (i < fun.params.len - 1) {
                        try std.fmt.format(writer, ", ", .{});
                    }
                }
                try std.fmt.format(writer, "): {any} do\n", .{fun.result_type});
                try std.fmt.format(writer, "{any}", .{fun.body});
                try std.fmt.format(writer, "end", .{});
            },
            .let => |let| {
                try std.fmt.format(writer, "let {s} = {any}; ", .{ let.name, let.value });
            },
            .type_ => |type_| {
                try std.fmt.format(writer, "type {s}", .{type_.name});
                for (type_.params, 0..) |param, i| {
                    if (i == 0) {
                        try std.fmt.format(writer, "(", .{});
                    }
                    try std.fmt.format(writer, "{s}: type", .{param});
                    if (i < type_.params.len - 1) {
                        try std.fmt.format(writer, ", ", .{});
                    } else {
                        try std.fmt.format(writer, ")", .{});
                    }
                }
                try std.fmt.format(writer, " = struct {{\n", .{});
                for (type_.fields) |field| {
                    try std.fmt.format(writer, "{s}: {any},\n", .{ field.name, field.type_ });
                }
                for (type_.methods) |method| {
                    try std.fmt.format(writer, "{any}", .{method});
                }
                try std.fmt.format(writer, "\nextends: {{", .{});
                for (type_.extends) |extend| {
                    try std.fmt.format(writer, "{s}: {any},\n", .{ extend.name, extend.type_ });
                }
                try std.fmt.format(writer, "}}\n", .{});
                try std.fmt.format(writer, "}}", .{});
            },
        }
    }
};

pub const Module = struct {
    decls: []Decl,
    type_defs: TypeDefs,
    generic_calls: []GenericCallInfo,

    pub fn format(
        self: Module,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        for (self.decls) |decl| {
            try std.fmt.format(writer, "{any}\n\n", .{decl});
        }
    }
};

pub const AstTypeError = error{
    UnexpectedType,
};

pub const StructField = struct {
    name: []const u8,
    type_: Type,
};

pub const FunType = struct {
    params: []FunParam,
    return_type: *Type,
    context: ?[]const u8,

    pub fn format(
        self: FunType,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        try std.fmt.format(writer, "FunType{{ .params = {any}, .return_type = {any}, .context = {any} }}", .{ self.params, self.return_type.*, self.context });
    }
};

pub const TypeType = enum {
    unknown,
    bool_,
    byte,
    int,
    fun,
    struct_,
    ident,
    apply,
    ptr,
    type_,
};

pub const Type = union(TypeType) {
    unknown: bool,
    bool_: bool,
    byte: bool,
    int: bool,
    fun: FunType,
    struct_: []StructField,
    ident: []const u8,
    // Deprecated
    apply: struct {
        name: []const u8,
        params: []Type,
    },
    ptr: struct {
        type_: *Type,
    },
    type_: bool, // for type parameters

    pub fn format(
        self: Type,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        switch (self) {
            .unknown => try std.fmt.format(writer, "unknown", .{}),
            .bool_ => try std.fmt.format(writer, "bool", .{}),
            .byte => try std.fmt.format(writer, "byte", .{}),
            .int => try std.fmt.format(writer, "int", .{}),
            .fun => {
                try std.fmt.format(writer, "fun(", .{});
                for (self.fun.params, 0..) |param, i| {
                    try std.fmt.format(writer, "{s}: {any}", .{ param.name, param.type_ });
                    if (i < self.fun.params.len - 1) {
                        try std.fmt.format(writer, ", ", .{});
                    }
                }
                try std.fmt.format(writer, ")", .{});
                try std.fmt.format(writer, ": {any}", .{self.fun.return_type});
            },
            .struct_ => |fields| {
                try std.fmt.format(writer, "struct{{", .{});
                for (fields, 0..) |field, i| {
                    try std.fmt.format(writer, "{s}: {any}", .{ field.name, field.type_ });
                    if (i < fields.len - 1) {
                        try std.fmt.format(writer, ", ", .{});
                    }
                }
                try std.fmt.format(writer, "}}", .{});
            },
            .ident => try std.fmt.format(writer, "{s}", .{self.ident}),
            .apply => |apply| {
                try std.fmt.format(writer, "{s}(", .{apply.name});
                for (apply.params, 0..) |param, i| {
                    try std.fmt.format(writer, "{any}", .{param});
                    if (i < apply.params.len - 1) {
                        try std.fmt.format(writer, ", ", .{});
                    }
                }
                try std.fmt.format(writer, ")", .{});
            },
            .ptr => try std.fmt.format(writer, "[*]{s}", .{self.ptr.type_}),
            .type_ => try std.fmt.format(writer, "type", .{}),
        }
    }

    pub fn size(self: Type) u4 {
        return switch (self) {
            Type.unknown => unreachable,
            Type.bool_ => 1,
            Type.byte => 1,
            Type.int => 8,
            Type.fun => unreachable,
            Type.struct_ => 8,
            Type.ident => 8, // FIXME: should be unreachable
            Type.apply => unreachable,
            Type.ptr => 8,
            Type.type_ => unreachable,
        };
    }

    pub fn getIndexType(self: Type, defs: TypeDefs) anyerror!Type {
        switch (self) {
            .apply => |apply| {
                if (std.mem.eql(u8, apply.name, "array")) {
                    return Type{ .int = true };
                } else if (std.mem.eql(u8, apply.name, "slice")) {
                    // FIXME: type params for key
                    const def = defs.get(apply.name).?;
                    for (def.extends) |extend| {
                        if (std.mem.eql(u8, extend.name, "key")) {
                            return extend.type_;
                        }
                    }

                    unreachable;
                } else if (std.mem.eql(u8, apply.name, "vec")) {
                    return Type{ .int = true };
                } else if (std.mem.eql(u8, apply.name, "map")) {
                    return apply.params[0];
                } else {
                    std.log.err("Expected array-like data structure, got {any} ({s}:{})\n", .{ self, @src().file, @src().line });
                    return error.UnexpectedType;
                }
            },
            .ptr => {
                return .{ .int = true };
            },
            else => {
                std.log.err("Expected array-like data structure, got {any} ({s}:{})\n", .{ self, @src().file, @src().line });
                return error.UnexpectedType;
            },
        }
    }

    pub fn getValueType(self: Type, defs: TypeDefs, _: std.mem.Allocator) anyerror!Type {
        switch (self) {
            .apply => |apply| {
                if (std.mem.eql(u8, apply.name, "array")) {
                    return apply.params[0];
                } else if (std.mem.eql(u8, apply.name, "slice")) {
                    const def = defs.get(apply.name).?;
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
                    std.log.err("Expected array-like data structure, got {any} ({s}:{})\n", .{ self, @src().file, @src().line });
                    return error.UnexpectedType;
                }
            },
            .ptr => |ptr| {
                return ptr.type_.*;
            },
            else => {
                std.log.err("Expected array-like data structure, got {any} ({s}:{})\n", .{ self, @src().file, @src().line });
                return error.UnexpectedType;
            },
        }
    }

    fn replace(self: Type, allocator: std.mem.Allocator, name: []const u8, type_: Type) anyerror!Type {
        return switch (self) {
            .ident => |ident| {
                if (std.mem.eql(u8, ident, name)) {
                    return type_;
                }

                return self;
            },
            .struct_ => |data| {
                var fields = std.ArrayList(StructField).init(allocator);
                for (data) |field| {
                    try fields.append(StructField{
                        .name = field.name,
                        .type_ = try field.type_.replace(allocator, name, type_),
                    });
                }

                return Type{ .struct_ = fields.items };
            },
            .unknown => self,
            .bool_ => self,
            .byte => self,
            .int => self,
            .fun => unreachable,
            .apply => |apply| {
                var params = std.ArrayList(Type).init(allocator);
                for (apply.params) |param| {
                    try params.append(try param.replace(allocator, name, type_));
                }

                return Type{ .apply = .{
                    .name = apply.name,
                    .params = params.items,
                } };
            },
            .ptr => |ptr| {
                const t = try allocator.create(Type);
                t.* = try ptr.type_.replace(allocator, name, type_);

                return Type{ .ptr = .{ .type_ = t } };
            },
            .type_ => unreachable,
        };
    }

    fn replaceMany(self: Type, allocator: std.mem.Allocator, names: [][]const u8, types: []Type) anyerror!Type {
        var t = self;

        for (names, types) |name, type_| {
            t = try t.replace(allocator, name, type_);
        }

        return t;
    }

    pub fn applyAssignments(self: Type, allocator: std.mem.Allocator, assignments: Assignments) anyerror!Type {
        switch (self) {
            .ident => |ident| {
                if (assignments.get(ident)) |type_| {
                    return type_;
                }

                return self;
            },
            .int => return self,
            .bool_ => return self,
            .byte => return self,
            .struct_ => |fields| {
                var new_fields = std.ArrayList(StructField).init(allocator);
                for (fields) |field| {
                    try new_fields.append(StructField{ .name = field.name, .type_ = try field.type_.applyAssignments(allocator, assignments) });
                }
                return Type{ .struct_ = new_fields.items };
            },
            .ptr => |ptr| {
                const t = try allocator.create(Type);
                t.* = try ptr.type_.*.applyAssignments(allocator, assignments);

                return Type{ .ptr = .{ .type_ = t } };
            },
            .unknown => return self, // FIXME: should be unreachable
            .apply => |apply| {
                var params = std.ArrayList(Type).init(allocator);
                for (apply.params) |param| {
                    try params.append(try param.applyAssignments(allocator, assignments));
                }

                return Type{ .apply = .{ .name = apply.name, .params = params.items } };
            },
            .type_ => return self,
            else => {
                std.log.err("Unexpected type, got {any} ({s}:{})\n", .{ self, @src().file, @src().line });
                unreachable;
            },
        }
    }
};

pub const Assignments = std.StringHashMap(Type);

pub const TypeParam = struct {
    name: []const u8,
    type_: Type,
};

pub const GenericCallInfo = struct {
    name: []const u8,
    types: []Type,

    pub fn format(
        self: GenericCallInfo,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        try std.fmt.format(writer, "GenericCallInfo{{ .name = {s}, .types = {any} }}", .{ self.name, self.types });
    }
};

pub const TypeDef = struct {
    name: []const u8,
    params: [][]const u8,
    fields: []StructField,
    methods: []MethodField,
    extends: []ExtendField,
    assignments: Assignments,

    pub fn hasField(self: TypeDef, name: []const u8) bool {
        for (self.fields) |field| {
            if (std.mem.eql(u8, field.name, name)) {
                return true;
            }
        }

        return false;
    }

    pub fn getFieldType(self: TypeDef, name: []const u8) !Type {
        for (self.fields) |field| {
            if (std.mem.eql(u8, field.name, name)) {
                return field.type_;
            }
        }

        unreachable;
    }

    pub fn getFieldOffset(self: TypeDef, name: []const u8) !usize {
        for (self.fields, 0..) |field, i| {
            if (std.mem.eql(u8, field.name, name)) {
                return i;
            }
        }

        std.log.err("Field not found: {s} in fields: {any}, methods: {any}\n", .{ name, self.fields.len, self.methods.len });
        unreachable;
    }

    pub fn hasMethod(self: TypeDef, name: []const u8) bool {
        for (self.methods) |method| {
            if (std.mem.eql(u8, method.name, name)) {
                return true;
            }
        }

        return false;
    }

    pub fn getMethod(self: TypeDef, name: []const u8) MethodField {
        for (self.methods) |method| {
            if (std.mem.eql(u8, method.name, name)) {
                return method;
            }
        }

        unreachable;
    }

    pub fn apply(self: TypeDef, allocator: std.mem.Allocator, args: []Type) anyerror!TypeDef {
        if (self.params.len != args.len) {
            std.log.err("Expected {d} arguments, got {d} ({s}:{d})", .{ self.params.len, args.len, @src().file, @src().line });
            return error.UnexpectedType;
        }

        var fields = std.ArrayList(StructField).init(allocator);
        for (self.fields) |field| {
            var t = field.type_;
            for (self.params, 0..) |param, i| {
                t = try t.replace(allocator, param, args[i]);
            }

            try fields.append(StructField{
                .name = field.name,
                .type_ = t,
            });
        }

        var methods = std.ArrayList(MethodField).init(allocator);
        for (self.methods) |method| {
            var params = std.ArrayList(FunParam).init(allocator);
            for (method.params) |param| {
                var param_type = param.type_;
                param_type = try param_type.replaceMany(allocator, self.params, args);

                try params.append(.{
                    .name = param.name,
                    .type_ = param_type,
                });
            }

            var result_type = method.result_type;
            result_type = try result_type.replaceMany(allocator, self.params, args);

            std.debug.assert(method.type_params.len == args.len);

            try methods.append(MethodField{
                .name = method.name,
                .type_params = method.type_params,
                .params = params.items,
                .result_type = result_type,
            });
        }

        var extends = std.ArrayList(ExtendField).init(allocator);
        for (self.extends) |extend| {
            var type_ = extend.type_;
            type_ = try type_.replaceMany(allocator, self.params, args);

            try extends.append(ExtendField{
                .name = extend.name,
                .type_ = type_,
            });
        }

        return TypeDef{
            .name = self.name,
            .params = &[_][]const u8{},
            .fields = fields.items,
            .methods = methods.items,
            .extends = extends.items,
            .assignments = self.assignments,
        };
    }

    fn replace(self: TypeDef, name: []const u8, arg: Type) anyerror!TypeDef {
        switch (self) {
            .name => |n| {
                if (std.mem.eql(u8, n, name)) {
                    self.assignments.put(name, arg);

                    return arg;
                }
            },
            else => {
                std.log.err("Cannot replace {any} in {any}\n", .{ name, self });
                return error.UnexpectedType;
            },
        }
    }
};
pub const TypeDefs = std.StringHashMap(TypeDef);

pub const ValueError = error{
    UnexpectedType,
};

pub const ValueType = enum {
    nil,
    i64_,
    bool_,
};

pub const Value = union(ValueType) {
    nil: bool,
    i64_: i64,
    bool_: bool,

    pub fn asI64(self: Value) ValueError!i64 {
        return switch (self) {
            Value.i64_ => self.i64_,
            else => error.UnexpectedType,
        };
    }

    pub fn asBool(self: Value) ValueError!bool {
        return switch (self) {
            Value.bool_ => self.bool_,
            else => error.UnexpectedType,
        };
    }
};

pub const InstructionType = enum {
    push,
    pop,
    ret,
    eq,
    jump,
    jump_ifzero,
    add,
    add_di,
    madd_d,
    sub,
    mul,
    div,
    call,
    call_d,
    call_vtable,
    label,
    get_pc,
    get_bp,
    set_bp,
    get_sp,
    set_sp,
    jump_d,
    jump_ifzero_d,
    get_local_d,
    set_local_d,
    nop,
    mod,
    lt,
    lt_d,
    lte,
    gt,
    gte,
    load,
    store,
    set_memory,
    set_cip,
    table_set,
    table_get,
};

pub const Instruction = union(InstructionType) {
    push: i32,
    pop: bool,
    ret: bool,
    eq: bool,
    jump: []const u8, // (resolve_label phase) replaced with jump_d
    jump_ifzero: []const u8, // (resolve_label phase) replaced with jump_ifzero_d
    add: bool,
    add_di: struct {
        lhs: i32,
        imm: i32,
        target: ?i32,
    },
    madd_d: struct {
        lhs: i32,
        rhs: i32,
        base: i32,
        target: ?i32,
    },
    sub: bool,
    mul: bool,
    div: bool,
    call: []const u8, // (resolve_label phase) replaced with call_d
    call_d: usize,
    call_vtable: usize, // For JIT
    label: []const u8, // (resolve_label phase) replaced with nop
    get_pc: bool,
    get_bp: bool,
    set_bp: bool,
    get_sp: bool,
    set_sp: bool,
    jump_d: usize,
    jump_ifzero_d: usize,
    get_local_d: i32,
    set_local_d: i32,
    nop: bool,
    mod: bool,
    lt: bool,
    lt_d: struct {
        lhs: i32,
        rhs: i32,
    },
    lte: bool,
    gt: bool,
    gte: bool,
    load: u4,
    store: u4,
    set_memory: struct {
        data: []const u8,
    },
    set_cip: usize, // For tracing JIT
    table_set: bool, // For hashmap
    table_get: bool, // For hashmap

    pub fn format(
        self: Instruction,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        switch (self) {
            Instruction.push => {
                try std.fmt.format(writer, "push #{d}", .{self.push});
            },
            Instruction.pop => {
                try std.fmt.format(writer, "pop", .{});
            },
            Instruction.ret => {
                try std.fmt.format(writer, "ret", .{});
            },
            Instruction.eq => {
                try std.fmt.format(writer, "eq", .{});
            },
            Instruction.jump => {
                try std.fmt.format(writer, "jump {s}", .{self.jump});
            },
            Instruction.jump_ifzero => {
                try std.fmt.format(writer, "jump_ifzero {s}", .{self.jump_ifzero});
            },
            Instruction.add => {
                try std.fmt.format(writer, "add", .{});
            },
            Instruction.sub => {
                try std.fmt.format(writer, "sub", .{});
            },
            Instruction.mul => {
                try std.fmt.format(writer, "mul", .{});
            },
            Instruction.div => {
                try std.fmt.format(writer, "div", .{});
            },
            Instruction.call => {
                try std.fmt.format(writer, "call {s}", .{self.call});
            },
            Instruction.call_d => {
                try std.fmt.format(writer, "call_d #{d}", .{self.call_d});
            },
            Instruction.label => {
                try std.fmt.format(writer, "label {s}", .{self.label});
            },
            Instruction.get_pc => {
                try std.fmt.format(writer, "get_pc", .{});
            },
            Instruction.get_bp => {
                try std.fmt.format(writer, "get_bp", .{});
            },
            Instruction.set_bp => {
                try std.fmt.format(writer, "set_bp", .{});
            },
            Instruction.get_sp => {
                try std.fmt.format(writer, "get_sp", .{});
            },
            Instruction.set_sp => {
                try std.fmt.format(writer, "set_sp", .{});
            },
            Instruction.jump_d => {
                try std.fmt.format(writer, "jump_d #{d}", .{self.jump_d});
            },
            Instruction.jump_ifzero_d => {
                try std.fmt.format(writer, "jump_ifzero_d #{d}", .{self.jump_ifzero_d});
            },
            Instruction.get_local_d => {
                try std.fmt.format(writer, "get_local_d [{d}]", .{self.get_local_d});
            },
            Instruction.set_local_d => {
                try std.fmt.format(writer, "set_local_d [{d}]", .{self.set_local_d});
            },
            Instruction.nop => {
                try std.fmt.format(writer, "nop", .{});
            },
            Instruction.mod => {
                try std.fmt.format(writer, "mod", .{});
            },
            Instruction.lt => {
                try std.fmt.format(writer, "lt", .{});
            },
            Instruction.lte => {
                try std.fmt.format(writer, "lte", .{});
            },
            Instruction.gt => {
                try std.fmt.format(writer, "gt", .{});
            },
            Instruction.gte => {
                try std.fmt.format(writer, "gte", .{});
            },
            Instruction.load => {
                try std.fmt.format(writer, "load_{d}", .{self.load});
            },
            Instruction.store => {
                try std.fmt.format(writer, "store_{d}", .{self.store});
            },
            Instruction.set_memory => {
                try std.fmt.format(writer, "set_memory #{s}", .{self.set_memory.data});
            },
            Instruction.set_cip => {
                try std.fmt.format(writer, "set_cip #{d}", .{self.set_cip});
            },
            Instruction.lt_d => {
                try std.fmt.format(writer, "lt_d [{d}] [{d}]", .{ self.lt_d.lhs, self.lt_d.rhs });
            },
            Instruction.add_di => {
                if (self.add_di.target) |t| {
                    try std.fmt.format(writer, "add_di [{d}] #{d} -> [{d}]", .{ self.add_di.lhs, self.add_di.imm, t });
                } else {
                    try std.fmt.format(writer, "add_di [{d}] #{d}", .{ self.add_di.lhs, self.add_di.imm });
                }
            },
            Instruction.madd_d => {
                if (self.madd_d.target) |t| {
                    try std.fmt.format(writer, "madd_d [{d}] [{d}] [{d}] -> [{d}]", .{ self.madd_d.lhs, self.madd_d.rhs, self.madd_d.base, t });
                } else {
                    try std.fmt.format(writer, "madd_d [{d}] [{d}] [{d}]", .{ self.madd_d.lhs, self.madd_d.rhs, self.madd_d.base });
                }
            },
            Instruction.table_set => {
                try std.fmt.format(writer, "table_set", .{});
            },
            Instruction.table_get => {
                try std.fmt.format(writer, "table_get", .{});
            },
            Instruction.call_vtable => {
                try std.fmt.format(writer, "call_vtable #{d}", .{self.call_vtable});
            },
        }
    }

    pub fn is_get_local_d(self: Instruction) bool {
        return switch (self) {
            Instruction.get_local_d => true,
            else => false,
        };
    }

    pub fn is_set_local_d(self: Instruction) bool {
        return switch (self) {
            Instruction.set_local_d => true,
            else => false,
        };
    }

    pub fn is_push(self: Instruction) bool {
        return switch (self) {
            Instruction.push => true,
            else => false,
        };
    }

    pub fn is_mul(self: Instruction) bool {
        return switch (self) {
            Instruction.mul => true,
            else => false,
        };
    }

    pub fn is_add(self: Instruction) bool {
        return switch (self) {
            Instruction.add => true,
            else => false,
        };
    }

    pub fn is_add_di(self: Instruction) bool {
        return switch (self) {
            Instruction.add_di => true,
            else => false,
        };
    }

    pub fn is_madd_d(self: Instruction) bool {
        return switch (self) {
            Instruction.madd_d => true,
            else => false,
        };
    }
};
