const std = @import("std");

pub const Operator = enum {
    plus,
    star,
    eq,
    semicolon,
    let,
    do,
    end,
};

pub const TokenType = enum {
    keyword,
    number,
    ident,
};

pub const Token = union(TokenType) {
    keyword: Operator,
    number: u32,
    ident: []const u8,
};

pub const ExpressionType = enum {
    var_,
    literal,
    binop,
    block,
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
};

pub const StatementType = enum {
    let,
    return_,
    expr,
};

pub const Statement = union(StatementType) {
    let: struct {
        name: []const u8,
        value: Expression,
    },
    return_: Expression,
    expr: Expression,
};

pub const Block = struct {
    statements: []Statement,
    expr: ?*Expression,
};
