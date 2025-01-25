const std = @import("std");

pub const Operator = enum {
    eqeq,
    eq,
    semicolon,
    comma,
    lparen,
    rparen,
    plus,
    minus,
    star,
    let,
    do,
    end,
    fun,
    return_,
    if_,
    else_,
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
    call,
    if_,
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
    call: struct {
        name: []const u8,
        args: []Expression,
    },
    if_: struct {
        cond: *Expression,
        then_: Block,
        else_: ?Block,
    },
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

pub const DeclType = enum {
    fun,
};

pub const Decl = union(DeclType) {
    fun: struct {
        name: []const u8,
        params: []const []const u8,
        body: Block,
    },
};

pub const Module = struct {
    decls: []Decl,
};
