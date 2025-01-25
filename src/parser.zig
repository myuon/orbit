const std = @import("std");
const ast = @import("ast.zig");

const Parser = struct {
    tokens: []ast.Token,
    position: usize,
};
