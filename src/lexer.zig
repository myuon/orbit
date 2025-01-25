const std = @import("std");
const utils = @import("utils.zig");
const ast = @import("ast.zig");

const Allocator = std.heap.page_allocator;

pub const LexerError = error{
    UnexpectedToken,
};

pub const Lexer = struct {
    source: []const u8,
    position: usize,

    fn peek(self: *Lexer) ?u8 {
        if (self.position >= self.source.len) {
            return null;
        }

        return self.source[self.position];
    }

    fn consume(self: *Lexer) ?u8 {
        if (self.peek()) |ch| {
            self.position += 1;
            return ch;
        } else {
            return null;
        }
    }

    fn consume_number(self: *Lexer) ?u32 {
        var happened = false;
        var number: u32 = 0;
        while (self.peek()) |ch| {
            if (ch < '0' or ch > '9') {
                break;
            }

            number = number * 10 + @as(u32, (ch - @as(u8, '0')));
            _ = self.consume();
            happened = true;
        }

        if (!happened) {
            return null;
        }
        return number;
    }

    fn consume_spaces(self: *Lexer) usize {
        var n: usize = 0;

        while (self.peek()) |ch| {
            if (ch == ' ' or ch == '\n' or ch == '\t') {
                _ = self.consume();
                n += 1;
            } else {
                break;
            }
        }

        return n;
    }

    fn consume_token(self: *Lexer) ?ast.Token {
        const tokenTable = comptime [_]struct { str: []const u8, operator: ast.Operator }{
            .{ .str = "+", .operator = ast.Operator.plus },
            .{ .str = "-", .operator = ast.Operator.minus },
            .{ .str = "*", .operator = ast.Operator.star },
            .{ .str = ";", .operator = ast.Operator.semicolon },
            .{ .str = ",", .operator = ast.Operator.comma },
            .{ .str = "==", .operator = ast.Operator.eqeq },
            .{ .str = "=", .operator = ast.Operator.eq },
            .{ .str = "(", .operator = ast.Operator.lparen },
            .{ .str = ")", .operator = ast.Operator.rparen },
            .{ .str = "let", .operator = ast.Operator.let },
            .{ .str = "do", .operator = ast.Operator.do },
            .{ .str = "end", .operator = ast.Operator.end },
            .{ .str = "fun", .operator = ast.Operator.fun },
            .{ .str = "return", .operator = ast.Operator.return_ },
            .{ .str = "if", .operator = ast.Operator.if_ },
            .{ .str = "else", .operator = ast.Operator.else_ },
        };

        for (tokenTable) |token| {
            if (utils.startsWith(self.source[self.position..], token.str)) {
                self.position += token.str.len;
                return ast.Token{ .keyword = token.operator };
            }
        }

        return null;
    }

    fn consume_ident(self: *Lexer) ?[]const u8 {
        const start = self.position;
        while (self.peek()) |ch| {
            if ((ch < 'a' or ch > 'z') and ch != '_' and (ch < 'A' or ch > 'Z') and (ch < '0' or ch > '9')) {
                break;
            }

            _ = self.consume();
        }

        if (start == self.position) {
            return null;
        }
        return self.source[start..self.position];
    }

    pub fn run(self: *Lexer) anyerror!std.ArrayList(ast.Token) {
        var tokens = std.ArrayList(ast.Token).init(Allocator);

        while (self.position < self.source.len) {
            if (self.consume_spaces() > 0) {
                continue;
            }

            if (self.consume_token()) |token| {
                try tokens.append(token);
                continue;
            }

            if (self.consume_number()) |number| {
                try tokens.append(ast.Token{ .number = number });
                continue;
            }

            if (self.consume_ident()) |ident| {
                try tokens.append(ast.Token{ .ident = ident });
                continue;
            }

            std.debug.print("unexpected token: {s}\n", .{self.source[self.position..]});
            return error.UnexpectedToken;
        }

        return tokens;
    }
};

test "lexer" {
    var lexer = Lexer{ .source = "1 + 20 * 300", .position = 0 };
    const result = try lexer.run();

    var expected = std.ArrayList(ast.Token).init(std.testing.allocator);
    defer expected.deinit();
    try expected.append(ast.Token{ .number = 1 });
    try expected.append(ast.Token{ .keyword = ast.Operator.plus });
    try expected.append(ast.Token{ .number = 20 });
    try expected.append(ast.Token{ .keyword = ast.Operator.star });
    try expected.append(ast.Token{ .number = 300 });

    try std.testing.expectEqualSlices(ast.Token, expected.items, result.items);
}
