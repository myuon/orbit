const std = @import("std");

const Allocator = std.heap.page_allocator;

const Lexeme = enum { Plus, Star, Number };
const Token = struct {
    lexeme: Lexeme,
    number: ?u64 = null,
};

const LexerError = error{
    Eos,
};

const Lexer = struct {
    source: []const u8,
    position: usize,

    fn peek(self: *Lexer) LexerError!u8 {
        if (self.position >= self.source.len) {
            return error.Eos;
        }

        return self.source[self.position];
    }

    fn consume(self: *Lexer) LexerError!u8 {
        const ch = try self.peek();
        self.position += 1;

        return ch;
    }

    fn consume_number(self: *Lexer) !u32 {
        var number: u32 = 0;
        while (true) {
            const ch = self.peek() catch |err| {
                if (err == error.Eos) {
                    break;
                }
                return err;
            };
            if (ch < '0' or ch > '9') {
                break;
            }

            number = number * 10 + @as(u32, (ch - @as(u8, '0')));
            _ = try self.consume();
        }

        return number;
    }

    fn run(self: *Lexer) !std.ArrayList(Token) {
        var tokens = std.ArrayList(Token).init(Allocator);

        while (self.position < self.source.len) {
            const c = self.source[self.position];
            switch (c) {
                ' ' => {
                    _ = try self.consume();
                },
                '+' => {
                    _ = try self.consume();
                    try tokens.append(Token{ .lexeme = Lexeme.Plus });
                },
                '*' => {
                    _ = try self.consume();
                    try tokens.append(Token{ .lexeme = Lexeme.Star });
                },
                else => {
                    const number = try self.consume_number();
                    try tokens.append(Token{ .lexeme = Lexeme.Number, .number = number });
                },
            }
        }

        return tokens;
    }
};

test "lexer" {
    var lexer = Lexer{ .source = "1 + 20 * 300", .position = 0 };
    const result = try lexer.run();

    var expected = std.ArrayList(Token).init(std.testing.allocator);
    defer expected.deinit();
    try expected.append(Token{ .lexeme = Lexeme.Number, .number = 1 });
    try expected.append(Token{ .lexeme = Lexeme.Plus });
    try expected.append(Token{ .lexeme = Lexeme.Number, .number = 20 });
    try expected.append(Token{ .lexeme = Lexeme.Star });
    try expected.append(Token{ .lexeme = Lexeme.Number, .number = 300 });

    try std.testing.expectEqualSlices(Token, expected.items, result.items);
}
