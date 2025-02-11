const std = @import("std");
const vaxis = @import("vaxis");
const vxfw = vaxis.vxfw;
const Cell = vaxis.Cell;
const TextInput = vaxis.widgets.TextInput;
const border = vaxis.widgets.border;

const Event = union(enum) {
    key_press: vaxis.Key,
    winsize: vaxis.Winsize,
    focus_in,
};

pub const Debugger = struct {
    allocator: std.mem.Allocator,
    tty: vaxis.Tty,
    vx: vaxis.Vaxis,
    loop: vaxis.Loop(Event),
    text_input: vaxis.widgets.TextInput,

    pub fn init(allocator: std.mem.Allocator) anyerror!Debugger {
        var tty = try vaxis.Tty.init();
        var vx = try vaxis.init(allocator, .{});

        var loop: vaxis.Loop(Event) = .{
            .tty = &tty,
            .vaxis = &vx,
        };
        try loop.init();

        const text_input = TextInput.init(allocator, &vx.unicode);

        return Debugger{
            .allocator = allocator,
            .tty = tty,
            .vx = vx,
            .loop = loop,
            .text_input = text_input,
        };
    }

    pub fn deinit(self: *Debugger) void {
        self.loop.stop();
        self.text_input.deinit();
        self.vx.deinit(self.allocator, self.tty.anyWriter());
        self.tty.deinit();
    }

    pub fn handle_event(self: *Debugger, event: Event) anyerror!bool {
        switch (event) {
            .key_press => |key| {
                if (key.matches('c', .{ .ctrl = true })) {
                    return false;
                } else if (key.matches('q', .{})) {
                    return false;
                }
            },

            .winsize => |ws| try self.vx.resize(self.allocator, self.tty.anyWriter(), ws),
            else => {},
        }

        return true;
    }

    pub fn draw(self: *Debugger) anyerror!void {
        const win = self.vx.window();
        win.clear();

        const child = win.child(.{
            .x_off = win.width / 2 - 20,
            .y_off = win.height / 2 - 3,
            .width = 40,
            .height = 3,
            .border = .{
                .where = .all,
            },
        });

        self.text_input.draw(child);

        try self.vx.render(self.tty.anyWriter());
    }

    pub fn start(self: *Debugger) anyerror!void {
        try self.loop.start();
        try self.vx.enterAltScreen(self.tty.anyWriter());
        try self.vx.queryTerminal(self.tty.anyWriter(), 1 * std.time.ns_per_s);
    }

    pub fn fetchEvent(self: *Debugger) !Event {
        return self.loop.nextEvent();
    }
};
