const std = @import("std");
const vaxis = @import("vaxis");
const vxfw = vaxis.vxfw;
const Cell = vaxis.Cell;
const TextInput = vaxis.widgets.TextInput;
const border = vaxis.widgets.border;

const TuiEvent = union(enum) {
    key_press: vaxis.Key,
    winsize: vaxis.Winsize,
    focus_in,
};

pub const Tui = struct {
    allocator: std.mem.Allocator,
    tty: vaxis.Tty,
    vx: vaxis.Vaxis,
    loop: vaxis.Loop(TuiEvent),
    view: vaxis.widgets.View,
    text: []u8 = "",

    pub fn init(allocator: std.mem.Allocator) anyerror!Tui {
        var tty = try vaxis.Tty.init();
        var vx = try vaxis.init(allocator, .{});

        var loop: vaxis.Loop(TuiEvent) = .{
            .tty = &tty,
            .vaxis = &vx,
        };
        try loop.init();

        const view = try vaxis.widgets.View.init(
            allocator,
            &vx.unicode,
            .{ .width = 80, .height = 24 },
        );

        return Tui{
            .allocator = allocator,
            .tty = tty,
            .vx = vx,
            .loop = loop,
            .view = view,
        };
    }

    pub fn deinit(self: *Tui) void {
        self.loop.stop();
        self.view.deinit();
        self.vx.deinit(self.allocator, self.tty.anyWriter());
        self.tty.deinit();
    }

    pub fn recover_panic() void {
        vaxis.panic_handler("Panic Recovered", null, null);
    }

    pub fn handle_event(self: *Tui, event: TuiEvent) anyerror!bool {
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

    pub fn draw(self: *Tui) anyerror!void {
        const win = self.vx.window();
        win.clear();

        const child = win.child(.{
            .x_off = @intCast(win.width / 2 - 20),
            .y_off = @intCast(win.height / 2 - 3),
            .width = 80,
            .height = 10,
            .border = .{
                .where = .all,
            },
        });

        _ = child.printSegment(.{ .text = self.text }, .{});

        try self.vx.render(self.tty.anyWriter());
    }

    pub fn start(self: *Tui) anyerror!void {
        try self.loop.start();
        try self.vx.enterAltScreen(self.tty.anyWriter());
        try self.vx.queryTerminal(self.tty.anyWriter(), 1 * std.time.ns_per_s);
    }

    pub fn fetchEvent(self: *Tui) !TuiEvent {
        return self.loop.nextEvent();
    }

    pub fn set_text(self: *Tui, text: []u8) void {
        self.text = text;
    }
};
