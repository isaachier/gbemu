const std = @import("std");

const c = @import("c.zig");

const ErrorSet = error{SDLError};

pub fn main() !void {
    if (c.SDL_Init(c.SDL_INIT_VIDEO | c.SDL_INIT_AUDIO) != 0) {
        std.debug.warn("Unable to initialize SDL: {s}", c.SDL_GetError());
        return ErrorSet.SDLError;
    }
    defer c.SDL_Quit();

    const screen_width = 160;
    const screen_height = 144;
    const undefined_pos = 0x1FFF0000;

    const window = c.SDL_CreateWindow(
        c"gbemu",
        undefined_pos,
        undefined_pos,
        screen_width,
        screen_height,
        c.SDL_WINDOW_SHOWN,
    );
    if (window == null) {
        std.debug.warn("Cannot create window: {s}", c.SDL_GetError());
        return ErrorSet.SDLError;
    }
    defer c.SDL_DestroyWindow(window);
    const screen = c.SDL_GetWindowSurface(window);
    std.os.nanosleep(5, 0);
}
