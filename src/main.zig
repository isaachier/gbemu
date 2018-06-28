const c = @import("c.zig");

const ErrorSet = error{SDLError};

pub fn main() !void {
    if (c.SDL_Init(c.SDL_INIT_VIDEO | c.SDL_INIT_AUDIO) != 0) {
        c.SDL_Log(c"Unable to initialize SDL: %s", c.SDL_GetError());
        return ErrorSet.SDLError;
    }

    const screen_width = 640;
    const screen_height = 480;
    const undefined_pos = 0x1FFF0000;

    const window = c.SDL_CreateWindow(
        c"gbemu",
        undefined_pos,
        undefined_pos,
        screen_width,
        screen_height,
        c.SDL_WINDOW_SHOWN,
    );
    const screen = c.SDL_GetWindowSurface(window);
    c.SDL_Delay(2000);
    c.SDL_DestroyWindow(window);
    c.SDL_Quit();
}
