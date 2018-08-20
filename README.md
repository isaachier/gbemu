# gbemu

Zig Game Boy emulator

## Why Zig?

Zig is a promising new language aimed at replacing C. Many Game Boy emulators
are written in C, C++, and Rust. While these languages definitely succeed in
emulating a Game Boy, I wanted to test out the power of Zig on a major project.

## Resources

There are many resources dedicated to Game Boy emulator development. A great
list can be found [here](https://github.com/avivace/awesome-gbdev).

## Assembler/Compiler

gbemu does not contain an assembler nor a compiler at this time. I highly
recommend using RGBDS for Game Boy development.

## Roadmap

- [x] CPU implementation
- [x] Disassembler
- [ ] Video emulation
- [ ] Audio emulation
