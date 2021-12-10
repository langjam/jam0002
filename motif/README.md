Motif Lang
==========

Motif is an esoteric, symbol-agnostic programming language based on combination of line patterns.

Building & Running
------------------
1. Install node & npm
2. Run `npm install` to install dependencies
3. Run `npm run build` to build the source code
4. Run `./motif [filename]` or `npm start [filename]` to run a motif program.
   ```bash
   ./motif examples/hello_world.txt
   ./motif examples/fibonacci.png
   ```

Patterns
---------------
Motif is designed to be symbol-agnostic as much as possible. Instead relying on symbols and keywords,
all instructions in motif is composed of character or pixel pattern. There are 6 patterns and 1 special patterns in motif:

1. Rainbow: 1-2-3
   A symbol (character or pixel color) followed by itself or other symbols without any repetition, and the length of symbol streak is the same for every symbol.
   Example:

   `abc` is a rainbow pattern with color ["a", "b", "c"], each has width of 1

   `eeeeefffffggggg` is a rainbow pattern with color ["e", "f", "g"], each has width of 5

   ![image](./docs/rainbow.png) is a rainbow pattern with color [#FF0000, #00FF00, #0000FF], each has width of 4

2.
