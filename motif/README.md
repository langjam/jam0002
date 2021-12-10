Motif Lang
==========

Motif is an esoteric, symbol-agnostic programming language based on combination of line patterns.

Building
--------
1. Install nodejs & npm
2. Run `npm install` to install the dependencies
3. Run `npm run build` to build the source code

Running
-------
To start a motif program, run `./motif [filename]` or `npm start [filename]`. Motif program can be a png file or a text file.
```bash
./motif examples/hello_world.txt
./motif examples/factorial.png
npm start examples/fibonacci
```

How Motif Works
===============

Patterns
--------
Motif is designed to be symbol-agnostic as much as possible. Instead relying on symbols and keywords,
all instructions in motif is composed of character or pixel pattern. There are 6 patterns and 1 special patterns in motif:

0. Solid (Special Pattern)

   Solid pattern is composed of cells of one symbol (a UTF-8 character or a pixel's 32-bit RGBA color). Solid pattern is not used for instruction, but for section marker. Examples:

   `a` is a solid pattern of color "a"

   `eeeeeeeee` is a solid pattern of color "e"

   A 12-pixel row ![image](./docs/solid.png) is a solid pattern of color #FF0000


1. Rainbow

   Rainbow pattern is composed of cells of a symbol followed by cells of other symbols without any repetition, and the length of cell streak is the same for every symbol.
   Examples:

   `ab` is a rainbow pattern with colors ["a", "b"], each has cell width of 1

   `eeeeefffffggggg` is a rainbow pattern with colors ["e", "f", "g"], each has cell width of 5

   A 12-pixel row ![image](./docs/rainbow.png) is a rainbow pattern with colors [#FF0000, #00FF00, #0000FF], each has cell width of 4

2. Irregular Rainbow

   Irregular Rainbow pattern is the same like Rainbow pattern, but the cell streak lengths are different. Examples:

   `aab` is an irregular rainbow pattern with colors ["a", "b"] and cell widths of [2, 1]

   `eeeffffgggggg` is an irregular rainbow pattern with colors ["e", "f", "g"] and cell widths of [3, 4, 5]

   A 12-pixel row ![image](./docs/rainbow_irregular.png) is a rainbow pattern with colors [#FF0000, #00FF00, #0000FF] and cell widths of [2, 6, 4]

3. Checkerboard

   Checkerboard pattern is composed of cells of a symbol followed by cells of other symbols with at least 1 repetition to the first symbol. The length of cell streak is the same for every symbol. Examples:

   `ababab`, is a checkerboard pattern with colors ["a", "b"] repeated 3 times, each has cell width of 1

   `eeffggee`, is a checkerboard pattern with colors ["e", "f", "g"] repeated ~2 times (incomplete), each has cell width of 2

   A 12-pixel row ![image](./docs/checker.png) is a checkerboard pattern with colors [#FF0000, #00FF00, #0000FF] repeated twice, each has cell width of 2

4. Irregular Checkerboard

   Irregular Checkerboard pattern is the same like Checkerboard pattern, but the cell streak lengths are different. Examples:

   `aababbb` is an irregular checkerboard pattern with colors ["a", "b"] repeated 2 times, and widths of [2, 1, 1, 3]

   `eeffgge`, is an irregular checkerboard pattern with colors ["e", "f", "g"] repeated ~2 times (incomplete), and widths of [2, 2, 2, 1]

   A 12-pixel row ![image](./docs/checker_irregular.png) is an irregular checkerboard pattern with colors [#FF0000, #00FF00, #0000FF] repeated twice, and widths of [1, 3, 2, 1, 3, 2]

5. Wave

   Wave patterns is composed of cells of a symbol followed by cells of other symbol, but after the last unique symbol is used it's followed by the previous symbols in reverse order. The length of cell streak must be the same for every symbol. Examples:

   `abcbabcba` is a wave pattern with colors ["a", "b", "c"], each has cell width of 1

   `eeffgghhggffeeff` is a wave pattern with colors ["e", "f", "g", "h"], each has cell width of 2

   A 12-pixel row ![image](./docs/wave.png) is a wave pattern with colors [#FFFFFF, #999999, #000000], each has cell width of 1

6. Irregular Wave

   Irregular Wave pattern is the same like Wave pattern, but the cell streak lengths are different. Examples:

   `abbcbaa` is a wave pattern with colors ["a", "b", "c"] and cell widths of [1, 2, 1, 1, 2]

   `efgghhhggfe` is a wave pattern with colors ["e", "f", "g", "h"] and cell widths of [1, 1, 2, 3, 2, 1, 1]

   A 12-pixel row ![image](./docs/wave_irregular.png) is a wave pattern with colors [#FFFFFF, #999999, #000000] and cell widths of [1, 4, 2, 4, 1]

Parsing Rule
------------

1. Program is parsed line by line. For text programs, a line is ended with CRLF or LF. For image programs, a line is simply a row in the image. The parser will try to match the line to one of the seven patterns.
2. Pattern matching is always done in this order: solid, rainbow, irregular rainbow, checkerboard, irregular checkerboard, wave, irregular wave.
3. Every line that is empty or didn't match any of the seven patterns is skipped as a comment. Example:

    ```
    this is a comment, because it doesn't match any pattern
    also a comment

    the next one is not a comment, since it matches irregular rainbow of colors ["c", "o", "m", "e", "n", "t"]
    comment
    ```
4. Every line that has the same exact pattern (in type, colors, and cell widths) with the last matched line is skipped. This is done to make sure multiple rows of same pattern is treated as one row. Examples:

   ```
   aaaaaa
   aaaaaa
   aaaaaa
   ababab
   ababab
   efefef
   aabbaa
   aabbaa
   ```

   is equivalent to

   ```
   aaaaaa
   ababab
   efefef
   aabbaa
   ```

   It also skip same lines even if it's seperated by empty lines and comments.

   ```
   aaaaaa
   this is a comment


   aaaaaa
   bbbbbb

   bbbbbb
   aaaaaa

   yet another comment

   aaaaaa
   ```

   is equivalent to
   ```
   aaaaaa
   bbbbbb
   aaaaaa
   ```

   Image rows example.

   ![image](./docs/image_comment.png)

   is equivalent to

   ![image](./docs/solid.png)



5. First matched pattern will determine the length of all subsequent patterns. Any line that doesn't have the right length will be skipped as a comment, even if it may match a pattern. This rule is only relevant to text programs, because lines always have consistent length in image programs. Example:
   ```
   aabbcc
   abc
   aaaa
   aaaaaa
   bbbbbbbbbbbbbb
   aaaaaa
   ```

   is equivalent to

   ```
   aabbcc
   aaaaaa
   ```

Program structure
-----------------

A motif program is composed of palettes and sections. A palette defines what symbols (UTF-8 characters or 32-bit RGBA color) that can be used in the program and maps them to color IDs. Palettes are declared by using any non-solid patterns matched before the first section. Every palette must has different symbols

Sections serve like functions like in other programming languages. They are declared with a solid pattern, and followed by the pattern-encoded instructions that will be executed when the section is called. The first section after palette declarations is the main section, meaning it will be called first when the program runs.

Consider this text program:
```
abcdef
ghighi

iiiiii
aaabbb
ababab
efefef
hhhiii

eeeeee
dddeee
fghgfg
```

The program has 2 palettes definition: `abcdef` and `ghighi`. Together, these palettes are interpreted as the following symbol-color id mapping:
```
a = 0, b = 1, c = 2, d = 3, e = 4, f = 5, g = 6, h = 7, i = 8
```

The program also has 2 sections:
1. Section 8 (or, colored "i"). It contains 2 pattern-encoded instructions: rainbow->checker (`aaabbb`->`ababab`) and checker->rainbow (`efefef`->`hhhiii`).
   This is the main section.

2. Section 4 (or, colored "e"). It contains 1 pattern-encoded instruction: rainbow->wave (`dddeee`->`fghgfg`).

Consider this image program:

![image](./docs/image_program_structure.png)

The program has 1 palette definition: ![image](./docs/image_palette_definition.png). It will be interpreted as the following mapping:
```
red (#FF0000) = 0, green (#00FF00) = 1, blue (#0000FF) = 2, yellow (#FFFF00) = 3
```

The program also has only 1 section, Section 0 (or, colored red). It contains 1 pattern encoded instruction rainbow->checker ( ![image](./docs/image_struct_instr1.png) -> ![image](./docs/image_struct_instr2.png) )
