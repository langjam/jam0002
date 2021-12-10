# [`mosaic`](https://github.com/tjjfvi/mosaic)

_An esoteric programming language which performs computations by creating
beautiful mosaic patterns_

Created for [langjam #2](https://github.com/langjam/jam0002).

### [It is showcased in an interactive interpreter with multiple examples, featuring procedurally generated 3D visualizations of the mosaics!](https://mosaic.t6.fyi/)

## Installation

- Go to https://mosaic.t6.fyi/

Or, for a command line version:

- Install the [Rustup toolchain](https://www.rust-lang.org/tools/install)
- `cargo run --release your/file.mosaic`
  - Requires input from stdin; if you don't want any input, pipe an empty stream into it.
  - Outputs to stdout, and prints debug info to stderr.

To build the interactive interpreter (an online version is available at
https://mosaic.t6.fyi/):

- Install node.js (in addition to rust from earlier)
- `cd web && npm ci && npm run serve`
- Open `localhost:8080`

## Documentation

### Overview

Programs in mosaic operate by appling replacement patterns to the mosaic, an
infinite grid of tiles.

Each tile in the mosaic is represented by 2 characters; the first represents the
color of the tile, and the second respresents the symbol on the tile.

The program source code consists of a initial mosaic and a list of
instructions, the most important of which are replacement patterns.

### Initial Mosaic

The initial mosaic is defined using a grid of pairs of characters, separated by
spaces and newlines.

For examples, here is a 3x3 grid containing 5 non-blank tiles (non-trailing
blank tiles are represented by `..` (two periods)):

```
ab cd ef
.. 12
x#
```

The rest of the infinite mosaic is intialized with blank tiles.

### Patterns

Patterns are represented by two grids, separated by multiple spaces;
the left is the matcher and the right is the replacement. The special character
`_` acts as a wildcard.

Here is an example of a pattern that operates on a 2x2 section of the mosaic:

```
a_ cd  aa ..
.. __  cd __
```

Applying this rule to the above initial mosaic would match and result in:

```
aa .. ef
cd 12
x#
```

Were this pattern to be repeated, it would not match, as there is
no matching section of the mosaic.

If multiple matches for the pattern exist, it only replaces the earliest one
(left to right, then top to bottom).

### Control Flow

Control flow in mosaic is accomplished through loops. Loops are opened and
closed with `[` and `]`, respectively, and their body consists of one or more statements.

When a loop is reached, it first runs all of the instructions in the body. If
any patterns matched, it will run its body again. Once an
iteration of the loop body results in no successful replacements, the loop ends.

For example, given this program:

```
bb .. .. bb

bb  aa
[
  aa ..   .. aa
]
aa bb  bb bb
```

Execution would proceed as follows:

- The mosaic is initialized to `bb .. .. bb`.
- The first pattern matches, changing the mosaic to `aa .. .. bb`.
- The loop is entered.
  - We execute all instructions in the body:
    - The pattern matches, changing the mosaic to `.. aa .. bb`.
  - The end of the loop is reached. A pattern matched, so we return to the start of the loop.
  - We execute all instructions in the body:
    - The pattern matches, changing the mosaic to `.. .. aa bb`.
  - The end of the loop is reached; we return to the start of the loop.
  - We execute all instructions in the body:
    - The pattern does not match, so we skip it.
  - The end of the loop is reached. No pattern matched, so we exit the loop.
- The final pattern matches, changing the mosaic to `.. .. bb bb`.
- The end of the program is reached.

### I/O

I/O in mosaic is accomplished through four commands: `i` and `I` for input; `o`
and `O` for output. Commands `i` and `o` use a character encoding, while `I` and
`O` use a binary encoding (explained further below).

All I/O commands must be followed by a single tile matcher pattern (e.g `i a_`).

#### `i`

If the `i` command finds a matching tile, it reads a byte from the input and puts it
in the symbol position of that tile.

For example, given the program:

```
aa ab

i a_
```

If the input is `X`, the mosaic will become:

```
aX ab
```

If the character is not a valid tile symbol (e.g. whitespace) or the input is
empty, it will not a make areplacement.

#### `I`

If the `I` command finds 8 matching tiles, it reads 8 bits from the input and
puts either `0` or `1` in the symbols of those 8 tiles.

For example, given:

```
aa ab ac ad
aa ab ac ad ae

I a_
```

If the input is `X` (`01011000` in binary), the mosaic will become:

```
a0 a1 a0 a1
a1 a0 a0 a0 ae
```

If the input is empty, it will not make any replacements.

#### `o`

If the `o` command finds a matching tile, it outputs the tile's symbol.

For example, given:

```
aX aY

o a_

o _Y
```

The output will be `XY`.

#### `O`

If the `o` command finds 8 matching tiles, it outputs the character represented
by the symbols of the tiles interpreted as binary (where `1` is `1`
and any other character is `0`).

For example, given:

```
a0 a1 az a1
a1 a0 a0 a0 ae

O a_
```

The output will be `X` (`01011000` in binary).

#### I/O Example

A simple `cat` program:

```
a. a. a. a. a. a. a. a.

[
  # input into the 8 `a.` tiles
  I a.

  [
    # if any tiles are still `a.`, eof was reached, so replace each with `..` to end the main loop
    a.  ..
  ]

  # output the byte just inputted
  O a_

  [
    # return the bits to `a.` for the next iteration of the loop

    a0  a.

    a1  a.
  ]
]
```

### Debug

The `.` command acts as a debug statement. In the command line version, this
prints a textual representation of the mosaic; in the visual interpreter, it
adds additional delay to the frame.

### Comments

Comments start with a `#` and a space, and continue to the end of the line.

Comments are only allowed in the instruction area, not the initial mosaic.
