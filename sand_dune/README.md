Sand Dune is a language with a visual pattern-matching system (inspired by PuzzleScript).

I didn't have much time, so it's pretty basic. Mainly, it can express 1D cellular automata, though
with arbitrarily large neighborhoods and a nice notation.
It wouldn't be hard to add a more powerful model (like PuzzleScript's powerful
bag-of-atoms-with-directions model) in place of the simple cell-or-no-cell booleans.

Here's the famously Turing-complete Rule 110:

```
Rules
  [ _ ] -> [ cell ]                               # By default, cells are on...
  [ No cell | No cell ] -> [ No cell | _ ]        # Unless they and their rightwards neighbor are off...
  [ cell | cell | cell ] -> [ _ | No cell | _ ]   # Or they and both neighbors are on.
Start
  # The "default wallpaper", with a glider moving to the right:
  [   #  ## #####   ### ###   #  ## #####   #  ## #####   #  ## #####]
```

To run Sand Dune:
```
$ sudo apt install cargo                 # Or your distro's equivalent
$ cargo install unseemly                 # Sand Dune is implemented in Unseemly
$ unseemly sand_dune.unseemly rule_110_glider.sand_dune
```

