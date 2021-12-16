**Sand Dune** is a language with a visual pattern-matching system (inspired by [(vote)](https://github.com/langjam/jam0002/pull/15)
[PuzzleScript](https://www.puzzlescript.net/Documentation/rules101.html)).

Check out the [live demo](https://paulstansifer.github.io/sand_dune/ide.html),
which has the same information as this README, but with a live demo!

I didn't have much time (I spent about six hours on coding), so it's pretty basic.
Mainly, it can express 1D cellular automata, though with arbitrarily-large
neighborhoods and a nice notation. So in addition to using spacial pattern-matching,
its primary purpose is to make pretty patterns!

The LHS of each rule is a pattern (underscore means "don't care"),
and the RHS of each rule is an action (underscore means "don't do anything").
Rules are applied in sequence, with later rules taking precedence over earlier
rules, but (unlike in PuzzleScript!) the rules always look at the initial state
(i.e., they can't see each others' effects).

Sand Dune is implemented in [Unseemly](https://github.com/paulstansifer/unseemly/), a macro language
with support for typed macros. The [Web IDE](https://paulstansifer.github.io/sand_dune/ide.html) and
its syntax-highlighter (not that there's much syntax) were automatically-derived.
The actual typedness of Unseemly macros wasn't too useful for this language, but a
couple of ancillary features of Unseemly (arbitrary scannerless grammars for macros,
and support for Macro By Example like in Rust or Scheme) were quite helpful.

## Running Sand Dune

```
$ sudo apt install cargo        # or your distro's equivalent
$ cargo install unseemly
```

Then, from the `jam0002/sand_dune` directory:
```
$ unseemly sand_dune.unseemly rule_110_glider.sand_dune
```

## Examples

```
# Rule 110
Rules
  [ _ ] -> [ cell ]                               # By default, cells are on...
  [ No cell | No cell ] -> [ No cell | _ ]        # Unless they and their rightwards neighbor are off...
  [ cell | cell | cell ] -> [ _ | No cell | _ ]   # Or they and both neighbors are on.
Start
  # The "default wallpaper", with a glider moving to the right:
  [   +  ++ +++++   +++ +++   +  ++ +++++   +  ++ +++++   +  ++ +++++]
Steps
  12
```

```
# Rule 62: A Christmas tree
Rules
  [ _ ] -> [ cell ]
  [ cell | cell ] -> [ _ | No cell ]
  [ No cell | No cell | No cell ] -> [ _ | No cell | _ ]
Start
  [               +               ]
Steps
  16
```

```
# Rule 30: A cool chaotic pattern
Rules
  [ _ ] -> [ No cell ]
  [ cell | No cell] -> [ cell | _ ]
  [ cell | No cell | No cell] -> [ _ | cell | _ ]
  [ No cell | No cell | cell] -> [ _ | cell | _ ]
Start
  [               +               ]
Steps
  15
```

```
# Elementary 1D cellular automata have a neighborhood of 3 cells,
# but Sand Dune allows arbitrarily-large neighborhoods.
# This pattern expands rightward faster than would be possible with 3 cells
Rules
  [ _ | cell | _ | _ | _ | _ ] -> [ cell | No cell | _ | _ | _ | cell]
Start
  [             +                                         ]
Steps
  15
```