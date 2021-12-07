Sand Dune is a language with a visual pattern-matching system (inspired by PuzzleScript).

Expressing 1D cellular automata is really easy.
For example, to see a glider in the Rule 110 automaton, you could write:

```
Atoms
  cell '#'
Rules
  [ | | ] -> [ | cell | ]                    # By default, cells are on...
  [ | No cell | No cell ] -> [ | No cell | ] # Unless they and their rightwards neighbor are off...
  [ cell | cell | cell ] -> [ | No cell | ]  # Or they and both neighbors are on.
Start
  # The "default wallpaper", with a glider moving to the right:
  [   #  ## #####   ### ###   #  ## #####   #  ## #####   #  ## #####]
```

Other Rule 110 patterns to try:
  * Default wallpaper:  `[   #  ## #####   #  ## #####   #  ## #####   #  ## #####]`
  * Just one start cell:  `[                                                      # ]`
  * Meandering leftward glider: `[   #  ## #####   #  ## ######  ####   #  ## #####   #  ## #####]`
