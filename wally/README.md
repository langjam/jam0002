# Wally Lang

Wally lang is a simple programming language where variables can be "locked" using "patterns". To assign a value to these "locked" variables they have to be unlocked using the pattern that was assigned to the variable when it was declared ( like passwords ).

## Building

* Create a `bin` folder if it doesn't exist
* Run `make` inside the project directory, this will create an executable ( `wally` ) inside `bin\`

## Compiling a file

* Wally lang files should have `.wly` as extension

* `wally` program takes a file path as an argument and compiles that to `C` code
  * The generated `C` code can be compiled with any C compiler

* To automate the above steps, the `Makefile` has a recipe.

    ```make <file>.wly```

  * this compiles the Wally code to `C`, compiles the `.c` file to an executable using `gcc` and runs it.
  * Refer to `%.wly` recipe inside `Makefile`


## Language documentation

* All variables in Wally are converted to `double` in `C`.

## Examples