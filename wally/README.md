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

* Error reporting for the language is not very helpful. For eg. the line number of error is not shown. If you get some parser error containing `tao::pegtl` ( or `tao::pegtl::eof` ) make sure the syntax is correct. You can try removing trailing spaces and adding new lines. For more info look at examples inside `examples` folder or the **Language documentation** section.


## Language documentation

The syntax is very similar to `C`. Wally is a simple language with a small set of allowed statements.

* Variables
  * All variables in Wally are converted to `double` in `C`.
  * Variable can be declared using `var <variable_name>`
  * Variable names must start with lower case letter and can contain alpha-numeric or `'_'` characters.
  * Floating numbers or other variables can be used inside a `expression` to assign value to a variable
  * An optional initialisation expression can be added using `var <variable_name> = expression` on the same line as the declaration. If not present, the variable is initialised to `0` .
  * Assigning a value to a variable is very similar to `C`. `variable-name = expression`. Look at `Patterns` section for more info on assigning to "locked" variables

* If-condition, While-loop
  * Their bodies can contain only value assignments to variables and other if-condition, while-loops
  * Variables cannot be declared inside them.
  * Conditions can compare two `expressions` using `>`, `<`, `==`, `>=`, `<=`, `!=`.
  * Multiple conditions can be added using `||`, `&&`.
  *  The condition statements are directly converted to `C` code, so their implementation is left to `C` language.
  *  The body should always be written between `{` and `}`.
  *  If there are any parse errors try to remove spaces and empty lines. Refer to `examples` for what is allowed.

* `Print ( )`
  * Can be used to print variables
  * takes only one variable as argument ( expressions are not allowed )

* Comments
  * Wally has only one-line comments
  * They are similar to `C`, starting with `\\` until end of line everything is considered a comment

* **Patterns**
  * Patterns start with `/*` and end with `*/` and should be on one line.
  * Between `/*` and `*/` it can have a sequence of `' '` (space) or `*` characters. For eg. `/*  ** * */` is a valid pattern
  * If it is placed directly above a variable declaration statement, then the variable becomes 'locked' and the pattern is assigned to it.
  * The value of Locked variables can be accessed in any expression
  * But to assign a value to the locked variable, the assigned pattern must be present **directly above the assignment statement** (might give parsing errrors if it is not directly above an assignment statement, but the message might not be helpful)
  * Look at `examples\err_pattern.wly` and `examples\all_test.wly`.
  * If correct pattern is used, the variable will be unlocked only for the next statement. Pattern needs to supplied for each statement separately.

## Examples

* All examples files have meaningful names to explain what code they contain.
* If you are having parsing errors, try to use the code in the examples ( the parser is not fully tested, so even extra spaces or empty lines may break it )
* Compile an example using `make <example>.wly`