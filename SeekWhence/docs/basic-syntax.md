# Basic Syntax

**Documentation Table of Contents**
- [Basic Syntax](./basic-syntax.md)
- [Sequences and Slices](./sequences.md)
- [Namespaces and Imports](./namespaces.md)
- Standard Library
  - [builtin](./stdlib/builtin.md) - Core functions and constants
  - [math](./stdlib/math.md) - Additional math functions and constants
  - [strutils](./stdlib/strutils.md) - String manipulation utilities
  - [time](./stdlib/time.md) - Time processing utilities

### Comments
```racket
; SeekWhence comments start with a semicolon, and run to the end of the line.
; Block comments aren't supported.
```


### Basic datatypes
```racket
; Integers
100
-100

; Floats
100.0
-100.0
INF
NaN

; Strings
"Hello, world!"
'Hello, world!'
"Standard escape sequences like \" and \n are supported in both quote types."

; Booleans
true
false

; None (equivalent to Python's None)
none

; Valid identifiers consist of alphanumeric characters and underscores.
; camelCase is recommended.

someIdentifier
some_identifier
```

### Operators
```racket
; The following operators are supported in expressions, from highest to lowest precedence.

: ::               ; Sequence index and slice
-                  ; Unary negation
^                  ; Exponentiation
* / %              ; Multiplication, division, modulus
+ -                ; Addition, subtraction
> < >= <= == !=    ; Comparison and equality
and or             ; Logical and/or
```


### Variables
```racket
; You can set or update a variable with the set statement. Variables are always set
;   in the current scope, even if the name is present in a higher scope.

set variable = 10 + 15
```


### Functions
```racket
; You can define a function with fairly familiar syntax. Functions are closures,
;   and support the same braceless single-statement syntax as if and for.

function sayHello {
  print "Hello, world!"
}

function greet with greetee {
  print "Hello,", greetee
}

function sum with x, y return x + y


; Function calls are written without parentheses. If used in an expression, they
;   are wrapped in [].

sayHello
greet 'SeekWhence'
print [sum 10, 15]
```


### If Statements and For Loops
```racket
; If statements should be fairly familiar syntax for most programmers.

if true then {
  print "Hello!"
}

if false then {
  print "Oh no :("
} else {
  print "Yay!"
}

; You can omit braces when the following block consists of only one statement.

if true then print "Hello!"
if false then print "Oh no :(" else print "Yay!"


; For loops iterate over a sequence, taking each step as a given identifier.

sequence indices = n

for 10 of indices as i do {
  print i
}

for 10 of indices as i do print i

; To iterate indefinitely, you can use the special :inf keyword. Make sure to
;   use the break statement to stop the loop eventually.

for :inf of indices as i do {
  print i
  if i >= 10 then break
}
```
