# SeekWhence

A simple programming language built around mathematical sequences as a primitive.

SeekWhence was created in 28 hours over the course of a week for [LangJam 2](https://github.com/langjam/jam0002).

## Installation

- Clone or download the source
- Make sure you have Python 3.8.10 or higher installed

To run a SeekWhence script, run 
```bash
chmod +x ./seekwhence
./seekwhence [script path]
```

To run all interpreter tests, run 
```bash
python3 src/test.py tests
```


## Examples

You can find several examples of SeekWhence programs in the `examples` folder. These also serve as the test cases for the interpreter; you can find the descriptions of their expected output in the `tests` folder.


## Syntax and Semantics

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

### Sequences and Slices
```racket
; Sequence definitions consist of an identifier, an optional list of base cases,
;   and a list of expressions, with each list separated by commas. The base cases
;   are used for indices starting from 0, and indices past the end of the base
;   cases are calculated by the expressions. If there are multiple expressions,
;   they are rotated through for each successive index, looping back to the start
;   once the last expression has been used.
;
; The following special variables can be used in sequence expressions:
;   n - The current index
;   m - The position of the current expression in the list ((n - # of base cases) % # of expressions)
;   x - The value of the previous index, equivalent to S:(n-1)
;   S - The sequence itself
;
; Negative indices of a sequence always return 0 as a default base case.

sequence indices = n
sequence factorial from 1 = x * n
sequence fibonacci from 0, 1 = x + S:(n-2)
sequence alternate = x+1, x-1

; Sequences can be indexed like arrays.

indices:12     ; 12
factorial:5    ; 120
fibonacci:12   ; 144


; Sequences can also be sliced, which creates a lightweight view of the underlying
;   sequence which starts from a different index. Slices close over their underlying
;   sequence, and can be treated identically to sequences in almost all cases.
;
; The 0 index of a slice is equivalent to the N index of its underlying sequence,
;   where N is the number specified when constructing the slice.

indices::12     ; 12, 13, 14, 15...
factorial::5    ; 120, 720, 5040...
fibonacci::12   ; 144, 233, 377...

; You can also slice a sequence on construction, which will assign the slice to the
;   given name instead of the underlying sequence.

sequence sliceSugar::10 = n

; The above is equivalent to...

sequence sliceSugar = n
set sliceSugar = sliceSugar::10


; Sequences perform aggressive constant and operation folding on construction. As a
;   result, the following sequences are all equivalent. Keep in mind that this is
;   only implemented to a certain extent; expressions may not be simplified as far
;   as is technically possible. Notably, division does not currently fold unless both
;   operands are constants.

sequence folding1 from 5      = n^3
sequence folding2 from (4+1)  = n^2 * n
sequence folding3 from (7-2)  = n * n * n
sequence folding4 from (10/2) = n^3

print folding1   ; [sequence folding1: 5 | (n ^ 3)]
print folding2   ; [sequence folding2: 5 | (n ^ 3)]
print folding3   ; [sequence folding3: 5 | (n ^ 3)]
print folding4   ; [sequence folding4: 5.0 | (n ^ 3)]


; You can perform arithmetic operations directly on sequences and slices. The result
;   is an anonymous sequence with each base case and expression wrapped in the given
;   operation. When done on a slice, the result is an equivalent slice of the new
;   anonymous sequence.
;
; To modify just the base cases, suffix the operator with ~, e.g. +~
; To modify just the expressions, suffix the operator with :, e.g. +:
; The order of operations is preserved; 4 - factorial != factorial - 4

sequence factorial from 1 = x * n

print factorial + 4       ; [sequence <factorial+4>: 5 | ((x * n) + 4)]
print factorial +~ 4      ; [sequence <factorial+~4>: 5 | (x * n)]
print factorial +: 4      ; [sequence <factorial+:4>: 1 | ((x * n) + 4)]

print factorial::10 + 4   ; [slice <factorial+4>::10]


; Sequences can be directly compared for equality. Two sequences are equal if they
;   can be determined equivalent; the system is not especially smart, but it understands
;   commutative operations and function calls with equivalent arguments.
;
; Slices are equal if their underlying sequences and their slice indexes are both equal.
; A sequence is equal to a slice if the underlying sequence is equal and the slice index is 0.

sequence other_factorial from 1 = n * x

print factorial == other_factorial           ; true
print factorial == factorial::0              ; true
print factorial::10 == other_factorial::10   ; true
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

### Namespaces and Imports
```racket
; You can create a namespace with the namespace keyword. Variables, sequences, and
;   functions created inside a namespace are assigned to the namespace rather than
;   the global scope.

namespace example {
  sequence factorial = n
}

; To access a variable, sequence, or function from a namespace, you can use the ~
;   operator to unwrap the namespace.

print example~factorial:5   ; 120


; To import a file, you can use the import statement. Imports create a namespace of
;   everything in the imported file's global scope. Imported files must use the .seq
;   extension.

; === imported.seq

set variable = "Hello, world!"

; === importer.seq

import imp from 'imported'
print imp~variable           ; Hello, world!


; You can also import files from the language's standard library. There are currently
;   two stdlib modules, math and strutils.

import math from 'math'
import strutils from 'strutils'

print math       ; [namespace math { sqrt, log, sin, cos, tan, asin, acos, atan, atan2, toDeg, toRad, PI, E, TAU, PHI }]
print strutils   ; [namespace strutils { length, startsWith, endsWith, indexOf, split, toSeq, trim, trimLeft, trimRight }]
```

### Interpreter meta variables
```
print $file     ; Absolute path of current script
print $dir      ; Absolute path to directory of current script
print $stdlib   ; Absolute path to interpreter stdlib directory
print $main     ; true if this file is the entry point
```
