# Sequences and Slices

**Documentation Table of Contents**
- [Basic Syntax](./basic-syntax.md)
- [Sequences and Slices](./sequences.md)
- [Namespaces and Imports](./namespaces.md)
- Standard Library
  - [builtin](./stdlib/builtin.md) - Core functions and constants
  - [math](./stdlib/math.md) - Additional math functions and constants
  - [strutils](./stdlib/strutils.md) - String manipulation utilities
  - [time](./stdlib/time.md) - Time processing utilities
  
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
