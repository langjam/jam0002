# Namespaces and Imports

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
