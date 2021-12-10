# math - Additional math functions and constants

**Documentation Table of Contents**
- [Basic Syntax](./basic-syntax.md)
- [Sequences and Slices](./sequences.md)
- [Namespaces and Imports](./namespaces.md)
- Standard Library
  - [builtin](./stdlib/builtin.md) - Core functions and constants
  - [math](./stdlib/math.md) - Additional math functions and constants
  - [strutils](./stdlib/strutils.md) - String manipulation utilities
  - [time](./stdlib/time.md) - Time processing utilities

## Import
```racket
import math from 'math'
```


## Functions

| Name | Arguments | Description |
| :--- | :-------- | :---------- |
| `sqrt` | `x` Input value | Square root |
| `log`  | `x` Input value<br>`b` Log base | Logarithmic function<br>If `b` is not given, defaults to natural log. |
| `sin` | `x` Input value | Trigonometric sine function |
| `cos` | `x` Input value | Trigonometric cosine function |
| `tan` | `x` Input value | Trigonometric tangent function |
| `asin` | `x` Input value | Trigonometric arcsine function |
| `acos` | `x` Input value | Trigonometric arccosine function |
| `atan` | `x` Input value | Trigonometric arctangent function |
| `atan2` | `x` Horizontal axis position<br>`y` Vertical axis position | Equivalent to `[atan y/x]`, but retains quadrant information. |
| `toDeg` | `x` Input value | Convert from radians to degrees |
| `toRad` | `x` Input value | Convert from degrees to radians |


## Constants

| Name | Description |
| :--- | :---------- |
| `PI` | The value of pi (circumferance / diameter), to available precision |
| `E` | The value of e (Euler's number), to available precision |
| `TAU` | The value of tau (2 * PI), to available precision |
| `PHI` | The value of phi (the golden ratio), to available precision |
