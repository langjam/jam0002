# builtin - Core functions and constants

**Documentation Table of Contents**
- [Basic Syntax](../basic-syntax.md)
- [Sequences and Slices](../sequences.md)
- [Namespaces and Imports](../namespaces.md)
- Standard Library
  - [builtin](./builtin.md) - Core functions and constants
  - [math](./math.md) - Additional math functions and constants
  - [strutils](./strutils.md) - String manipulation utilities
  - [time](./time.md) - Time processing utilities

## Import
This module is automatically imported into every file. It should not be manually imported.


## Functions

| Name | Arguments | Description |
| :--- | :-------- | :---------- |
| `print` | `s` Input string (repeatable) | Prints the input string to the console.<br>If multiple inputs are passed, prints them separated by spaces. |
| `input` | `p` Prompt (optional) | If piped stdin is present, reads a line from stdin.<br>Otherwise, prints the prompt to the console if given, then waits to read a line from user input. |
| `systime` | - | Returns the current Unix time in milliseconds. |
| `floor` | `x` Input value | Rounds the given number towards negative infinity. |
| `ceil` | `x` Input value | Rounds the given number towards positive infinity. |
| `round` | `x` Input value | Rounds the given number to the nearest integer. |
| `abs` | `x` Input value | Returns the absolute value of the given number. |
| `sign` | `x` Input value | Returns the sign of the given number (-1, 0, 1). |
| `unslice` | `s` Input slice | Converts a slice to its underlying sequence, or returns the input if it isn't a slice. |
| `isSequence` | `x` Input value | `true` if the input value is a sequence or slice. |
| `isSlice` | `x` Input value | `true` if the input value is a slice. |
| `isNumber` | `x` Input value | `true` if the input value is an integer or float. |
| `isString` | `x` Input value | `true` if the input value is a string. |
| `isBoolean` | `x` Input value | `true` if the input value is a boolean. |
| `isNone` | `x` Input value | `true` if the input value is `none`. |
| `not` | `b` Input boolean | Returns `true` if the input value is falsey, or `false` if it's truthy. |


## Constants

| Name | Description |
| :--- | :---------- |
| `true` | Boolean true value. |
| `false` | Boolean false value. |
| `none` | Represents a missing or error value (equivalent to Python's `None`). |
| `INF` | Floating point positive infinity. |
| `NaN` | Floating point "not a number" marker. |
