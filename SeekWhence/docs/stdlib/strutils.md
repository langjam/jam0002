# strutils - String manipulation utilities

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
import strutils from 'strutils'
```


## Functions

| Name | Arguments | Description |
| :--- | :-------- | :---------- |
| `length` | `s` Input string | The length of the given string. |
| `startsWith` | `h` Haystack<br>`n` Needle | `true` if the haystack starts with the needle. |
| `endsWith` | `h` Haystack<br>`n` Needle | `true` if the haystack ends with the needle. |
| `indexOf` | `h` Haystack<br>`n` Needle | The position of the needle in the haystack, or `none` if it isn't present. |
| `split` | `h` Haystack<br>`n` Needle | A slice containing the string segments of the haystack, delimited by the needle.<br>`[split]:-1` is `none`. |
| `toSeq` | `s` Input string | A slice containing the characters of the string.<br>`[toSeq]:-1` is `none`. |
| `trim` | `s` Input string | The input string with all whitespace trimmed from the start and end. |
| `trimLeft` | `s` Input string | The input string with all whitespace trimmed from the start. |
| `trimRight` | `s` Input string | The input string with all whitespace trimmed from the end. |

