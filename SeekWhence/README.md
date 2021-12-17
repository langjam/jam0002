# SeekWhence [(vote)](https://github.com/langjam/jam0002/pull/7)

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

You can find several examples of SeekWhence programs in the `examples` folder. Most of these also serve as the test cases for the interpreter and standard library; you can find the descriptions of their expected output in the `tests` folder.


## Language Documentation

- [Basic Syntax](./docs/basic-syntax.md)
- [Sequences and Slices](./docs/sequences.md)
- [Namespaces and Imports](./docs/namespaces.md)
- Standard Library
  - [builtin](./docs/stdlib/builtin.md) - Core functions and constants
  - [math](./docs/stdlib/math.md) - Additional math functions and constants
  - [strutils](./docs/stdlib/strutils.md) - String manipulation utilities
  - [time](./docs/stdlib/time.md) - Time processing utilities

