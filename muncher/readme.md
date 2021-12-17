> Note: this was imported from https://github.com/jDomantas/muncher. Head to the [(vote)](https://github.com/langjam/jam0002/pull/16)
> original repo to find updates and pre-built binaries.


# Muncher programming language

This is an entry to the second langjam. Theme of the contest was "patterns".


## Language

Muncher is a dynamically typed object oriented programming language. The twist
is that objects have an ability to define the syntax of its method invocations,
allowing users to recreate many basic language features (ifs, loops, lambda
functions, etc.) in their own code without having to use some generic
compiler-friendly syntax.

The idea is that objects method should be thought as defined in terms of pattern
macthing on syntax instead of an object providing a few syntactic features to
access object's properties.

Take a look at the [intro example](./programs/examples/intro) for an overview of
the language.

Take a look at other [examples](./programs/examples) to see some small programs
written in muncher.


## Interpreter

Interpreter can run single-file programs. Usage:

```shell
muncher <path-to-file>
```

The only IO primitive available to programs is `print` (which writes to stdout).
Currently there's no way for a program to read input.

Interpreter uses the native stack to interpret function calls. Therefore on
highly recursive programs it might fail with a stack overflow. You can increase
the native stack size with `--stack` option:

```shell
# run with 64 megabyte stack
muncher program.mnc --stack 64
```


## Installation

You can download pre-built binaries from [Github releases](https://github.com/jDomantas/muncher/releases).
Note that linux binaries did not receive much testing.

### From source

If you have rust tooling installed (rustc, cargo) you can install the
latest version of the interpreter straight from this repo:

```shell
cargo install muncher --git https://github.com/jDomantas/muncher.git
```

Muncher requires stable rust 1.57.0 to build. Older versions might work but were
not tested.
