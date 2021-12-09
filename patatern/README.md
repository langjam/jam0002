# Patatern

## Overwiew

Patatern is a minimalistic logic language.
Due to its simplicity, it is suitable as an introduction to logic programming.

It can be thought of as a Prolog dialect without backtracking
(it always commits to the first matching rule),
with a comma-free syntax that allows writing predicates that resemble natural language.

## How to run

### Requirements

In order to build and run the patatern interpreter,
you must have `ghc` and `cabal` installed on your system.

You can install both `ghc` and `cabal` with [ghcup](https://www.haskell.org/ghcup/).

### Installing the patatern executable

Once you are done, you can install patatern by executing `cabal install`
in the `patatern` directory.
This will copy the `patatern` executable to `$HOME/.cabal/bin`.

### Running without installing

Alternatively, you can run patatern on the fly (without installing it)
by executing `cabal run` in the `patatern` directory.

## Syntax

### Comments

```
; Single-line comment

{- Block
   comment -}
```

### Symbols

Symbols start with a lower-case letter.

```
foo
bar
baz
```

### Integers

```
1
5
42
```

### Variables

Variables start with an upper-case letter.

```
Foo
Bar
Baz
```

### Pairs

Pairs are defined by simple juxtaposition of two terms.
They are left-associative.

```
a b
a b c ; equivalent to (a b) c
```

### Lists

Lists are just syntactic sugar for right-associative pairs
ending with the symbol `fin`.

```
[a b c] ; desugars to a (b (c fin))
```

### Facts

A fact is simply a patatern term terminated by a `.`.

```
john is 23 years old.
the brown fox jumps over the lazy dog.
```

### Rules

A rule is a fact with a sequence of comma-separated terms
on the right-hand side.

```
sayHello: print hello.
sayHelloTwice: print hello, print hello.
```

## The language

### The knowledge base

A patatern program is composed of a list of files,
each containing one or more definitions (facts or rules).

You can load a set of rules by executing the `:load` command
in the interactive interpreter.

```
?> :load file1.pt file2.pt
```

### The queries

A program is started by typing a query into the interpreter.

A query is simply a sequence of comma-separated terms,
terminated by a `.`.

Each term is matched against all the definitions in the knowledge base,
in a top-down fashion, until it finds a matching definition.
Matching is performed through [unification](https://en.wikipedia.org/wiki/Unification_(computer_science)).

If the match is a fact, the two terms are unified
but no other action is performed.
If the match is a rule, the left-hand side is unified with the query term
and all the terms in the right-hand side are evaluated recursively as the next query.

If no definition in the knowledge base matches,
the built-in rules are attempted.

### Built-in rules

#### Arithmetic

```
?> 1 + 2 = X.
X = 3.

?> 1 + X = 3.
X = 2.

?> 1 + X = 3, X + Y = 10, 32 = X + Y + Z.
X = 2.
Y = 8.
Z = 22.
```

#### IO

```
?> print (hello world).
hello world

?> print (what is your name?), getSymbol Name, print (hello Name).
what is your name?
john 
hello john
Name = john.

?> print (how old are you?), getInt Age, Year = 2021 - Age, print (you were born in Year).
how old are you?
23
you were born in 1998
Age = 23.
Year = 1998.
```

### Examples

There is a file `example.pt` in the `patatern` directory.
You can load it into the interpreter and run queries
to get more familiar with the language.
