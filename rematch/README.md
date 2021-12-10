# Rematch

[Pattern-matching](https://en.wikipedia.org/wiki/Pattern_matching) is a an extremely powerful programming concept allowing one to reduce complex algorithms down to systematic case analysis. Programming languages such as OCaml or Haskell highlights the beauty of this approach by providing syntactic constructs to define (recursive) fonctions by case analysis.

**Rematch** is a tiny functional programming language designed in [Rust](https://www.rust-lang.org/) and whose only feature is pattern-matching. It is mainly an experiment to stress test the expressiveness of patterns in a programming language.

## Compiling the project

To compile the projet, all you need is an [up-to-date rust installation](https://www.rust-lang.org/learn/get-started).

The following command should start a **Rematch** repl:
```
cargo run
```

To run the unit tests:
```
cargo test
```

## Syntax

### Primitive data types

As in any programming language, **Rematch** comes with its own primitive data types.
There are only 2 base types in **Rematch**: natural numbers and named records.

#### Natural numbers

Any sequence of digits is a natural number:
```
42
2048
255
```
#### Named records

Named records are generic data structures that one can use to do anything from list to trees to graphs (or even more complex structures).
A named record is simply a name (starting with a capital case) followed by a sequence of comma-separated values.

```
Triple(1, 2, 3)
```

List can be simulated:
```
Cons(1, Cons(2, Cons(Nil)))
```

#### Primitive operations

There is no primitive operations on natural numbers nor on named records in **Rematch** ! However, one can use infix operators as shortcuts to introduce common named records.

+ `x + y` is an alias for `Add(x, y)`
+ `x - y` is an alias for `Sub(x, y)`
+ `x * y` is an alias for `Mul(x, y)`

### Matching

#### Standard case analysis

As explained above, matching is all you can do in **Rematch**. Matchings can be used to look at the structure of any value. The general syntax for matching is as follows:

```
case ... of
  | ... match ...
  | ... match ...
  | ... match ...
```

For instance, the program `case 42 of | x match x` outputs `42`.

Matchings can be used to analyse the structure of named records:

```
case Cons(42, Cons(41, ...)) of
  | Cons(x, y) match x
```

The code above outputs `42`.

#### Rematching

The true power of rematch is its ability to rerun the same pattern matching.
This behavior is permitted by the `rematch` construct:

```
case input of
  | pattern1 match output
  | pattern2 rematch next_input
```

If `input` matches `pattern2` then `next_input` will be evaluated and then the whole pattern matching will be re-executed with the result as a new input.