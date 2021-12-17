# Patty - LISP with patterns [(vote)](https://github.com/langjam/jam0002/pull/13)

Created for [Lang Jam](https://github.com/langjam/jam0002). Theme is **patterns**, and this language is exploration of generating math patterns with lazy sequences. See [examples](examples/) to get gist of idea behind this language.

Language inspired by one line of Haskell code:

```haskell
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
```

In Patty it works like this:

```lisp
(do
	(def fibs (seq 1 1 (+ (index n fibs) (index (+ n 1) fibs))))
	(print (take 10 fibs)))
```

## Building

You need to make sure, that you have C++20 compiler, Make and [fmtlib](https://github.com/fmtlib/fmt).

To install them on Arch Linux:
```console
$ sudo pacman -S fmt gcc make
```

Then build with

```console
make patty
```

And run one of examples with
```console
$ ./patty examples/list.patty
```

Or enter interactive mode (REPL) with:
```console
$ ./patty
```

Since Patty interactive mode does not support readline functionality, usage of tools like [rlwrap](https://github.com/hanslub42/rlwrap) is recommended.

## Examples

See also [examples](examples/)

### Hello world

```lisp
(print "hello, world!")
```

### Truth machine

Defined at [esolangs.org/wiki/Truth-machine](https://esolangs.org/wiki/Truth-machine).

- Ask for input.
	- If the input is 0:
		- Output 0.
		- Terminate program.
	- Whereas if the input is 1:
		- Repeat infinitely:
			- Output 1.

```lisp
(if (= (read int) 0)
	(print 0)
	(loop (print 1)))
```

### Functional data operations

| Patty | Haskell |
| ----- | ------- |
| `(seq 5)` | `repeat 5` |
| `(seq 0 1)` | `cycle [0, 1]` |
| `(def repeat (fn (v) (seq! v)))` | `repeat n = n : repeat n` |
| `(zip-with + (list 1 2 3) (list 10 20 30))` | `zipWith (+) [1,2,3] [10,20,30]` |
| `(fold * (take 5 (seq (+ n 1))))` | `foldl1 (*) $ take 5 $ [1..]` |

## Documentation

Language is 0-indexed, LISP based with some funcitonal touches. Due to personal time limitations, it's not finished and it's far from exploring it's concept properly.

### Lists

- Collections of finitely many values
- Values stored in lists don't have to have same type
- Implemented with doubly-linked list, so many of the operations are O(n) (like size, getting value from specific index)

See [examples/list.patty](examples/list.patty)

### Sequences

- Lazy producers of values
- Can contain either normal or generative expressions. Generative expressions contain identifier `n` which constains index of currenlty produced value. This allows for construction of sequence of positive integers like `(def positive (seq (+ n 1)))`
- Normal expressions are first values of sequence (if sequence constains generative expressions) or are used to generate infinite sequence
	- As prefix: `(seq 10 9 n)` produces `(10 9 0 1 2 3 4 ...)`
	- As infinite sequence: `(seq 10 9)` produces `(10 9 10 9 10 9 10 9 ...)`

Due to lazy evaluation variables inside sequences are resolved when sequence is forced to produce values (for example in `take` function.). To resolve values when sequence is defined use `seq!`.

Currently, not all operations are supported on them. More time and effort is required.

See [examples/sequences.patty](examples/sequences.patty)

### Functions

- User-defined functions have bodies evaluated in scope where their are called.
- Arguments of C++ defined functions may be lazy (for example in `if`)
- User-defined functions are just lists

### Builtin functions


- Comparisons (only integers): `< <= > >=`, example: `(< 1 2)`
- Arithmetic (only integers): `+ - *`, example: `(+ 2 (* 4 3))`
- `do` - evaluate each argument and return last `(do (print 10) (+ 10 20))`
- `print` - prints provided arguments (without seperators), and then newline `(print "foo = " foo)`
- `def` - defines symbol to be this value. Used to define:
	- functions: `(def add2 (fn (n) (+ n 2)))`
	- other values: `(def foo 20)`
- `fun` - creates anonymous function with arguments given in first list and body in second

```lisp
(def count-down (fun (n)
	(if (!= n 0) (do
		(print n)
		(count-down (- n 1))))))
```

- `list` - creates list with content beeing arguments `(list 1 2 3 "foo")`
- `if` - if condition is true (non zero, not nil, non empty), evaluete first block, otherwise evaluate second if exists
- `++` - concat two list or add element to list `(++ (list 1 2 3) 4 (list 5 6 7))`
- `len` - get length of sequence, string or list `(len (list 1 2 3))`
- `index` - get nth value from sequence, string or list `(index 2 (list 1 2 3))`
- `for` - iterate over list `(for n (list 1 2 3) (print n))`
	- Additionaly `for` supports "list deconstruction". See [examples/list.patty](examples/list.patty)
- `zip` - zip several lists
- `zip-with` - zip with operation two or more lists, strings or sequences
- `take` - take n elements from sequence, list or string
- `fold` - fold list into value osing function `(fold * (list 1 2 3 4 5))`
- `loop` - eval provided block infinietly many times
- `seq` - construct sequence from arguments (precise definition above)
- `seq!` - construct sequence with arguments evaluated in current scope
- `pop` - remove value from sequence
