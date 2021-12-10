# likelang

The Like programming language (likelang) is a *dynamically typed*, *function-first language* that allows you to import functions to a namespace based on a pattern.

An example program in likelang ([source](./examples/fibo.like)):

![A Fibonacci and Factorial example in likelang](./img/fibo.png)

### Features

 - Dynamically typed variables
 - if-else control statements
    - who needs loops anyway
 - Functions!
    - Also dynamically typed
    - Can be *nested* - functions defined within other functions
    - *First class* members - can be assigned to a variable
    - Supports *recursion*
 - *The Collect pattern*
    - In likelang, you can collect various functions using a pattern, into a namespace

### Quick Example

```
let tests = collect /*test_util/
```

Here we are collecting all functions that end in `test_util` and assigning it to a "namespace" called `tests`.

Using this we can invoke functions that end with test.

For example if there is a function called `like_test_util` we would invoke it as:

```
like_.tests()
```

Similarly for prefixes:
```
fn testsomething() {
}

let tests = collect /test*/

test.something()
```

## Installation

#### Using pip

The recommended way to install dependencies is
```bash
python3 -m venv env
source env/bin/activate
pip3 install -r requirements.txt
```

#### Using nix

This method is only possible *nix (MacOS, Linux, WSL, etc) systems.
Install nix from [here](https://nixos.org/download.html).
Now let it install the dependencies.

```bash
nix-shell
```

## How to run

Once you have the dependencies just run it as follows. Here we run an example test file.

```bash
python3 like.py examples/fibo.like
```

## Example

Find more examples of likelang in [examples](./examples/).

## Syntax highlighting

Currently only supported in vim and neovim.

Open a `.like` file and use `:source extra/like.vim`.

## License

MIT
