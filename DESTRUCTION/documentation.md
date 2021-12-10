# DESTRUCTION

A lot of modern programming language use _destructuring patterns_. This is the concept of extracting values from a datastructure by making an outline of the datastructure and marking where the desired values would be, as seen in this example in rust:

```rs
let (a, 2, [c, d]) = (1, 2, [3, 4]);
assert_eq!(a, 1);
assert_eq!(c, 3);
assert_eq!(d, 4);
```

Our language, **DESTRUCTION**, explores how this feature can be expanded, and it also explores how destructuring patterns could be a core mechanic of a language.

## Transformations

In a DESTRUCTION program, the state of the program is always a single value. At the start of the program this value is the command line input, and at the end this value will be outputted to the console. The program itself consists of a series of _transformations_ that transform the value to the desired output. A transformation has two parts:

- A destructuring pattern, that extracts desired values from the current state of the program
- A constructing pattern, that uses the extracted values from the first part to make a new datastructure

A transformation is denoted with a `->` arrow, with the destructuring pattern to the left, and the constructing pattern to the right.
Here is an example of a transformation that reverses an array of 3 elements:

```js
[a, b, c] -> [c, b, a]
```

You can chain multiple tranformations together with a `|` symbol, like this:

```js
// reverses the array and attaches the elements 2 and 3 to the end
[a, b, c] -> [c, b, a] | arr -> arr + [2, 3]
```

> If the current value does not fit the given destructuring pattern, the interpreter will stop and print an error

## Symmetry

In DESTRUCTION, a transformation is always _symmetrical_. This means that:

- all possible constructing patterns can also be destructuring patterns, and vice versa
- any tranformation where the left side is equal to the right side will do nothing

From now on, it's useful to think about constructing patterns as functions that take a number of inputs and outputs the constructed datastructure, and destructuring patterns as a way to _run such a function in reverse_, aka. use the output value to determine the input values. With this expanded definition we gain a lot of new abilities, for example, we can now also destructure **mathematical operations**:

```js
// input is 60
n * 10 -> n
// `n` is 6, because 6 * 10 = 60
```

In DESTRUCTION, you can also use the operators `+` and `*` on strings and arrays. This can also be done in a destructuring pattern to get subsets of an array or a string:

```js
// input is "hello world!"
"hello " + greeted + "!" -> greeted
// `greeted` is "world", because "hello " + "world" + "!" is equal to the input
```

> To understand this better, you can imagine how the transformation would not change the value if the right side was the same expression as the left, for example `n * 10 -> n * 10` would first extract `n` as 6, and then construct it back to 60 on the right side

This is especially useful for operations that mean the opposite of eachother. For example, in DESTRUCTION, you can split a string with a separator with `/`, like this:

```js
// input is "hello world!"
a -> a / " "
// output is ["hello", "world!"]
```

By using this feature in the destructuring pattern, you can get a `string.join` feature for free!

```
// input is ["hello", "world!"]
a / " " -> a
// output is "hello world!"
```

## Conditional transformations

A _conditional transformation_ consists of two sub-transformations. It will first try to transform the current value using the first transformation, but if the value doesn't fit the first transformation's destructuring pattern, it will run the other transformation instead.

The syntax for a _conditional transformation_ looks like this:

```js
? first_transformation : other_transformation
```

Conditional transformations are just as much a transformation as normal ones, and you can therefore chain them like this:

```js
?  trans1
:? trans2
:? trans3
...
:  trans4
```

## Functions

An actual DESTRUCTION file is a collection of functions, where the `main` function is the entry point. You can define functions with this syntax:

```js
my_function := transformations;
```

A function takes a single input, and gives a single output. You can use functions in constructing patterns like this:

```js
shout := str -> str + "!!!";
// syntax: `function_name input_value`
main := input -> shout input;
// input is "hello", output is "hello!!!"
```

You can also use functions in the destructuring pattern. This will effectively run the function in reverse, using the current value as the expected output, and figuring out the input from that.

```js
shout := str -> str + "!!!";
// syntax: `function_name input_value`
main := shout output -> output;
// input is "hello!!!", output is "hello"
```

The interpreter does this by literally running each transformation in the function in reverse, using the destructuring pattern as the constructing pattern and vice versa. This will of course not work for all functions, since some functions lose information from the input in the output. For example:

```js
add_nums := (a, b) -> a + b;
test1 :=
    _ -> 10 // sets the current value to 10, regardless of what it was before (`_` matches any value)
    | add_nums (a, b) -> a; // ERROR: Cannot destruct expression with two unknowns
    // it throws this error because it tried to destructure the construction pattern in `add_nums` (`a + b`),
    // which is not possible in this case

test2 := _ -> add_nums (10, 10); // returns 20
```

## Polyvalues

Sometimes it's useful to store a variable number of values in a single variable. In DESTRUCTION, you can do this with _polyvalues_.
A polyvalue is a variable that can be destructed to multiple different values in the destructuring pattern, and be constructed back to the corresponding values in the constructing pattern.
A polyvalue is denoted with a `*` symbol, and is followed by an identifier name.

Example:

```js
// takes an array with a variable number of elements
// returns an array with each element doubled
double_elements :=
    [*a] * n -> [*a * 2] * n;
```

## Reference

### Datatypes / Datastructures

| name    | syntax                    | example                                   |
| ------- | ------------------------- | ----------------------------------------- |
| number  |                           | `100`, `-10`, `3.14159`                   |
| string  | `"[content]"`             | `"Hello"`                                 |
| boolean |                           | `true`, `false`                           |
| array   | `[element, element, ...]` | `[1, 2, 3]`, `[["hello"], 1, 2, []]`      |
| tuple   | `(element, element, ...)` | `(1, 2, 3)`, `(("hello", "world"), 1, 2)` |

### Operators

| symbol           | syntax            | datatypes                                                                                                | description                                                                                                   | example                                                                          |
| ---------------- | ----------------- | -------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------- | -------------------------------------------------------------------------------- |
| `-`              | `a - b`           | `number - number` (returns number)                                                                       | Subtracts the right value from the left value                                                                 | `10 - 3 // 7`                                                                    |
| `-`              | `-a`              | `-number` (returns number)                                                                               | Negates the value                                                                                             | `-10 // -10`                                                                     |
| `+`              | `a + b`           | `number + number` (returns number), `string + string` (returns string), `array + array` (returns array)  | Adds/attaches the right value to the left value                                                               | `10 + 3 // 13`, `"a" + "b" // "ab"`, `[1, 2] + [3, 4] // [1, 2, 3, 4]`           |
| `*`              | `a * b`           | `number * number` (returns number), `string * number` (returns string), `array * number` (returns array) | Multiplies/repeats left value by the right value left value by the right value                                | `10 * 3 // 30`, `"ab" * 3 // "ababab"`, `[1, 2] * 3 // [1, 2, 1, 2, 1, 2]`       |
| `/`              | `a / b`           | `number / number` (returns number) `string / string` (returns array)                                     | Divides the left number by the the right number, or splits the left string with the right string as separator | `10 / 2 // 5`, `"a, b, c" / ", " // ["a", "b", "c"]`                             |
| `&&`             | `a && b`          | `boolean && boolean` (returns boolean)                                                                   | Logical AND                                                                                                   | `true && true // true`, `true && false // false`, `false && false // false`      |
| `\|\|`           | `a \|\| b`        | `boolean \|\ boolean` (returns boolean)                                                                  | Logical OR                                                                                                    | `true \|\| true // true`, `true \|\| false // true`, `false \|\| false // false` |
| `==`             | `a == b`          | `any == any` (returns boolean)                                                                           | Checks if the left value is equal to the right value                                                          | `10 == 10 // true`, `10 == "10" // false`, `[1, 2] == [1, 2] // true`            |
| `!=`             | `a != b`          | `any != any` (returns boolean)                                                                           | Checks if the left value is not equal to the right value                                                      | `10 != 10 // false`, `10 != "10" // true`, `[1, 2] != [1, 2] // false`           |
| `<`              | `a < b`           | `number < number` (returns boolean)                                                                      | Checks if the left value is less than the right value                                                         | `10 < 20 // true`, `10 < 10 // false`, `10 < -10 // false`                       |
| `>`              | `a > b`           | `number > number` (returns boolean)                                                                      | Checks if the left value is greater than the right value                                                      | `10 > 20 // false`, `10 > 10 // false`, `10 > -10 // true`                       |
| `<=`             | `a <= b`          | `number <= number` (returns boolean)                                                                     | Checks if the left value is less than or equal to the right value                                             | `10 <= 20 // true`, `10 <= 10 // true`, `10 <= -10 // false`                     |
| `>=`             | `a >= b`          | `number >= number` (returns boolean)                                                                     | Checks if the left value is greater than or equal to the right value                                          | `10 >= 20 // false`, `10 >= 10 // true`, `10 >= -10 // true`                     |
| `!`              | `!a`              | `!boolean` (returns boolean)                                                                             | Logical NOT                                                                                                   | `!true // false`, `!false // true`                                               |
| `::#type~>#type` | `v::#type~>#type` | `any1::#any1~>#any2` (returns any2)                                                                      | Converts a value from one datatype to another                                                                 | `10::number~>string // "10"`, `"10"::string~>number // 10`                       |

> DESTRUCTION has no builtin functions, and no standard library.
