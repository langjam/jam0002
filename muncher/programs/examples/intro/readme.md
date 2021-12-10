# Intro to muncher language

> This readme file is a single program written in a literate style - you can
> obtain then program by concatenating all code blocks in here, or by just
> looking at [program.mnc](./program.mnc).

Muncher is a dynamically typed, "object oriented", imperative programming
language.

There are only a few interesting syntactic forms in the language.

1. Builtin primitives nil, numbers, booleans, and strings are written as usual:
`nil`, `12345`, `true`, `"Hello, world"`.

2. `let` statements define variables:

    ```
    let a = "Hello, world";
    ```

3. Object expressions define objects (that you can bind to variables for use
later):

    ```
    let foo = object {
        .bar() {
            // print is a builtin
            print("called bar!\n");
        }
    };
    ```

4. Call expressions invoke methods on objects:

    ```
    foo.bar();
    // prints: called bar!
    ```

Now here's the interesting part: on `foo` object we defined the method to be
invokable as `.bar()`, which is a common method syntax in many languages. But
here we don't have rigid constraints on how the syntax needs to look like! We
can define (almost) whatever we want:

```
let fancy = object {
    . fly me to the moon! {
        print("let me play among the stars\n");
    }
    (++/!@#?/1)%?!3% {
        print("what an abomination\n");
    }
};
```

Which we can then invoke using the syntax we defined:

```
fancy.fly me to the moon!;
// prints: let me play among the stars

fancy(++/!@#?/1)%?!3%;
// prints: what an abomination
```

There are only a 3 requirements for defining a method:

1. Call syntax must begin with either a `.`, `(`, or a `[`.
2. Parentheses and curly braces must be balanced (in fact they must be balanced
in the whole source file).
3. A single object's method dispatch must be decidable without lookahead (we'll
see more about that in a bit).

However, just matching plain tokens if no interest - we also want to be able to
pass arguments to methods. To achieve that we can use "munchers" in method
definition to mark what kind of syntax the object consumes. The syntax for using
those is `$binding:muncherType`. Here's an example:

```
let o = object {
    .callMe($arg:expr) {
        print("called me with a ");
        print(arg);
        print("\n");
    }
};
```

Now the method `callMe` will consume an expression as an argument, and the value
of that expression will be bound to `arg` when executing the body of the method:

```
o.callMe("particular string");
// prints: called me with a particular string
```

Note that the expression itself can be a call to some object with whatever
syntax that object defined:

```
let oh = object {
    .my what a syntax! {
        return "fancy syntax";
    }
};

o.callMe(oh.my what a syntax!);
// prints: called me with a fancy syntax
```

> Small detour: now that we now how take parameters let's define a little
> helper object so that we would not need to write "\n" everywhere:
> ```
> let println = object {
>     ($a:expr) {
>         print(a);
>         print("\n");
>     }
>     ($a:expr, $b:expr) {
>         print(a);
>         print(b);
>         print("\n");
>     }
> };

Currently there are 3 types of munchers:

1. `expr` consumes an expression and gives the value that the expression
evaluated to.
2. `ident` consumes a single identifier and returns an object representing that
identifier.
3. `block` consumes tokens delimited by a pair of curly braces and returns a
block object representing that code. The block also captures the environment
where it was written (so lexical scoping is maintained). The block object can be
executed by calling `.exec` on it.

Munchers are the reason for the "methods must be parsable without lookahead"
rule. If one method is defined to be `.(a)!` and another is `.$e:expr?`,
then only once we see a `!` or a `?` we can know whether `a` was supposed to be
a plain token, or an expression (which would have required `a` to be a defined
variable). So instead you get an error that methods are ambiguous when you try
to create an object. However, in some cases ambiguity is allowed and interpreter
will try to pick the more specific option if possible.

Let's play around with it! The language does not even have a built-in `if`
statement, but we can define one ourselves:

```
let if = object {
    ($cond:expr) $then:block else $else:block {
        // booleans have one method on them - pick
        // it takes two values, with `true` returning the first one
        // and `false` returning the second
        cond.pick(then, else).exec;
    }
};

// .eq(_) is one of the builtin methods on strings
// other available methods are .len, .concat(_), and .substr(start, end)
if ("test".eq("test")) {
    println("test is equal to test");
} else {
    println("our string operations are broken");
}
// prints: test is equal to test

if ("hello world".eq("hola mundo")) {
    println("our string operations are broken");
} else {
    println("both branches of `if` work!");
}
// prints: both branches of `if` work!
```

This `if` object is not too fancy, as we can't write a plain `if (cond) {}` - we
only defined the version that needs an `else` branch. Defining a better `if` is
not very straightforward because we can't implement it as `cond.pick(then, {})`.
`pick` is defined to take two *expressions* that evaluate to blocks. `{}` by
itself is not even a valid expression. So first we need to somehow obtain a
block object that would be bound to a variable, and then we could use that
variable as an argument to pick. We can do that with a helper object that
captures a block and returns it as a value:

```
let helper = object {
    ($b:block) {
        return b;
    }
};
let emptyBlock = helper({});
```

If you don't understand what just happened then don't worry, just continue
reading. The point is that now we can have a more useful `if` where `else`
branch is optional:

```
if = object {
    ($cond:expr) $then:block {
        cond.pick(then, emptyBlock).exec;
    }
    ($cond:expr) $then:block else $else:block {
        cond.pick(then, else).exec;
    }
};
```

Another control flow structure we could do is a `for` loop. The `for` object
would take a block (just like `if`), and the it would execute it repeatedly. It
also needs to update the counter - for that block objects allow defining a new
variable in its captured scope using `.def(ident, value)`:

```
let for = object {
    ($counter:ident from $start:expr to $end:expr) $body:block {
        // internally the loop is done through recursion, we create a helper
        // object for that
        // note that the object captures variables from outer scope, so we don't
        // need to pass counter, end, and body again
        let iterate = object {
            ($iter:expr) {
                // .lt is builtin method on ints
                if (iter.lt(end)) {
                    // .def takes an identifier object and a value,
                    // and returns a new block with extended environment
                    body.def(counter, iter).exec;
                    iterate(iter.add(1));
                }
            }
        };
        iterate(start);
    }
};

for (i from 0 to 5) {
    println("i = ", i);
}
// prints: i = 0
// prints: i = 1
// prints: i = 2
// prints: i = 3
// prints: i = 4
```

And for our last trick let's make ourselves some lambda functions. A lambda
"keyword" takes a block as a parameter and returns an object that executes the
said block when invoked:

```
let lambda = object {
    ($param:ident) $body:block {
        return object {
            ($arg:expr) {
                // if a block executes a return statement,
                // then that value is returned from exec
                return body.def(param, arg).exec;
            }
        };
    }
};

let increment = lambda(x) { return x.add(1); };
println(increment(14));
// prints: 15

let callMe = lambda(callback) { callback(42); };
callMe(lambda(x) {
    println("I got a ", x);
});
// prints: I got a 42
```
