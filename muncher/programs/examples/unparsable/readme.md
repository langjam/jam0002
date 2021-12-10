# Unparsable program

An little example of how compilation-hostile this language is. Method
invocations cannot be parsed without knowing the type on an object, so code must
be parsed at execution time.

As always, here's a println:

```
let println = object {
    ($e:expr) {
        print(e);
        print("\n");
    }
};
```

Now, the fun bit. An object that will call the provided callback with a local:

```
let call = object {
    ($callback:expr) {
        let a = "local";
        callback(a);
    }
};
```

If we give an object that takes an actual parameter, then we will get the local:

```
call(object {
    ($x:expr) {
        println(x);
    }
});
// prints: local
```

And if we give an object that just eats the specific sequence of tokens, then
`a` in the call site will be a plain token insted of an expression. In fact `a`
does not even need to be defined!

```
call(object {
    (a) {
        println("(a)");
    }
});
// prints: (a)
```

That example did not look too bad, but we can make even more contrived one:

```
let callWithField = object {
    ($obj:expr, $callback:expr) {
        callback(obj.foo);
    }
};
```

Everything is fine with a nice object:

```
callWithField(
    object { .foo { return "accessed .foo"; } },
    object { ($e:expr) { println(e); } }
);
// prints: accessed .foo
```

But `obj.foo` does not need to be a call to `obj` if we provide an appropriate
object. Scoping rules allow resolving to a global declared later (so that we
could have mutually recursive global functions), so we can make `foo` bit in
`callWithField` to resolve to a global.

```
let foo = "I am a global foo";
callWithField(
    object { .no foo field { return "nope" } },
    object { (obj.$e:expr) { println(e); } }
);
// prints: I am a global foo
```
