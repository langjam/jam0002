# Array implementation

Muncher does not have builtin arrays. We can implement those on our own using
objects, and then create a for loop syntax to iterate over arrays.

We need an `if` first:

```
let emptyBlock = (object { .$b:block { return b }}).{};
let if = object {
    ($cond:expr) $then:block {
        cond.pick(then, emptyBlock).exec;
    }
    ($cond:expr) $then:block else $else:block {
        cond.pick(then, else).exec;
    }
    ($cond:expr) $then:block
    else if ($cond2:expr) $then2:block
    else $else:block {
        if (cond) {
            then.exec;
        } else {
            cond2.pick(then2, else).exec;
        }
    }
};
```

Arrays will be represented as binary trees. A node will store the middle element
in the range, and have two arrays for the left and right children.

An empty array will be a different object but with the same methods.

```
// array is a factory object that creates array objects
let array = object {
    ($len:expr) {
        // returning directly from if does not work!
        let arr = nil;
        if (len.eq(0)) {
            arr = object Array {
                .len { return len; }
                [$idx:expr] { return nil; }
                [$idx:expr] = $value:expr {}
            };
        } else {
            let value = nil;
            let middle = len.div(2);
            let left = array(middle);
            let right = array(len.sub(middle).sub(1));
            arr = object Array {
                .len { return len; }
                [$idx:expr] {
                    let result = nil;
                    if (idx.lt(middle)) {
                        result = left[idx];
                    } else if (idx.eq(middle)) {
                        result = value;
                    } else {
                        result = right[idx.sub(middle).sub(1)];
                    }
                    return result;
                }
                [$idx:expr] = $val:expr {
                    if (idx.lt(middle)) {
                        left[idx] = val;
                    } else if (idx.eq(middle)) {
                        value = val;
                    } else {
                        right[idx.sub(middle).sub(1)] = val;
                    }
                }
            };
        }
        return arr;
    }
};
```

And let's make a helper for printing:

```
let println = object {
    () { print("\n"); }
    ($a:expr) { print(a); println(); }
    ($a:expr, $b:expr) { print(a); println(b); }
}
```

Now we can have array operations:

```
let testArray = array(5);
println(testArray);
// prints: <Object Array>
println(testArray[4]);
// prints: nil
testArray[3] = 42;
println(testArray[3]);
// prints: 42
```

And let's make a `for` loop. It will have two flavours:

1. Iterating over a range of integers. We will need use as an implementation
base for the second case.
2. Iterating over arrays.

```
let for = object {
    ($counter:ident from $start:expr to $end:expr) $body:block {
        let iterate = object {
            ($iter:expr) {
                if (iter.lt(end)) {
                    body.def(counter, iter).exec;
                    iterate(iter.add(1));
                }
            }
        };
        iterate(start);
    }
    ($item:ident in $array:expr) $body:block {
        for (i from 0 to array.len) {
            body.def(item, array[i]).exec;
        }
    }
};
```

And now we can iterate over arrays:

```
let arr = array(3);
arr[0] = 42;
arr[1] = "test";
arr[2] = false;
for (item in arr) {
    println("array item: ", item);
}
// prints: array item: 42
// prints: array item: test
// prints: array item: false
```
