# Advent of Code 2021 day 7 solution

For starters - an array implementation (take a look at
[array example](../array) for an explanation how it works) together with some
other utilities.

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

let println = object {
    () { print("\n"); }
    ($a:expr) { print(a); println(); }
    ($a:expr, $b:expr) { print(a); println(b); }
}

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

Then, a growable list. It has all of the array methods so we will be able to use
the for loop with it.

```
let list = object {
    () {
        let len = 0;
        let buffer = array(1);
        return object List {
            .len { return len; }
            [$idx:expr] { return buffer[idx]; }
            [$idx:expr] = $val:expr { buffer[idx] = val; }
            .add($elem:expr) {
                if (len.eq(buffer.len)) {
                    let newBuffer = array(buffer.len.mul(2));
                    for (i from 0 to len) {
                        newBuffer[i] = buffer[i];
                    }
                    buffer = newBuffer;
                }
                buffer[len] = elem;
                len = len.add(1);
            }
        };
    }
};
```

Also lambdas for convenience. Multi-parameter lambdas too!

```
let lambda = object {
    ($param:ident) $body:block {
        return object {
            ($arg:expr) {
                return body.def(param, arg).exec;
            }
        };
    }
    ($param1:ident, $param2:ident) $body:block {
        return object {
            ($arg1:expr, $arg2:expr) {
                return body.def(param1, arg1).def(param2, arg2).exec;
            }
        };
    }
    ($param1:ident, $param2:ident, $param3:ident) $body:block {
        return object {
            ($arg1:expr, $arg2:expr, $arg3:expr) {
                return body
                    .def(param1, arg1)
                    .def(param2, arg2)
                    .def(param3, arg3)
                    .exec;
            }
        };
    }
    ($param1:ident, $param2:ident, $param3:ident, $param4:ident) $body:block {
        return object {
            ($arg1:expr, $arg2:expr, $arg3:expr, $arg4:expr) {
                return body
                    .def(param1, arg1)
                    .def(param2, arg2)
                    .def(param3, arg3)
                    .def(param4, arg4)
                    .exec;
            }
        };
    }
};
```

Now, we'll need some stuff for string processing. Let's start with a string
split. The only string builtins we have are `len`, `substr`, `concat`, and `eq`.

```
let split = lambda(str, sep) {
    let result = list();
    let start = 0;
    for (i from 0 to str.len) {
        if (str.substr(i, i.add(1)).eq(sep)) {
            result.add(str.substr(start, i));
            start = i.add(1);
        }
    }
    result.add(str.substr(start, str.len));
    return result;
};
```

Then, number parsing. We will use an adapter to give string array methods so
that we can use for loops to iterate over characters.

```
let parseDigit = lambda(x) {
    let digit = nil;
    if (x.eq("0")) { digit = 0; }
    if (x.eq("1")) { digit = 1; }
    if (x.eq("2")) { digit = 2; }
    if (x.eq("3")) { digit = 3; }
    if (x.eq("4")) { digit = 4; }
    if (x.eq("5")) { digit = 5; }
    if (x.eq("6")) { digit = 6; }
    if (x.eq("7")) { digit = 7; }
    if (x.eq("8")) { digit = 8; }
    if (x.eq("9")) { digit = 9; }
    return digit;
};

let chars = lambda(str) {
    return object {
        .len { return str.len; }
        [$idx:expr] { return str.substr(idx, idx.add(1)); }
    };
};

let parseInt = lambda(str) {
    let result = 0;
    for (char in chars(str)) {
        result = result.mul(10).add(parseDigit(char));
    }
    return result;
};
```

Now we can finally start working on the actual task! First, input parser:

```
let parseInput = lambda(input) {
    let parts = split(input, ",");
    let numbers = list();
    for (part in parts) {
        numbers.add(parseInt(part));
    }
    return numbers;
};
```

Part 1 is just taking the difference between individual positions and the
median:

```
let abs = lambda(x) {
    if (x.lt(0)) {
        x = 0.sub(x);
    }
    return x;
};

let solvePart1 = lambda(positions) {
    let median = positions[positions.len.div(2)];
    let total = 0;
    for (pos in positions) {
        total = total.add(abs(pos.sub(median)));
    }
    return total;
};
```

For part 2 we'll do a simple quadratic solution. Interpreter is too slow to run
this on real input though.

```
let solvePart2 = lambda(positions) {
    let min = positions[0];
    let max = positions[0];
    for (pos in positions) {
        if (pos.lt(min)) { min = pos; }
        if (pos.gt(max)) { max = pos; }
    }
    let best = 999999999;
    for (pos from min to max.add(1)) {
        let total = 0;
        for (crab in positions) {
            let diff = abs(pos.sub(crab));
            let cost = diff.mul(diff.add(1)).div(2);
            total = total.add(cost);
        }
        if (total.lt(best)) {
            best = total;
        }
    }
    return best;
};
```

Let's try with the example:

```
let input = "16,1,2,0,4,2,7,1,2,14";
let positions = parseInput(input);
println("part 1: ", solvePart1(positions));
// prints: part 1: 37
println("part 2: ", solvePart2(positions));
// prints: part 2: 168
```
