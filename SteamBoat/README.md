# SteamBoat
A language for the Analytical age.

While going through my great-grandfather's things I uncovered the curious document you are about to read. It details a an organizational system for shipping that in some ways resembles a programming language. It is doubtful such an impractical system was ever put into use, it features 1920s analogs of pattern matching, stack semantics, ring buffers, and colonialism. I went to the trouble of writing an interpreter for this strange language and a few simple programs. 

You can run the interpreter with Python:

```
python steamboat.py examples/ahoy.sb
```

This command should print `Ahoy There!` to the console.

While I tried to maintain the accuracy of the document much was missing and had to be inferred and I have slightly modernized the explanation in places. I also changed it to American English so my spellchecker would stop yelling at me. Enjoy!

-R.M.

## Intro
In the year of our Lord 1921 the complexities of intercontinental shipping threaten to overwhelm the logistical departments of even modest companies. We propose the following notation to be used to coordinate the shipping and perhaps even operations in His Majesty King George V's navy.

## Example
In this example we show how a vessel can be ordered to Java, collect cargo represented by letters, and return them to England for display.
> For an example of this program with more comments consult the "simple.sb" file from the examples folder - R.M.
```
island England
port London
lock []
canal goto Jakarta, Java

port Bristol
lock _[]
canal out goto Bristol
lock []
canal collate halt

island Java
port Jakarta
lock []
canal
chars "Ahoy There!"
goto Bristol, England
```

## Terminology
`island [name]`: A landmass with various "locks" grouped into ports.

Islands posses a store of objects organized in a ring buffer. Islands also have a register that points at an address in the ring buffer. This register can be set and read from.

`port [name]`: A marker a ship can navigate to on an island

`lock [pattern]`: An island's canals can be accessed by locks. For a ship to gain entry to a lock it must match the lock's gates. A ship will proceed from lock to lock until it can pass the lock's gate. It will enter the first lock it qualifies for.

`lock [_ or number or character]`: A lock must have one or more gates. These determine whether the ship can enter based on the ship's cargo (the state of its stack).

`lock [string or array]`: Regular gates only operate on one item in the ship's cargo at a time. Gate groups are a way of matching several items of cargo at a time.

Locks are shared between islands. All locks on an island are numbered from `0`. If a lock is not matched the ship will proceed to the next lock on the island, looping to the start if needed. A ship can leave a port implicitly as it goes from lock to lock.

`canal [*station]`: Every lock connects to a canal which features one or more stations. These stations perform one of several instructions on the ship as it proceeds. If any of those cannot be accomplished the ship simply sails out of the canal ignoring every subsequent station.

`station`: When a ship sails through a canal it will pass by at least one station. Most of these stations will perform some operation on the ship's cargo. For example `dupe` will copy the item at the top of the ships stack and add it to the stack.

`ship`: In this version of SteamBoat only one ship is supported. The ship begins her trip at the first lock in England. The main job of the ship is to ferry around cargo, data stored in a stack structure. Most of the canal stations operate on this stack.

In addition the ship also has fields to keep track of her current island, destination island, and destination port. The latter two are set by `goto` commands as explained below and shown by example.

Finally the ship has a log that is used to record errors.

## Patterns

`.`: an empty stack.

`_`: a single wildcard character.

`n`: a number literal e.g. `0`, `1001`, `-20`.

`'char'`: an ascii character literal e.g. `'c'`, `'A'`, `'!'`.

`"string"`: several ascii characters e.g. `"Ahoy!"`.

`[]`: a stack of any size including empty. A wildcard pattern. It must appear at the end of a pattern because it will consume all remaining elements.

`[_, cond]`: a stack of any size where every element must satisfy the condition `cond`. It will continue to match elements while it can.

`[n, cond]`: a stack of fixed size `n` where every element must satisfy the condition `cond`. 

Patterns can be chained together. For example `'A'24[]` will match a ship whos stack begins with an `A` (a `33` internally) followed by a `24` followed by any number of other elements.

A pattern making use of SteamBoats's string and length specifiers might be the following: `"Ahoy "[5,_]!` which would match steamers containing `Ahoy matey!` and `Ahoy Phill!` but not `Ahoy Barbera!` or the rather more excited `Ahoy matey!!!`.

An example of a pattern that allows an unlimited amount of enthusiasm might be `"Ahoy "[5,_][_,'!']`.

## Canals and Stations

Once a ship has been granted access it proceeds down a canal. Each canal will have one or more stations.

If an error is made at any of these stations like trying to add with fewer than two items a note is made of it in the ship's log and all further stations in that canal are ignored.

### Stack Manipulation
`lit [arg]`: Push one or more literal values (a number, a character, a string, or even an array) to the ship's cargo.

`in`: Input an item from the island's buffer. The island's pointer will be decremented *and then* the item at that position is added to the cargo. Note that the item is only copied, it is not reset to zero.

`out`: The top element of the ship's cargo is popped and copied to the island's buffer at the position of the island's pointer. The pointer is then incremented.

`dump`: Output the ship's entire cargo to the island's buffer, as if out was run that many times.

`dupe`: Duplicate and push the top element of the cargo.

`del`: Pop the top element of the cargo and toss it overboard.

`swp`: Swap the positions of the top two elements in the cargo.

`rev`: Reverse the order of the items in the ship's cargo.

### Arithmetic

`inc`: Increments the value of the top element.

`dec`: Decrements the value of the top element.

`neg`: Negates the value of the top element.

`add`: Pops the top two elements, adds them, and pushes the result to cargo.

`sub`: Pops the top two elements, subtracts them, and pushes the result to cargo. Subtracts the *first* item popped from the *second.

`mul`: Pops the top two elements, multiplies them, and pushes the result to cargo.

`div`: Pops the top two elements, divides them, and pushes the result to cargo. Divides the *first* item popped by the *second.

`mod`: Pops the top two elements, finds the modulo, and pushes the result to cargo. Calculates the modulo of the *first* item popped from the *second.

### Assertations

Assertations require a little more explanation. Each is a conditional that checks the top element of the cargo. If that condition is true the ship proceeds from that station as normal. If it is false the ship bails out of the canal, ignoring every station after this one.

`aez`: Assert equals zero.

`alz`: Assert less than zero.

`agz`: Assert greater than zero.

`anz`: Assert not zero.

### Misc.

`goto [Port, Island]` or `goto [Port]`: This sets the destination of the ship. Note that unlike other languages `goto` does not happen instantly. The rest of the stations in the canal are still visited. This means that if another `goto` command is reached it resets the command. If you try to go to a port or island that does not exist the error will be reported when the ship sets course.

`status`: Report the ship's status including its cargo, current island, the current lock number.

`survey`: Report the status of the island including its buffer and pointer values.

`collate`: Print the numbers in the island's buffer as characters.

`halt`: Stop the ship entirely. Halts the program.

`set`: Pop the top item and set the island's pointer to that value.

`read`: Read the current island's pointer and push it to the cargo.

`inp`: Await keyboard input and push the characters to the ship's cargo. Perhaps designed for telegraphs.