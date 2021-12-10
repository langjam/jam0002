# Agents
## Table of Contents
<!-- TOC -->

- [Agents](#agents)
	- [Table of Contents](#table-of-contents)
	- [Demo](#demo)
	- [General](#general)
	- [Running Locally](#running-locally)
		- [Dependencies](#dependencies)
	- [Ideas and Inspirations](#ideas-and-inspirations)
	- [Tools and Technologies Used](#tools-and-technologies-used)
	- [Next Steps](#next-steps)
	- [Example Program](#example-program)
		- [Spiral](#spiral)
	- [Referenced Links and Credits](#referenced-links-and-credits)

<!-- /TOC -->

## Demo
https://ncatelli.github.io/agents/


## General
Agents is a small basic-like language for describing the behavior of agents that travel around a board, painting a pattern in their wake. Our goal was to build a language that was both fun and simple to learn, in the same way as [LOGO](https://el.media.mit.edu/logo-foundation/index.html) or BASIC were to many of us as kids.

## Running Locally
The most up-to-date project lives at [ncatelli/agents](https://github.com/ncatelli/agents).

### Dependencies
- rust 1.56
- wasm-pack
- node v16.x.x

From the root of the project:

```bash
cargo install wasm-pack
wasm-pack build --release
cd www
npm install
npm start
```

The project will then be available at `http://127.0.0.1:8080/`

## Ideas and Inspirations
We initially cycled on the idea that we were going to build a language around manipulating a grid as we fixated on the concept that Conway's Game of Life allowed for complex patterns to emerge from a simple set of rules. We then aimed to make something fun and simple, both for our own enjoyment and to ensure that we could complete it within the time constraints of the competition. 

## Tools and Technologies Used
Our parser and interpreter are written in rust, using a handrolled parser combinator library.

Currently these are executed via a wasm-based frontedn which I will cover below.

Our frontend was built around ace and a thin JS wrapper that calls out to the runtime in wasm. This implementation HEAVILY inspired by [wasm_game_of_life](https://github.com/rustwasm/wasm_game_of_life/).


## Next Steps

- Implementing a way to define agent interactions
  - How an agent should behave if 
    - it collides with an agent
    - it encounters an already painted cell
- Extending the available operators
  - modulus
  - exponents
- Board resets
- Better compiler errors in the UI

## Example Program
### Spiral

```
# A program is composed of one or more independent agents.
# Agents move around the board in the direction they are oriented in on cycle
# of ticks, leaving behind a colored wake in their path.
#
# Agents are defined by by using the "agent" keyword followed by a name and
# semicolon.
agent red_agent:
    # Everything in a agent block is a statement.
    #
    # Statements can be labels or commands.
    # 
    # One such command is "set", used for defining variables.
    # Some variables are special and impact an agents placement or effect on
    # the board.
    # These include color, x and y for cell color or coordinates.
    set color = 0xFF0000
    set x = 40
    set y = 40
    # Other variables can be general purpose.
    set acc = 1
    set radius = 2
    # Commands control either agent behavior or control flow.
    #
    # face changes orientation of an agent and takes a cardinal direction, like
    # S, E or NW.
    face NW
    # Labels allow defining locations in code for a program to jump to
    loop:
        # Move takes a positive 32 bit integer and controls the number of steps
        # to move in a tick in this case our agent will move one space per tick
        # in the direction they are facing.
        move 1
        # Agent scripts support branching through the "jump to" command.
        # These commands look like:
        # "jump to <label> if <expression> is <expresion>".
        jump to spin if acc is radius
        # Variables also support basic arithmetic expressions.
        set acc = acc + 1
        # The goto command allows for the basic jumping to a label without a
        # condition.
        goto loop
    spin:
        # The turn command allows spinning an agent based on their current 
        # orientation. With a positive number being a clockwise rotation and a
        # negative being counter-clockwise.
        turn 1
        set acc = 1
        set radius = radius + 1
        goto loop
agent blue_agent:
    set color = 0xFF
    set x = 40
    set y = 40
    face NE
    loop:
        move 2
        goto loop
```

## Referenced Links and Credits

- Frontend and rendering code heavily referenced examples from [wasm_game_of_life](https://github.com/rustwasm/wasm_game_of_life/).
