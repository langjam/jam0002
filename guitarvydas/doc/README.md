# Theme
Patterns
# Goals of This Project
Draw diagrams of some common concurrency patterns.  Transpile the diagrams to running C code.
# How to Run This Project
On Mac/Linux:

./run.bash

# Status
- Plan written
- Concurrency Pattern Diagrams drawn in draw.io (See below: Diagrams - Discussion) and exported as .SVG
- C-- transpiler unfinished, does some transpiling (.cmm files transpiled to -> .c files)
-  Code for very, very simple example written as 4 source files: 
	-  producer.cmm
	-  consumer.cmm
	-  os.cmm
	-  main.cmm
	-  types.h
	-  cos.h (Component O/S)
- The above code is in Alpha (POC) state.  The code is not fully tested (compile errors in os.c) due to time limitations, but should give the gist of what is intended / possible.
- PFR tool (built before JAM) - "Parsing Find Replace"
- D2F tool (built before JAM) - "Diagram to Factbase"
- F2J tool - "Factbase to JSON"

## Plan
see doc/Plan.md
## Diagrams - Discussion
Easier to "reason about" if I can see pictures (Graphic Spec (as opposed to Graphic Novel))
see doc/Diagrams.md
## Factbase
A Factbase is a set of triples.

In this project, a FB (factbase) is a set of triples in the syntax of PROLOG[^1] assertions.  This allows SWIPL[^2] to be used to query the factbase.
[^1]: The only reason I used PROLOG is that I already knew how to use PROLOG.  I believe that JavaScript, Python, C++, miniKanren, core.logic, etc. would be compatible.
[^2]: SWIPL is SWI PROLOG
## This Project Uses Existing Technologies
### Ohm-JS
npm install ohm-js@next

# C-- 
 C (and most languages, including C++, Haskell, Python, etc.) are dual purpose[^3]:
1. describe implementation details of data
2. describe implementation details of control flow

[^3]: In fact, these languages are designed to be GPLs (General purpose Programming Languages).  The idea of using GPLs is rooted in the biases (realities) of the 1950's, 1960's, 1970's.  What we *really* want is IDEs for building computer systems.  GPLs and O/Ss are IDE-wannabes.

C-- is dataless C.  
- Describe the control flow using only *handles* to data.  
- Use "super-macros" to hide details of data implementation.
- Macros implemented using Ohm-JS.

To make things painfully obvious, I've named ALL macros using "$" as the first character in the name.

# Software Components - LEGO® Blocks
all components are concurrent by default
very simple type for inter-component communication
Dispatcher() to run concurrent components
(build up more elaborate types in layers, instead of all-in-one flat design)

# Diagrams - DaS

We use text for programming languages based on the biases (realities) of the 1950s, 1960s, 1970s.

In 2020+, there is no reason that we can't *also* use diagrams as syntax.

DaS means "Diagrams as Syntax".

DaS should not be confused with VPL (Visual Programming).

DaS is a hybrid of *simple* diagrams and text.

The secret of using diagrams as syntax is to make all software components *concurrent* by default.

Some things are better left as text, e.g. `a = b + c;`.

Other things are better left as diagrams, e.g. network diagrams of LEGO®-like components.

# IDEs Instead of GPLs
# IDEs Instead of Operating Systems
# Backgrounders
## Triples
## Ohm-JS
DSL for building parsers.

Based on PEG technology.

IMO - best-of-breed PEG-based technology.
discord
github
## Diagrams
## Efficiency
## Discussion: Building Tools vs. Building Product
## Why Ohm-JS is the Best-of-Breed PEG Tool
### Transpilation
## Concurrency is Not Parallelism
Concurrency is a paradigm.
Parallelism is a tactic.
All Parallel programs are concurrent.
But, not all concurrent programs are parallel.

<< ref to Rob Pike video >>

