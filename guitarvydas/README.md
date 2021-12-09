# Theme
Patterns
# Goals of This Project
Draw diagrams of some common concurrency patterns.  

Transpile the diagrams to running C code.

Write a multitasking kernel in C, for event-driven components.

# How to Run This Project
On Mac/Linux:

./run.bash

# Contents
1. C-- transpiler (C minus minus - dataless C)
2. 5 Diagrams of various programming patterns (patterns.drawio)
3. tools - pfr, d2f, f2j (parsing find replace, diagram to facbtase, factbase to JSON, resp.)
4. run.bash
5. node_modules: Ohm-JS@next, atob, pako
6. POC .cmm code
7. README.md, doc/\*.md
8. DaisyChain.drawio - single diagram to be compiled (from diagrams above)
9. os.cmm - multitasking kernel (alpha, POC) for event-driven C components
# Status
- Plan written (doc/plan.md)
- Concurrency Pattern Diagrams drawn in draw.io (See below: Diagrams - Discussion) and exported as .SVG
- C-- transpiler unfinished, does some transpiling (.cmm files transpiled to -> .c files)
-  Code for very, very simple example written as 4 source files: 
	-  producer.cmm
	-  consumer.cmm
	-  os.cmm (Alpha, suspect, not finished compiling yet)
	-  main.cmm (Alpha, suspect, not compiled yet)
	-  types.h
	-  cos.h (Component O/S)
- The above code is in Alpha (POC) state.  The code is not fully tested (compile errors in os.c) due to time limitations, but should give the gist of what is intended / possible.
- PFR tool - "Parsing Find Replace" (built before JAM, ported during JAM)
- D2F tool - "Diagram to Factbase" (built before JAM, ported during JAM)
- F2J tool - "Factbase to JSON" (built during JAM)
- shell scripts to run the above tools (built during JAM)
- README.md and doc/\* (written during JAM)

## Plan
see doc/Plan.md

obsidian://open?vault=doc&file=Plan

## Diagrams - Discussion
I find it "easier to reason about programs" if I can see diagrams. 

In the vein of "graphic novels", I want a "graphic spec". 

Programs are patterns.  Diagrams are patterns. Diagrams are programs.

see doc/Diagrams.md

obsidian://open?vault=doc&file=Diagrams

## Factbase
A Factbase is a set of triples.

In this project, a FB (factbase) is a set of triples in the syntax of PROLOG[^1] assertions.  This allows SWIPL[^2] to be used to query the factbase[^10].

[^1]: The only reason I used PROLOG is that I already knew how to use PROLOG.  I believe that JavaScript, Python, C++, miniKanren, core.logic, etc. would be compatible.
[^2]: SWIPL is SWI PROLOG
[^10]: Querying is needed to make sense of the diagrams. I didn't get to the point of querying diagrams within the time limits (but am working on a diagram compiler in another project.)

## This Project Uses Existing Technologies
### Ohm-JS
npm install ohm-js@next
### atob
npm install atob
### pako
npm install pako
## Obsidian
for documentation
### Blogs / Articles / Youtube
I leaned on previous work (see blog below).

# C-- 
 C (and most languages, including C++, Haskell, Python, etc.) are dual purpose[^3]:
1. the language describes implementation details of `data`
2. the language describes implementation details of `control flow`

[^3]: In fact, these languages are designed to be GPLs (General purpose Programming Languages).  The idea of using GPLs is rooted in the biases (realities) of the 1950's, 1960's, 1970's.  What we *really* want is IDEs for building computer systems.  GPLs and O/Ss are IDE-wannabes.

C-- is dataless C.  
- c\-\- describes *only* the `control flow` of a program using *handles* to data.  
- Use "super-macros" to hide details of `data implementation`.
- Super-macros are implemented using Ohm-JS.

To make things painfully obvious, I've named ALL macros using "\$" as the first character in the name[^4].  

[^4]:I think that the leading "\$" could be dropped, but I haven't tested this notion.

# Software Components - LEGO® Blocks
There are 2 secrets to making pluggable components (IMO):
1. All components must be concurrent by default.
2. A very, very simple *type* needs to used for inter-component communication.

## Concurrency Simplified:
The secrets to simple concurrency are:
- Make all components into (anonymous) lambdas
- Create a Dispatcher() routine to invoke the concurrent components.

## Inter-Component Communication
A very, very simple *type* needs to used for inter-component communication.

UNIX® pipelines came close to this ideal - but no cigar.  

The inter-component type in UNIX® - lines of text - is too complicated (!) to be used as a universal inter-component communication type.

We need to use `bytes` (or something equally low-level, maybe bits) as the basic type, without ascribing special status to newlines.  We can build up more complicated types by creating type-pipelines.  For example, imagine dropping bytes into one end of a pipeline and getting an object certified to be of "Type XYZ" out of the other end of the pipeline.  This is nothing "new", except that we choose to present successive types in layers instead of in an either-or blob of type-ridden code.  I suspect that programmers think in layers, but hide the layers in the final result (or, they mask the layers by using the same "language" for each layer without intervening isolated components).

The type pipeline verifies that a set of bits does, or does not, conform to the desired type.  The pipeline contains at least two outputs:

1. The bits, if they conform to the desired type.  (Silence, otherwise).
2. An escape-hatch - the bits, if they do not conform to the desired type.  What comes out of this escape hatch?  More inter-component messages (that may hold the bits in question).  UNIX called this `stderr` and most modern GPLs use `throw` to provide escape hatches.  A `throw` or output to `stderr` models all programming as being black-and-white - good or bad - but, in internet programming (IoT, distributed, etc., etc.) not all outcomes can be classified into only 2 categories, for example a `retry` is not bad, it is just a part of the problem that needs to be solved. (If the bits conform to the desired type, then nothing[^5] is produced by this output.)

[^5]: Nothing means *nothing*.  Not even "void".

# Diagrams - DaS

We use text for programming languages based on the biases (realities) of the 1950s, 1960s, 1970s, etc.

In 2020+, there is no reason that we can't *also* use diagrams as syntax.

DaS means "Diagrams as Syntax".

DaS should not be confused with VPL (Visual Programming Languages).

DaS is a hybrid of *simple* diagrams and text.

Some things are better left as text, e.g. `a = b + c;`.

Other things are better left as diagrams, e.g. network diagrams of LEGO®-like components.

# IDEs Instead of GPLs
11th Rule (a) of Programming - GPLs (General purpose Programming Languages) are merely mediocre implementations of IDEs.

The goal of programming is to make computers do what is needed.  GPLs are merely stepping stones towards that goal.

# IDEs Instead of Operating Systems
11th Rule (b) of Programming - OSs (Operating Systems) are merely mediocre implementations of IDEs.

OSs are merely libraries.

The goal of programming is to make computers do what is needed.  OSs are merely stepping stones towards that goal.

# Backgrounders
[Note that urls beginning with "obsidian://open?vault=doc..." can be ignored.  I believe that I pasted full URLs in all such places].
## Triples
https://guitarvydas.github.io/2021/03/16/Triples.html
https://guitarvydas.github.io/2021/11/06/Triples-in-PROLOG.html
## Factbases
https://guitarvydas.github.io/2021/04/26/Factbases-101.html
## Diagrams to Code
See the latest TOC in the blog.
## Ohm-JS
DSL for building parsers.

Based on PEG technology.

IMO - Ohm-JS is the best-of-breed PEG-based technology.

discord: ohmland

github (ohm-js): https://github.com/harc/ohm/tree/master/doc

Ohm Editor: https://ohmlang.github.io/editor/

Ohm: https://ohmlang.github.io

## Diagrams
See the latest TOC in the blog for related topics.
## Efficiency
Computer programs fall - roughly - into two categories:
1. Computer programs that are products (e.g. games, apps, etc.).
2. Computer programs that help computer program developers create products

The idea of "efficiency" is different for both categories.

When designing products, the main criterions are cost and usability:
- How fast will the app run?
- How cheap can the hardware be?

When using computer programs to help program developers, the main criteria are:
- How fast will the app run on development hardware?
- How will the app save development time?
- If more than one human developer works on the same program, how quickly can the developers understand the program being developed?

Most current GPLs are optimized for (1) while sacrificing (2).

Many GPL features were invented in the early days of computing (1950, 1960, 1970, ...) and do not correspond to the realities of 2020+.

A very simple example of the trade-offs that favour (1) over (2) is the fact that most GPLs require declaration-before-use.  Development turn-around time and understanding time would be enhanced by declaration-after-use, but most GPLs insist that the developers waste their (human) time organizing code to appease compilers.

At first, development hardware was slow and 1-pass compilers seemed to be a good idea.  As computer speeds increased, we developed better apps for business users (e.g. better word processors, spreadsheets, etc.) and products (e.g. phones, etc.), but failed to take full advantage of computer speed-ups for developers.  Today, most GPLs still require developers to waste development time arranging code in declaration-before-use order.  

In early days, Early parsers were sneered at, but now we have PEG parsers - a technology which does not yet appear in most popular GPLs.  (Why not?)

We have backtracking languages like PROLOG (which brought about the invention of Relational technologies (like miniKanren)), but developers continue to use languages with features that were invented in the 1950's/60's/70's

## Why Ohm-JS is the Best-of-Breed PEG Tool
https://guitarvydas.github.io/2021/05/09/Ohm-Editor.html

https://guitarvydas.github.io/2021/08/30/Ohm-JS.html

(see the latest TOC in the blog)
## Transpilation
https://guitarvydas.github.io/assets/2012-07-12-Transpilation/index.html#25
## Concurrency is Not Parallelism
Concurrency is a paradigm.

Parallelism is a tactic.

All Parallel programs are concurrent.

Not all concurrent programs are parallel.

Rob Pike: Concurrency is not Parallelism:
https://www.youtube.com/watch?v=oV9rvDllKEg

## Patterns
https://guitarvydas.github.io/2021/11/24/On-Diagram-Notation.html

https://guitarvydas.github.io/2021/03/27/Patterns.html

## Blogs / Articles / Youtube
https://guitarvydas.github.io

## Dataless Languages
https://guitarvydas.github.io/2021/03/02/Dataless-Programming-Language.html

## Future
https://github.com/guitarvydas/jam0002/blob/main/guitarvydas/doc/Future.md

obsidian://open?vault=doc&file=Future
## Glue
https://guitarvydas.github.io/2021/04/11/Glue-Tool.html
https://guitarvydas.github.io/2021/09/15/ABC-Glue.html
https://guitarvydas.github.io/2021/03/24/Glue-Manual.html
## Obsidian
https://obsidian.md
## Programming
https://github.com/guitarvydas/jam0002/blob/main/guitarvydas/doc/Programming.md

obsidian://open?vault=doc&file=Programming
## Normalization
https://github.com/guitarvydas/jam0002/blob/main/guitarvydas/doc/Normalization.md

obsidian://open?vault=doc&file=Normalization
## Macros vs. DSLs
https://github.com/guitarvydas/jam0002/blob/main/guitarvydas/doc/Macros%20vs.%20DSLs.md

obsidian://open?vault=doc&file=Macros%20vs.%20DSLs
## Gotchas
https://github.com/guitarvydas/jam0002/blob/main/guitarvydas/doc/Gotchas.md

obsidian://open?vault=doc&file=Gotchas
## Pattern Matching
https://github.com/guitarvydas/jam0002/blob/main/guitarvydas/doc/Pattern%20Matching.md

obsidian://open?vault=doc&file=Pattern%20Matching
## Relation to Projectional Editing
https://github.com/guitarvydas/jam0002/blob/main/guitarvydas/doc/Related%20-%20Projectional%20Editing.md

obsidian://open?vault=doc&file=Related%20-%20Projectional%20Editing
## Relation to Software Architecture
https://github.com/guitarvydas/jam0002/blob/main/guitarvydas/doc/Software%20Architecture.md

obsidian://open?vault=doc&file=Software%20Architecture

https://guitarvydas.github.io/assets/2021-04-11-DI/index.html#15
## Programming Languages Are Patterns
https://github.com/guitarvydas/jam0002/blob/main/guitarvydas/doc/Programming%20Languages%20Are%20Patterns.md

obsidian://open?vault=doc&file=Programming%20Languages%20Are%20Patterns
## Functional Programming vs. Pattern Matching
https://github.com/guitarvydas/jam0002/blob/main/guitarvydas/doc/Functional%20Programming%20vs.%20Pattern%20Matching.md

obsidian://open?vault=doc&file=Functional%20Programming%20vs.%20Pattern%20Matching
## SWIPL
https://www.swi-prolog.org
## Blog
https://guitarvydas.github.io
