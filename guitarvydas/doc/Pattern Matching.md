The best way to recognize patterns in code is to use a "pattern matcher".

The canonical "pattern matcher" is called a "parser".

The best way to use a "pattern matcher" is to use a DSL geared specifically for the purpose of pattern matching.

Parsers have been traditionally been relegated to compiler / interpreter development,

*BUT ...*

1. REGEXP escaped compiler-dom and made its way into popular programming languages (e.g. Perl, JS, Python, etc., etc.).
2. PEG (parsing expression grammars) technology is on the verge of escaping compiler-dom and are being made available for popular use.

PEG libraries are available for many languages,

*BUT ...*

It doesn't matter which language+PEG combination you end up using, it only matters what you *do* with the combination.

IMO, Ohm-JS is the best variant of PEG available.

Currently, Ohm-JS uses JavaScript (I think that Ohm was originally developed in Smalltalk), but, if you use node.js or JS in a browser, then you can match patterns in *any* language, e.g. C++, Python, WASM, etc., etc., *and*, you don't need to care that JavaScript is being used under-the-hood.

Note that REGEXP is a DSL for pattern matching, but, that Ohm (PEG) is more powerful than REGEXP, because PEG allows named rules and recursion.  

I believe that REGEX will be relegated to one-liner knock-offs while Ohm will open the door to more interesting use-cases.

Steps to making Ohm pervasive:

1. The obvious first step in providing Ohm is to create a library.  Done.
2. The second step is to build Ohm into existing editors, e.g. one buffer for the grammar and one buffer for the "semantics".  The editor will, on command, parse-and-rewrite a piece of text from a source buffer to a target buffer using the specifications in the grammar and semantics buffers.
3. The third step is to provide Ohm in IDEs.
4. The final step is to build Ohm into programming languages.