synopsis: 
I want to write grammar that does macro processing.

A macro is "$data" (...) or "$newMessage" (...).  [There are lots more macros, but I've whittled the test down to these two as being representative of what I want to accomplish].

I want to write the macro grammar as syntactic rules.  If neither macro is seen, the grammar should preserve the next character without removing spaces.

I *think* that I want to write "lex_any" which is something like "#(any)".  I haven't figured out how to preserve whitespace for "any" in a syntactic rule. Maybe I'm missing something obvious, or, maybe I need to dig into the source component of the CST - I don't know, yet.

Background:
[I think that we are in vastly different timezones. My question is half-baked, but I want to ask it before I go to bed].

I'm trying to write a C program (dataless C) that uses only handles to data and invokes macros to do the dirty work (the actual C code).

The detailed grammar attempt is in https://github.com/guitarvydas/jam0002/blob/main/guitarvydas/short.ohm. (This is a small test, the full target is https://github.com/guitarvydas/jam0002/blob/main/guitarvydas/cpre.ohm)

An example (untested at this moment) of dataless C is https://github.com/guitarvydas/jam0002/blob/main/guitarvydas/os.cpre.

[FYI - This is with respect to langjam #0002.]

[FYI - I've built various attempts using only lexical rules. The result is ugly.
