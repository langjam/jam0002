A DSL is merely a macro on steroids.

Instead of invoking a macro as a function with arguments, you can write a small script (aka DSL).

# C Macros
C macros started out as simple find-and-replace operations.

The C (C++) preprocessor is a DSL for find-and-replace operations.

The C preprocessor is a language unto itself.  

To write a C++ program, you write code in 2 languages - the preprocessor language and the C++ language.

# Lisp Macros
Lisp macros can be thought of as function inlining.

Lisp macros are written as functions - the compiler remembers which functions are macros and which are runtime functions.

Lisp macros
- can be recursive
- use the full Lisp language in specifying the macro.

In Lisp, you write code in only 1 language - Lisp.

To write fancy macros, you write the macro code in Lisp.

Another way of saying it ... Lisp macros are Lisp functions that the Lisp compiler uses at compile time.  Lisp regular functions are used only at runtime.