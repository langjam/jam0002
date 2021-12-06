synopsis: 
I want to write grammar the does macro processing.

A macro is "$data" (...) or "$newMessage" (...).

If neither macro is seen, the grammar should preserve the next character without removing spaces.

Background:
I'm trying to write a C program that uses only handles to data and invokes macros to do the dirty work (the actual C code).
