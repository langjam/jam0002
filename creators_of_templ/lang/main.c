#include <stdio.h>
#include "templ.h"

int main() {
	Lexer l = {
".head {\n"
"  @frame {\n"
"     $circle.pos.x += 10;\n"
"     debug_print(\"hello world!\");\n"
"  circle { radius: 10, pos: $head_pos }\n"
"}\n",
		(Location){0, 0}, 0
	};
	printf("Welcome to templ!\n");

	for (Token tok = next(&l); tok.type && tok.type != tok_eof; tok = next(&l))
		print_tok(tok);
	return 0;
}

