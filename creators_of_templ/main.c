#include <stdio.h>
#include "lang/templ.h"
#include "lang/err.h"

int main() {
	Lexer l = {
".head {\n"
"  @frame {\n"
"     $circle.pos.x += 10.0.2.4.4.3.2.7.4.3.234234.2.2.7.4.3.234234.2;\n"
"     debug_print(\"hello world!\");\n"
"  circle { radius: 10, pos: $head_pos }\n"
"}\n",
		(Location){0, 0}, 0, { { 0 }, { 0, 0 } }
	};
	printf("Welcome to templ!\n");
	
	Token tok;
	for (tok = next(&l); tok.type && tok.type != tok_eof; tok = next(&l))
		print_tok(tok);
		
	if (!tok.type) {
		err_explain(&l.err, l.buf);
	}
	return 0;
}

