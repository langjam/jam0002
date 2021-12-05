#include <stdio.h>
#include <stdlib.h>
#include "lang/lexer.h"
#include "lang/err.h"
#include "lang/parser.h"
#include "lang/drawer.h"
#include "lang/runner.h"

char* read_file(const char *path) {
    FILE *f = fopen(path, "rb");
    if (f == NULL)
        return NULL;
    fseek(f, 0, SEEK_END);
    size_t fsz = (size_t) ftell(f);
    fseek(f, 0, SEEK_SET);
    char *input = (char*)malloc((fsz+1)*sizeof(char));
    input[fsz] = 0;
    fread(input, sizeof(char), fsz, f);
    fclose(f);
    return input;
}

int main() {
	char *file = read_file("input/playground.css");
	if (file == NULL) {
		fprintf(stderr, "Can't find input/playground.css!\n");
		return 1;
	}

	Parser p = parser_init(file);
	if (parser_run(&p)) {
		err_explain(&p.err, file);
		err_explain(&p.note, file);
	}
	else {
        ast_pretty_print(&p.ast);
    }
	Runner runner;
	if (runner_init(&p.ast, &runner))
		return -1; // TODO: Print good error message!
		
	runner_dump(&runner);
		
	/*
	Token tok;
	for (tok = next(&l); tok.type && tok.type != tok_eof; tok = next(&l))
		print_tok(tok);
		
	if (!tok.type) {
		err_explain(&l.err, l.buf);
	}
	*/
	

	draw_init(400);
	while (draw_running()) {
		draw_update(&runner);
	}

	runner_deinit(&runner);
	parser_deinit(&p);
	draw_deinit();
	free(file);

	return 0;
}

