#define attribute 
#include <stdio.h>
#include <stdlib.h>
#include "lang/lexer.h"
#include "lang/err.h"
#include "lang/parser.h"
#include "lang/drawer.h"
#include "raylib.h"
#include "lang/runner.h"

char* read_file(FILE *f) {
    fseek(f, 0, SEEK_END);
    size_t fsz = (size_t) ftell(f);
    fseek(f, 0, SEEK_SET);
    char *input = (char*)malloc((fsz+1)*sizeof(char));
    input[fsz] = 0;
    fread(input, sizeof(char), fsz, f);
    return input;
}

void help() {
	printf("templ <file> [ -size <n> ] [ -h ] [ -o <file> ] [ -d ]\n    -size The NxN dimensions of your window or image\n    -h Print this page\n    -o Output file name (please only use .png)\n    Example usage: templ -size 500 -o output.png input.css\n    -d Print debug information\n");
}

int main(int argc, char *argv[]) {
	int canvas_size = 400;
	FILE *f = NULL;
	char *out_file = NULL;
	bool debugmode = false;
	for (int i=1; i < argc; ++i) {
		if (*argv[i] == '-') {
			if (strcmp(argv[i], "-size") == 0) {
				++i;
				if (i == argc) {
					printf("Size requires an argument.\n");
					help();
					return 1;
				}
				char *end = NULL;
				canvas_size = strtol(argv[i], &end, 10);
				if (end == argv[i]) {
					printf("The argument to size has to be an integer.\n");
					return 1;
				}
			} else if (strcmp(argv[i], "-o") == 0) {
				++i;
				if (i == argc) {
					printf("-o requires an argument.\n");
					help();
					return 1;
				}
				out_file = argv[i];
			} else if (strcmp(argv[i], "-d") == 0) {
				debugmode = true;
			} else if (strcmp(argv[i], "-h") == 0) {
				help();
				return 0;
			} else if (strcmp(argv[i], "-ascii") == 0) {
				printf(
"      .-.\n"
"     (o.o)\n"
"      |=|\n"
"     __|__\n"
"   //.=|=.\\\\\n"
"  // .=|=. \\\\\n"
"  \\\\ .=|=. //\n"
"   \\\\(_=_)//\n"
"    (:| |:)\n"
"     || ||\n"
"     () ()\n"
"     || ||\n"
"     || ||\n"
"l42 ==' '==\n");
				return 0;
			}
		} else {
			f = fopen(argv[i], "r");
			if (f == NULL) {
				printf("Incorrect file.\n");
				return 1;
			}
		}
	}

	if (f == NULL) {
		printf("No input file supplied.\n");
	
		help();
		return 1;
	}

	

	char *file = read_file(f);
	fclose(f);

	Parser p = parser_init(file);
	if (parser_run(&p)) {
		err_explain(&p.err, file);
		err_explain(&p.note, file);
		goto fail;
	}
	else {
		if (debugmode) {
			ast_pretty_print(&p.ast);
		}
    }
	if (!debugmode) {
		SetTraceLogLevel(LOG_NONE);
	}
    
		
	// runner_dump(&runner);
		
	/*
	Token tok;
	for (tok = next(&l); tok.type && tok.type != tok_eof; tok = next(&l))
		print_tok(tok);
		
	if (!tok.type) {
		err_explain(&l.err, l.buf);
	}
	*/
	Runner runner = { 0 };
	if (runner_init(&p.ast, &runner))  {
		err_explain(&runner.err, file);
		runner_deinit(&runner);
		goto fail; 
	}
	

	draw_init(canvas_size);
	while (draw_running()) {
		draw_update(&runner);
	}

	if (out_file)
		draw_screenshot(out_file);
	draw_deinit();
	runner_deinit(&runner);
	fail:
	parser_deinit(&p);
	free(file);

	return 0;
}

