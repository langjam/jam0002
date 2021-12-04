#include <stdio.h>
#include <stdlib.h>
#include "lang/lexer.h"
#include "lang/err.h"
#include "lang/parser.h"

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

void prettyprint_node(Node *node, int depth) {
	printf("%*c", depth-1, ' ');
	
	switch (node->type) {
		case node_inval:       	   printf("invalid");		    break;
		case node_property:  	     printf("property");  	     break;
		case node_atom:     	      printf("atom");		       break;
		case node_property_list:      printf("property_list");      break;
		case node_simple_selector:    printf("simple_selector");    break;
		case node_selector_and_props: printf("selector_and_props"); break;
	}
	printf("\n");
	for (Node *child = node->first_child; child; child = child->sibling) {
		prettyprint_node(child, depth+4);
	}
}

int main() {
	char *file = read_file("input/playground.css");
	if (file == NULL) {
		fprintf(stderr, "Can't find input/playground.css!\n");
		return 1;
	}

	Parser p = parser_init(file);
	if (parser_run(&p))
		err_explain(&p.err, file);
	prettyprint_node(&p.ast.nodes[0], 0);
	parser_deinit(&p);
	/*
	Token tok;
	for (tok = next(&l); tok.type && tok.type != tok_eof; tok = next(&l))
		print_tok(tok);
		
	if (!tok.type) {
		err_explain(&l.err, l.buf);
	}
	*/
	free(file);
	return 0;
}

