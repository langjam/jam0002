#include "ast.h"
#include "lexer.h"
#include <stdio.h>
#include <stdlib.h>

#define CAPACITY (1 << 16)

// Creates an ast
Ast ast_init() {
	Node *nodes = malloc(CAPACITY*sizeof(Node));
	return (Ast) { .nodes = nodes };
}

// Makes a zero initialized node
Node* ast_make(Ast* ast, Node* parent) {
	// Allocate a new node
	Node *new_node = &ast->nodes[ast->node_count++];
	
	// Zero-initialize the node
	*new_node = (Node) { 0 };

	// Make node if we have a parent
	if (parent != NULL)
	{
	// Find previous item before last child
	Node **n;
	for (n = &parent->first_child; *n; n = &(*n)->sibling);
	
	// Attach the node
	*n = new_node;
	}
	
	return new_node;
}

// Safely assigns some fields from source, returns destination
Node* node_set(Node *destination, Node source) {
	// Acquire sensetive fields
	Node *first_child = destination->first_child;
	Node *sibling = destination->sibling;
	
	*destination = source;
	
	// Set them back
	destination->first_child = first_child;
	destination->sibling = sibling;
	
	// Return same node
	return destination;
}


Node node_of(NodeType type) {
	return (Node) {
	.type = type,
	};
}


Node node_from(NodeType type, Token token) {
	return (Node) {
	.type = type,
	.token = token
	};
}

// Returns a child at index n, NULL if out of bounds
Node* node_child(Node *node, size_t n) {
	Node *child = node->first_child;
	while (n-- && child) {
	child = child->sibling;
	}
	return child;
}

// Frees ast data
void ast_deinit(Ast* ast) {
	free(ast->nodes);
}	

void node_pretty_print(Node *node, int indent) {
	printf("%*c\x1b[34m", indent, ' ');
	switch (node->type) {
		case node_inval:	   	   printf("invalid");			break;
		case node_property:  		 printf("property");  		 break;
		case node_atom:	 		  printf("atom");			   break;
		case node_property_list:	  printf("property_list");	  break;
		case node_primitive_selector: printf("primitive_selector"); break;
		case node_class_selector:	 printf("class_selector");	 break;
		case node_selector_and_props: printf("selector_and_props"); break;
		case node_composite_selector: printf("composite_selector"); break;
		case node_call:			   printf("call");		  	 break;
		case node_pragma:			 printf("pragma");			 break;
		case node_unary:	 		 printf("unary");			 break;
		case node_binary:			 printf("binary");		     break;
		case node_var:		   	 printf("var");	   		 break;
	}
	printf(": \x1b[37m");
	
	// Invalid token usually means there's no need for a token
	// So we are just not gonna print them
	if (node->token.type != tok_inval) {
		print_tok(node->token);
	}
	else {
		printf("\n");
	}
	
	printf("\x1b[0m");

	if (node->first_child) {
		// printf("%*c(\n", indent, ' ');
		node_pretty_print(node->first_child, indent + 2);
		// printf("%*c)\n", indent, ' ');
	}

		if (node->sibling)
			node_pretty_print(node->sibling, indent);
}

void ast_pretty_print(Ast *ast) {
	node_pretty_print(ast->nodes, 1);
}

