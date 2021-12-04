#ifndef AST_H
#define AST_H

#include <stddef.h>
#include "lexer.h"


typedef enum NodeType {
	node_inval,
	node_simple_selector,
	node_property_list,
	node_atom,
	node_selector_and_props,
	node_property,
	node_call
} NodeType;


typedef struct Node {
	NodeType type;
	Token token;
	struct Node *first_child;
	struct Node *sibling;
} Node;


typedef struct Ast {
	Node *nodes;
	size_t node_count;
} Ast;

// Creates an ast
Ast ast_init();

// Makes a zero initialized node
Node* ast_make(Ast* ast, Node* parent);

// Returns a child at index n, NULL if out of bounds
Node* node_child(Node *node, size_t n);

// Safely assigns some fields from source, returns destination
Node* node_set(Node *destination, Node source);

// Convenient node constructor
Node node_from(NodeType type, Token token);

// Constructs node from type
Node node_of(NodeType type);

// Frees ast data
void ast_deinit(Ast* ast);

// Pretty prints the ast
void ast_pretty_print(Ast *ast);

#endif // AST_H

