#ifndef AST_H
#define AST_H

#include <stddef.h>
#include "templ.h"


typedef enum NodeType {
    node_inval,
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

// Frees ast data
void ast_deinit(Ast* ast);

#endif // AST_H

