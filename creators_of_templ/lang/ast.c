#include "ast.h"
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

