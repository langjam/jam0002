#include "lang/ast.h"
#include <string.h>
#include <stdio.h>

static
Token tok(char *s) {
    return (Token) {
        .val = s,
        .len = strlen(s)
    };
}


static
Node node(char *s) {
    return (Node) { .token = tok(s) };
}

int total_asserts = 0;
int total_passed = 0;

#define run_test(call) do { printf("%s:\n", #call); call; } while(0)
#define expect(condition) {printf("  %s \x1b[37m%s\x1b[0m\n", (total_asserts += 1, condition) ? (total_passed += 1, "\x1b[32m[PASS]\x1b[0m") : "\x1b[31m[FAIL]\x1b[0m", #condition);}
#define info(...) { printf("  \x1b[35m[INFO] \x1b[37m"); printf(__VA_ARGS__); printf("\x1b[0m\n"); }

void test_ast() {
    Ast ast = ast_init();
    
    Node *parent = ast_make(&ast, NULL);
    Node *left   = ast_make(&ast, parent);
    Node *right  = ast_make(&ast, parent);
    
    node_set(parent, node("+"));
    node_set(left,   node("10"));
    node_set(right,  node("20"));
    
    expect(strcmp(parent->token.val, "+") == 0);
    expect(parent->first_child == left);
    expect(parent->first_child->sibling == right);
    expect(strcmp(node_child(parent, 0)->token.val, "10") == 0);
    expect(strcmp(node_child(parent, 1)->token.val, "20") == 0);
    expect(node_child(parent, 3) == NULL);
    
    ast_deinit(&ast);
}

void tally() {
    printf("TESTING DONE -- %i out of %i assertions passed\n", total_passed, total_asserts);
}

int main() {
    run_test(test_ast());
    tally();
}

