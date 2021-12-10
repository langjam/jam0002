#include "lang/ast.h"
#include "lang/runner.h"
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

static
Node node_typed(char *s, NodeType type) {
    return (Node) { .token = tok(s), .type = type };
}


int total_asserts = 0;
int total_passed = 0;
#define run_test(call) do { printf("%s:\n", #call); call; } while(0)
#define expect(condition) {printf("  %s \x1b[37m%s\x1b[0m\n", (total_asserts += 1, condition) ? (total_passed += 1, "\x1b[32m[PASS]\x1b[0m") : "\x1b[31m[FAIL]\x1b[0m", #condition);}
#define info(...) { printf("  \x1b[35m[INFO] \x1b[37m"); printf(__VA_ARGS__); printf("\x1b[0m\n"); }
void tally() {
    printf("TESTING DONE -- %i out of %i assertions passed\n", total_passed, total_asserts);
}


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

void test_selector_matching() {
    Ast ast = ast_init();
    
    info("Class VS Primitive"); {
        Node *sel = node_set(ast_make(&ast, NULL), node_typed("red", node_class_selector));
        Node *against = node_set(ast_make(&ast, NULL), node_typed("circle", node_primitive_selector));
        expect(runner_selectors_match(sel, against) == false);
    }
    
    
        
    info("Primitive VS Primitive (Pt 1)"); {
        Node *sel = node_set(ast_make(&ast, NULL), node_typed("circle", node_primitive_selector));
        Node *against = node_set(ast_make(&ast, NULL), node_typed("circle", node_primitive_selector));
        expect(runner_selectors_match(sel, against) == true);
    }
    info("Primitive VS Primitive (Pt 2)"); {
        Node *sel = node_set(ast_make(&ast, NULL), node_typed("haha_i_dont_match", node_primitive_selector));
        Node *against = node_set(ast_make(&ast, NULL), node_typed("circle", node_primitive_selector));
        expect(runner_selectors_match(sel, against) == false);
    }
    info("Class VS Class (Pt 1)"); {
        Node *sel = node_set(ast_make(&ast, NULL), node_typed("red", node_class_selector));
        Node *against = node_set(ast_make(&ast, NULL), node_typed("red", node_class_selector));
        expect(runner_selectors_match(sel, against) == true);
    }
    info("Class VS Class (Pt 2)"); {
        Node *sel = node_set(ast_make(&ast, NULL), node_typed("red", node_class_selector));
        Node *against = node_set(ast_make(&ast, NULL), node_typed("bobble-wobble", node_class_selector));
        expect(runner_selectors_match(sel, against) == false);
    }
    info("Composite VS Class (Pt 1)"); {
        Node *sel = node_set(ast_make(&ast, NULL), node_typed("", node_composite_selector));
            node_set(ast_make(&ast, sel), node_typed("red", node_class_selector));
            node_set(ast_make(&ast, sel), node_typed("circle", node_primitive_selector));

        Node *against = node_set(ast_make(&ast, NULL), node_typed("red", node_class_selector));
        expect(runner_selectors_match(sel, against) == true);
    }
    info("Composite VS Class (Pt 2)"); {
        Node *sel = node_set(ast_make(&ast, NULL), node_typed("", node_composite_selector));
            node_set(ast_make(&ast, sel), node_typed("red", node_primitive_selector));
            node_set(ast_make(&ast, sel), node_typed("circle", node_primitive_selector));

        Node *against = node_set(ast_make(&ast, NULL), node_typed("red", node_class_selector));
        expect(runner_selectors_match(sel, against) == false);
    }
    info("Composite VS Class (Pt 3)"); {
        Node *sel = node_set(ast_make(&ast, NULL), node_typed("red", node_class_selector));

        Node *against = node_set(ast_make(&ast, NULL), node_typed("", node_composite_selector));
            node_set(ast_make(&ast, against), node_typed("red", node_primitive_selector));
            node_set(ast_make(&ast, against), node_typed("circle", node_primitive_selector));

        expect(runner_selectors_match(sel, against) == false);
    }
    info("Composite VS Class (Pt 4)"); {
        Node *sel = node_set(ast_make(&ast, NULL), node_typed("", node_composite_selector));
            node_set(ast_make(&ast, sel), node_typed("red", node_primitive_selector));
            node_set(ast_make(&ast, sel), node_typed("cool", node_primitive_selector));
            node_set(ast_make(&ast, sel), node_typed("circle", node_primitive_selector));
            
        Node *against = node_set(ast_make(&ast, NULL), node_typed("", node_composite_selector));
            node_set(ast_make(&ast, against), node_typed("red", node_primitive_selector));
            node_set(ast_make(&ast, against), node_typed("circle", node_primitive_selector));

        expect(runner_selectors_match(sel, against) == true);
    }
    info("Composite VS Class (Pt 5)"); {
        Node *sel = node_set(ast_make(&ast, NULL), node_typed("", node_composite_selector));
            node_set(ast_make(&ast, sel), node_typed("red", node_primitive_selector));
            node_set(ast_make(&ast, sel), node_typed("cool", node_primitive_selector));
            
        Node *against = node_set(ast_make(&ast, NULL), node_typed("", node_composite_selector));
            node_set(ast_make(&ast, against), node_typed("red", node_primitive_selector));
            node_set(ast_make(&ast, against), node_typed("circle", node_primitive_selector));

        expect(runner_selectors_match(sel, against) == false);
    }
    
    
    ast_deinit(&ast);
}


int main() {
    run_test(test_ast());
    run_test(test_selector_matching());
    tally();
}

