#include "runner.h"
#include <stdlib.h>

static
RunnerNode *alloc(Runner *r, RunnerNode *parent) {
	// Allocate a new node
	RunnerNode *new_node = &r->nodes[r->node_count++];
	
	// Zero-initialize the node
	*new_node = (RunnerNode) { 0 };
	new_node->parent = parent;

	// Make node if we have a parent
	if (parent != NULL)
	{
		// Find previous item before last child
		RunnerNode **n;
		for (n = &parent->first_child; *n; n = &(*n)->sibling);
		
		// Attach the node
		*n = new_node;
	}
	
	return new_node;
}

void cons_tree(Runner *r, Node *node) {
	// r->
	(void)(r);
	(void)(node);
	// for (Node *child = node->first_child;
}

// Initializes runner and uses AST,
// The AST must persist throughout the entire execution
Runner runner_init(Ast *ast) {
	Runner runner = {
		.nodes = malloc(sizeof(RunnerNode)*(1 << 14))
	};
	
	runner.root = alloc(&runner, NULL);
	
	cons_tree(&runner, &ast->nodes[0]);
	
	return runner;
}

// Executes the runner
void runner_exec(Runner *runner);

// Removes data (doesn't touch AST)
void runner_deinit(Runner *runner);

