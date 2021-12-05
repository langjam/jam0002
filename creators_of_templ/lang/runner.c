#include "runner.h"
#include <stdlib.h>
#include <assert.h>

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

ErrCode make_atom(Node *atom, RunnerProp *dest) {
	switch (atom->token.type) {
		case tok_hexlit:
			char *endptr = NULL;
			uint32_t val = strtoul(atom->token.val, &endptr, 16);
			dest->type = type_color;
			dest->data.color = val;
			break;
	}
}

ErrCode make_call(Node *atom, RunnerProp *dest) {
	// if (strncmp(propdesc->token.val, "vec2", propdesc->token.len)) {
		
	// }
	// else {
	// 	// TODO: Handle invalid call
	// }	
}

ErrCode make_prop(Node *propdesc, RunnerProp *dest) {
	switch (propdesc->type) {
		case node_call:
			make_call(propdesc, dest);
			break;
		case node_atom(propdesc, dest);
			break;
		default:
			// Handle error
	}
}

ErrCode expand_tree(Runner *r, Node *node, RunnerNode *dest) {
	assert(node->type == node_root || node->type == node_selector_and_props);
	Node *prop_list = node_child(node, node->type == node_selector_and_props);
	
	static char keybuf[1024] = { 0 };
	
	for (Node *child = prop_list->first_child; child; child = child->sibling) {
	
		if (child->type == node_selector_and_props) {
			expand_tree(r, node, parent);
		}
		if (child->type == node_property) {
			sprintf(keybuf, "%.*s", child->token.len, child->token.val);
			parse_prop()
			map_set(dest->props, keybuf, ); 
		}
	}
}

// Initializes runner and uses AST,
// The AST must persist throughout the entire execution
Runner runner_init(Ast *ast) {
	Runner runner = {
		.nodes = malloc(sizeof(RunnerNode)*(1 << 14))
	};
	(void)ast;
	runner.root = alloc(&runner, NULL);
	expand_tree(&runner, &ast->nodes[0], runner.root);
	return runner;
}

// Executes the runner
void runner_exec(Runner *runner);

// Removes data (doesn't touch AST)
void runner_deinit(Runner *runner);

