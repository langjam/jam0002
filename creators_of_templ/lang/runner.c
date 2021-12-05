#include "runner.h"
#include <stdlib.h>
#include <assert.h>
#include <stdio.h>

#define checkout(x) do { ErrCode err; if((err = (x))) { fprintf(stderr, ">>> %d %s:%d\n", err, __FILE__, __LINE__); return err;} } while (0)

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
		case tok_hexlit: {
			char *endptr = NULL;
			uint32_t val = strtoul(atom->token.val+1, &endptr, 16);
			dest->type = type_color;
			dest->data.color = val;
			return err_ok;
		} break;
		default:
			// TODO: Provide Err struct in runtime
			return err_badprop;
	}
}

ErrCode make_call(Node *call, RunnerProp *dest) {
	(void) dest;
	if (strncmp(call->token.val, "vec2", call->token.len)) {
		// TODO: Handle vec2 
		return err_ok;
	}
	else {
		// TODO: Provide info about invalid call
		return err_badprop;
	}	
}

ErrCode make_prop(Node *propdesc, RunnerProp *dest) {
	switch (propdesc->type) {
		case node_call:
			return make_call(propdesc, dest);
			break;
		case node_atom:
			return make_atom(propdesc, dest);
			break;
		default:
			// TODO: Provide Err struct in runtime
			return err_badprop;
	}
}

ErrCode expand_tree(Runner *r, Node *node, RunnerNode *dest) {
	if (node->type == node_root)
		node = node->first_child;
	assert(node->type == node_selector_and_props);
	Node *prop_list = node_child(node, node->type == node_selector_and_props);
	assert(prop_list->type == node_property_list);
	
	map_init(&dest->props);
	
	static char keybuf[1024] = { 0 };
	
	for (Node *child = prop_list->first_child; child; child = child->sibling) {
	
		if (child->type == node_selector_and_props) {
			// Expand children
			checkout(expand_tree(r, child, alloc(r, dest)));
		}
		if (child->type == node_property) {
			// Store the key in temporary storage
			sprintf(keybuf, "%.*s", child->token.len, child->token.val);
			
			printf("Adding a key %s\n", keybuf);
			
			// Get the value of the prop 
			Node *prop_val = node_child(child, 0);
			
			// Serialize the prop into runnerprop
			RunnerProp dest_prop;
			checkout(make_prop(prop_val, &dest_prop));

			// Put it into props
			map_set(&dest->props, keybuf, dest_prop); 
		}
	}
	return err_ok;
}

void dump_prop(RunnerProp prop) {
	switch (prop.type) {
		case type_nil:
			printf("nil");
			break;
		case type_color:
			printf("%x", prop.data.color);
			break;
		case type_number:
			printf("%g", prop.data.number);
			break;
		default:
			printf("TODO");
			break;
	}
}

void _runner_dump(RunnerNode *node, int depth) {
	if (node == NULL)
		return;
	
	printf("%*c", depth-1, ' '); 
	
	switch (node->type) {
		case element_rect:
			printf("rect");
			break;
		case element_circle:
			printf("circle");
			break;
	}
	printf("\n");
	
	map_iter_t iter = map_iter(node->props);
	const char *key;
	
	while ((key = map_next(&node->props, &iter))) {
		printf("%*c", depth-1, ' '); 
		printf("%s ", key);
		dump_prop(*map_get(&node->props, key));
		printf("\n");
	}	
		
	_runner_dump(node->first_child, depth + 4);
	_runner_dump(node->sibling, depth);
}

void runner_dump(Runner *runner) {
	printf("root\n");
	_runner_dump(runner->root, 0);
}

RunnerProp* runner_get_node_prop(RunnerNode *node, const char *key) {
	return map_get(&node->props, key);
}

// Initializes runner and uses AST,
// The AST must persist throughout the entire execution
ErrCode runner_init(Ast *ast, Runner *dest) {
	Runner runner = {
		.nodes = malloc(sizeof(RunnerNode)*(1 << 14))
	};
	(void)ast;
	runner.root = alloc(&runner, NULL);
	checkout(expand_tree(&runner, &ast->nodes[0], runner.root));
	*dest = runner;
	return err_ok;
}

// Executes the runner
void runner_exec(Runner *runner);

// Removes data (doesn't touch AST)
void runner_deinit(Runner *runner);

