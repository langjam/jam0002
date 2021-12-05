#include "runner.h"
#include <stdlib.h>
#include <assert.h>
#include <stdio.h>

#define checkout(x) do { ErrCode err; if((err = (x))) { fprintf(stderr, ">>> %d %s:%d\n", err, __FILE__, __LINE__); return err;} } while (0)

static char *type_names[] = {
	"nil",
	"number",
	"string",
	"position",
	"color"
};

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

ErrCode make_atom(Runner *r, Node *atom, RunnerProp *dest) {
	// token val isnt null terminated
	char buf[64] = {0};
	strncpy(buf, atom->token.val, atom->token.len);
	switch (atom->token.type) {
		case tok_hexlit: {
			char *endptr = NULL;
			uint32_t val = strtoul(buf+1, &endptr, 16);
			if (endptr == buf) {
				r->err = err_f(err_bad_number_literal, atom->token.loc, "Bad number literal");
				return err_bad_number_literal;
			}
			dest->type = type_color;
			dest->data.color = val;
		} break;
		case tok_numlit: {
			char *endptr = NULL;
			dest->type = type_number;
			dest->data.number = strtod(buf, &endptr);
			if (endptr == buf) {
				r->err = err_f(err_bad_number_literal, atom->token.loc, "Bad number literal");
				return err_bad_number_literal;
			}
		} break;
		default:
			// TODO: Provide Err struct in runtime
			r->err = err_f(err_badprop, atom->token.loc, "You can't use this in a property value");
			return err_badprop;
	}
	return err_ok;
}



ErrCode checked_atom(Runner *r, Node *atom, RunnerProp *dest, RunnerPropType type) {
	checkout(make_atom(r, atom, dest));
	if (dest->type != type) {
		r->err = err_f(err_badprop, atom->token.loc, "Type mismatch, expected %s got %s", type_names[type], type_names[dest->type]);
		return err_badprop;
	}
	return err_ok;
}

ErrCode make_call(Runner *r, Node *call, RunnerProp *dest) {

	if (strncmp(call->token.val, "vec2", call->token.len) == 0) {
		dest->type = type_position;
		if (node_child(call, 1) == NULL) {
			r->err = err_f(err_badprop, call->token.loc, "Not enough parameters");
			return err_badprop;
		}
		
		RunnerProp p1, p2;
		checkout(checked_atom(r, node_child(call, 0), &p1, type_number));
		checkout(checked_atom(r, node_child(call, 1), &p2, type_number));

		dest->data.pos.x = p1.data.number;
		dest->data.pos.y = p2.data.number;
	}
	else {
		// TODO: Provide info about invalid call
		return err_badprop;
	}	
	return err_ok;
}

ErrCode make_prop(Runner *r, Node *propdesc, RunnerProp *dest) {
	switch (propdesc->type) {
		case node_call:
			checkout(make_call(r, propdesc, dest));
			break;
		case node_atom:
			checkout(make_atom(r, propdesc, dest));
			break;
		default:
			// TODO: Provide Err struct in runtime
			r->err = err_f(err_badprop, propdesc->token.loc, "You can't use this in a property value");
			return err_badprop;
	}
	return err_ok;
}

ErrCode expand_tree(Runner *r, Node *node, RunnerNode *dest) {
	if (node->type == node_root)
		node = node->first_child;
	assert(node->type == node_selector_and_props);

	Node *selector = node_child(node, 0);
	dest->selector = selector;
	dest->type = element_root;
	
	if (selector->first_child->type == node_primitive_selector) {
		char *tagname = selector->first_child->token.val;
		int taglen = selector->first_child->token.len;
		if (strncmp(tagname, "circle", taglen) == 0) 
			dest->type = element_circle;
		if (strncmp(tagname, "rect", taglen) == 0) 
			dest->type = element_rect;
	}
	
	Node *prop_list = node_child(node, 1);
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
			
			// Get the value of the prop 
			Node *prop_val = node_child(child, 0);
			
			// Serialize the prop into runnerprop
			RunnerProp dest_prop;
			checkout(make_prop(r, prop_val, &dest_prop));

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
		case type_position:
			printf("vec2(%g, %g)", prop.data.pos.x, prop.data.pos.y);
			break;
		case type_string:
			printf("'%s'", prop.data.string);
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
		case element_root:
			printf("root");
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
	runner.root->type = element_root;
	*dest = runner;
	checkout(expand_tree(dest, &ast->nodes[0], dest->root));
	return err_ok;
}

// Executes the runner
void runner_exec(Runner *runner);

void _runner_deinit(RunnerNode *node) {
	if (node == NULL)
		return;
		
	map_deinit(&node->props);
	
	_runner_deinit(node->first_child);
	_runner_deinit(node->sibling);
}

// Removes data (doesn't touch AST)
void runner_deinit(Runner *runner) {
	_runner_deinit(runner->root);
	free(runner->nodes);
}

