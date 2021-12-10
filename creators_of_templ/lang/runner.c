#include "runner.h"
#include <stdlib.h>
#include <assert.h>
#include <math.h>
#include <stdio.h>
#include <time.h>

#define checkout(x) do { ErrCode err; if((err = (x))) { fprintf(stderr, ">>> %d %s:%d\n", err, __FILE__, __LINE__); return err;} } while (0)

static char *type_names[] = {
	"nil",
	"number",
	"string",
	"position",
	"color"
};

int64_t millis()
{
	static int x = 0;
	x += 16;
	return x;
}


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


ErrCode make_prop(Runner *r, Node *propdesc, RunnerProp *dest);
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
			dest->data.color = (val << 8) | 0xFF;
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

ErrCode make_value(Runner *r, Node *val, RunnerProp *dest, RunnerPropType type) {
	switch (val->type) {
		case node_unary:
			if (tok_eq(val->token, "-")) {
				if (type != type_number) {
					r->err = err_f(err_badprop, val->token.loc, "Cannot negate `%s'", type_names[dest->type]);
					return err_badprop;
				}
				checkout(make_value(r, val->first_child, dest, type_number));
				dest->data.number *= -1;
			}
			return err_ok;
			break;
		case node_binary:
			if (type != type_number) {
				r->err = err_f(err_badprop, val->token.loc, "Cannot use `%.*s' on `%s'", val->token.len, val->token.val, type_names[dest->type]);
				return err_badprop;
			}
			RunnerProp right;
			if (tok_eq(val->token, "+")) {
				checkout(make_value(r, val->first_child, dest, type_number));
				checkout(make_value(r, val->first_child->sibling, &right, type_number));
				dest->data.number += right.data.number;
			}
			else if (tok_eq(val->token, "-")) {
				checkout(make_value(r, val->first_child, dest, type_number));
				checkout(make_value(r, val->first_child->sibling, &right, type_number));
				dest->data.number -= right.data.number;
			}
			else if (tok_eq(val->token, "*")) {
				checkout(make_value(r, val->first_child, dest, type_number));
				checkout(make_value(r, val->first_child->sibling, &right, type_number));
				dest->data.number *= right.data.number;
			}
			else if (tok_eq(val->token, "/")) {
				checkout(make_value(r, val->first_child, dest, type_number));
				checkout(make_value(r, val->first_child->sibling, &right, type_number));
				dest->data.number /= right.data.number;
			}
			else if (tok_eq(val->token, "^")) {
				checkout(make_value(r, val->first_child, dest, type_number));
				checkout(make_value(r, val->first_child->sibling, &right, type_number));
				dest->data.number = pow(dest->data.number, right.data.number);
			}
			else {
				r->err = err_f(err_badprop, val->token.loc, "Sorry can't handle this operator for now");
				return err_badprop;
			}
			return err_ok;
			break;	
		case node_var:
			if (tok_eq(val->token, "depth")) {
				dest->type = type_number;
				dest->data.number = r->depth;
				return err_ok;
			}
			else {
				char key[128] = { 0 };
				snprintf(key, 128, "%.*s", val->token.len, val->token.val);
				RunnerProp *prop = map_get(&r->constants, key);
				if (prop != NULL) {
					*dest = *prop;
					return err_ok;
				}
			}
			r->err = err_f(err_badprop, val->token.loc, "This constant does not exist");
			return err_badprop;
		default:
		return checked_atom(r, val, dest, type);
	} 
}

double clamp(double n, double bottom, double up) {
	if (n < bottom) return bottom;
	if (n > up) return up;
	return n;
}

ErrCode make_call(Runner *r, Node *call, RunnerProp *dest) {
	
	if (strncmp(call->token.val, "vec2", call->token.len) == 0) {
		dest->type = type_position;
		if (node_child(call, 1) == NULL) {
			r->err = err_f(err_badprop, call->token.loc, "Not enough parameters");
			return err_badprop;
		}
		
		RunnerProp p1, p2;
		checkout(make_value(r, node_child(call, 0), &p1, type_number));
		checkout(make_value(r, node_child(call, 1), &p2, type_number));
		
		dest->data.pos.x = p1.data.number;
		dest->data.pos.y = p2.data.number;
	}
	else if (strncmp(call->token.val, "rgb", call->token.len) == 0) {

		dest->type = type_color;

		if (node_child(call, 2) == NULL) {

			r->err = err_f(err_badprop, call->token.loc, "Not enough parameters");
			return err_badprop;
		}

		RunnerProp red, green, blue, opacity;

		checkout(make_value(r, node_child(call, 0), &red, type_number));
		checkout(make_value(r, node_child(call, 1), &green, type_number));
		checkout(make_value(r, node_child(call, 2), &blue, type_number));
		red.data.number = clamp(red.data.number, 0, 255);
		green.data.number = clamp(green.data.number, 0, 255);
		blue.data.number = clamp(blue.data.number, 0, 255);

		if (node_child(call, 3) != NULL) {
			checkout(make_value(r, node_child(call, 3), &opacity, type_number));
			opacity.data.number = clamp(opacity.data.number, 0, 255);
		}
		else {
			opacity.data.number = 255;
		}

		dest->data.color = ((uint32_t)red.data.number << 24) | ((uint32_t)green.data.number << 16) | ((uint32_t)blue.data.number << 8) | (uint32_t)opacity.data.number;
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
		case node_binary:
		case node_unary:
		case node_var:
		checkout(make_value(r, propdesc, dest, type_number));
		break;
		default:
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
	
	// This is to avoid matched-against selectors re-assigning
	// the original selectors data
	if (dest->selector == NULL) {
		dest->selector = selector;
		
		dest->type = element_root;
		
		if (selector->first_child->type == node_primitive_selector) {
			char *tagname = selector->first_child->token.val;
			int taglen = selector->first_child->token.len;
			if (strncmp(tagname, "circle", taglen) == 0) 
				dest->type = element_circle;
			if (strncmp(tagname, "rect", taglen) == 0) 
				dest->type = element_rect;
			if (strncmp(tagname, "triangle", taglen) == 0) 
				dest->type = element_triangle;
		}
	}
	
	Node *prop_list = node_child(node, 1);
	assert(prop_list->type == node_property_list);
	
	
	static char keybuf[1024] = { 0 };
	
	for (Node *child = prop_list->first_child; child; child = child->sibling) {
		
		
		if (child->type == node_selector_and_props) {
			// Expand children
			if (r->depth <= 7) {
				checkout(expand_tree(r, child, alloc(r, dest)));
			}
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

// Returns whether `sel` contains `simple_against`
// For example: if `sel` is `circle.red` 
// and `simple_against` is `.red` it will match `.red`
// if simple_against is a composite selector (this is really not desirable)
// it will return false
bool runner_selector_contains(Node *sel, Node *simple_against) {
	switch (sel->type) {
		case node_composite_selector:
			for (Node *simple = sel->first_child; simple; simple = simple->sibling) {
				// Then we can re-use code for the composite selectors in same function
				// Here we need to assert that its actually one of the simple selectors
				// Otherwise infinite recursion might happen
				assert(simple->type == node_primitive_selector || simple->type == node_class_selector);
				if (runner_selector_contains(simple, simple_against)) {
					return true;
				}
			} 
			break;
		case node_primitive_selector:
			if (simple_against->type == node_primitive_selector) {
				return toks_eq(simple_against->token, sel->token);
			}
			break;
		case node_class_selector:
			if (simple_against->type == node_class_selector) {
				return toks_eq(simple_against->token, sel->token);
			}
			break;
		default:
			assert(false && "This is not a selector");
	}
	return false;
}

// Returns whether `sel` is compatible to be matched by `against.
// For example: When `sel` is `circle.red` and `against` is `.red` then
// 			 it will return true, because red is in `sel`
//
// Another example: When `sel` is `.red` and against is `circle.red` then
//				  it will return false, because the `circle` is not satisfied
bool runner_selectors_match(Node *sel, Node *against) {
	switch (against->type) {
		case node_composite_selector:
			// Composite selectors are similar to logical "and"
			// They will match so long all elements in `against` 
			// Match those in `sel`, it will still be valid for 
			// `sel` to have specifiers not present in `against`
			for (Node *simple = against->first_child; simple; simple = simple->sibling) {
				// Re-use the bottom implementations, also make sure
				// that these are the primitive types because it may cause infinite recursion
				assert(simple->type == node_primitive_selector || simple->type == node_class_selector);
				
				// A single mismatch is matching fail
				if (!runner_selectors_match(sel, simple)) {
					return false;
				}
			}
			return true;
		break;
		case node_primitive_selector:
			return runner_selector_contains(sel, against);
		case node_class_selector:
			return runner_selector_contains(sel, against);
		default:
			assert(false && "This is not a selector");
	}
	
	return false;
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
		case element_root:
			printf("root");
			break;
		case element_rect:
			printf("rect");
			break;
		case element_circle:
			printf("circle");
			break;
		case element_triangle:
			printf("triangle");
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

// Here we iterate through each node and match it against each selector
// If it matches we expand it
// This works because children are added on-fly, which is pretty neato
ErrCode explosion(Runner *r, RunnerNode *node, Ast *ast, int nesting) {
	if (node == NULL)
		return err_ok;
	
	r->depth = nesting;

	for (Node *rule = ast->nodes[0].first_child; rule; rule = rule->sibling) {
		if (rule->type == node_constant) {
			RunnerProp prop;
			make_prop(r, rule->first_child, &prop);
			char key[128] = { 0 };
			snprintf(key, 128, "%.*s", rule->token.len, rule->token.val);
			map_set(&r->constants, key, prop);
			continue;
		}
		assert(rule->type == node_selector_and_props);
		Node *selector = node_child(rule, 0);
		/*
		printf("Matching ===\n");
		node_pretty_print(node->selector, 0);
		printf("============\n");
		node_pretty_print(selector, 0);
		*/
		bool match = runner_selectors_match(node->selector, selector);
		// printf(".... %s\n", match ? "YES" : "NO");
		if (match) {
			checkout(expand_tree(r, rule, node));
		}
	}
	
	// if (nesting <= 6) {
	checkout(explosion(r, node->first_child, ast, nesting + 1));
	// }
	return explosion(r, node->sibling, ast, nesting);
}

// Initializes runner and uses AST,
// The AST must persist throughout the entire execution
ErrCode runner_init(Ast *ast, Runner *dest) {
	Runner runner = {
		.nodes = malloc(sizeof(RunnerNode)*(1 << 14))
	};
	(void)ast;
	runner.root = alloc(&runner, NULL);
	runner.root->selector = ast_make(ast, NULL);
	
	// This is a hacky way to make a fake root selector
	// So that root will get matched by the expander
	runner.root->selector->token = (Token) { .val = "root", .len = 4 };
	runner.root->selector->type = node_primitive_selector;
	
	runner.root->type = element_root;
	*dest = runner;

	checkout(explosion(dest, dest->root, ast, 0)); 
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
	map_deinit(&runner->constants);
	free(runner->nodes);
}

