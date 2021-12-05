// Runner that traverses AST

#ifndef RUNNER_H
#define RUNNER_H

#include "parser.h"
#include "map/map.h"
#include <stdint.h>


typedef enum RunnerPropType {
	type_nil,
	type_number,
	type_string,
	type_position,
	type_color
} RunnerPropType;

typedef struct Pos {
	double x;
	double y;
} Pos;

typedef struct RunnerProp {
	RunnerPropType type;
	union {
		double number;
		char *string;
		Pos pos;
		uint32_t color;
	} data;
} RunnerProp;


typedef map_t(RunnerProp) RunnerProps;


typedef enum RunnerNodeType {
	element_circle,
	element_rect,
} RunnerNodeType;


typedef struct RunnerNode {
	Node *selector;
	RunnerNodeType type;
	RunnerProps props;
	struct RunnerNode *parent;
	struct RunnerNode *sibling;
	struct RunnerNode *first_child;
} RunnerNode;


typedef struct Runner {
	RunnerNode *root;
	RunnerNode* nodes;
	size_t node_count;
} Runner;


// Initializes runner and uses AST,
// The AST must persist throughout the entire execution
ErrCode runner_init(Ast *ast, Runner *dest);

// Looks up node property
RunnerProp* runner_get_node_prop(RunnerNode *node, const char *key);

void runner_dump(Runner *runner);

// Executes the runner
void runner_exec(Runner *runner);

// Removes data (doesn't touch AST)
void runner_deinit(Runner *runner);

#endif // RUNNER_H

