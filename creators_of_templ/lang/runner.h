// Runner that traverses AST

#include "parser.h"
#include "map/map.h"
#include <stdint.h>

typedef enum RunnerPropType {
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
	RunnerNode *nodes;
	size_t node_count;
} Runner;



