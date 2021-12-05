// Runner that traverses AST

#include "parser.h"
#include "map/map.h"

typedef map_t(Node) RunnerProps;

typedef enum RunnerNodeType {
	element_circle,
	element_rect,
} RunnerNodeType;

typedef struct RunnerNode {
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

