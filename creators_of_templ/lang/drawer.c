#include "drawer.h"
#include "runner.h"
#include <raylib.h>

int should_close = 0;

void draw_init(int canvas_size) {
	InitWindow(canvas_size, canvas_size, "TEMPL");

	WindowShouldClose();
	BeginDrawing();
}

void draw_update() {
	EndDrawing();

	should_close = WindowShouldClose();
	BeginDrawing();
}

void draw_deinit() {
	CloseWindow();
}

void draw_circle(RunnerProps *props) {
	double r = 10;
	Pos pos = {0, 0};
	uint32_t color = 0xff;

	RunnerProp *p;
	if ( (p = map_get(props, "position")) && p->type == type_position)
		pos = p->data.pos;
	if ( (p = map_get(props, "radius")) && p->type == type_number)
		r = p->data.number;
	if ( (p = map_get(props, "color")) && p->type == type_color)
		color = p->data.color;

	DrawCircle(pos.x, pos.y, r, *(Color *)&color);
}

void draw_rect(RunnerProps *props) {
	Pos pos = {0, 0};
	Pos dm = {10, 10};
	uint32_t color = 0xff;

	RunnerProp *p;
	if ( (p = map_get(props, "position")) && p->type == type_position)
		pos = p->data.pos;
	if ( (p = map_get(props, "dimensions")) && p->type == type_number)
		dm = p->data.pos;
	if ( (p = map_get(props, "color")) && p->type == type_color)
		color = p->data.color;

	DrawRectangle(pos.x, pos.y, dm.x, dm.y, *(Color *)&color);
}

void draw_runner_node(RunnerNode *node) {
	switch (node->type) {
	case element_circle:
		draw_circle(&node->props);	
		break;
	case element_rect:
		draw_rect(&node->props);	
		break;
	}

	if (node->first_child)
		draw_runner_node(node->first_child);

	if (node->sibling)
		draw_runner_node(node->sibling);
}
