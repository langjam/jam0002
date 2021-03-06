#include "drawer.h"
#include "runner.h"
#include "raylib.h"
#include <stdio.h>

bool should_close = false;

Color int_to_color(uint32_t color) {

	Color result = {

		.r = (color >> 24) & 0xFF,
		.g = (color >> 16) & 0xFF,
		.b = (color >> 8) & 0xFF,
		.a = color & 0xFF
	};

	return result;
}

void draw_init(int canvas_size) {
	SetConfigFlags(FLAG_MSAA_4X_HINT);
	InitWindow(canvas_size, canvas_size, "TEMPL");
	SetTargetFPS(60);
}

bool draw_running() {
	return !should_close;
}

void draw_runner_node(RunnerNode *node, Camera2D transform);

void draw_update(Runner *runner) {
	if (WindowShouldClose())
		should_close = true;
	
	BeginDrawing();
	
	ClearBackground(RAYWHITE);
	
	Camera2D cam = { 0 };
	cam.zoom = 1.0;
	
	draw_runner_node(runner->root, cam);
	
	EndDrawing();
}

void draw_deinit() {
	
	CloseWindow();
}

void draw_circle(RunnerProps *props) {
	double r = 0;
	uint32_t color = 0xff;

	RunnerProp *p;
	if ((p = map_get(props, "radius")) && p->type == type_number)
		r = p->data.number;
	if ((p = map_get(props, "color")) && p->type == type_color)
		color = p->data.color;
		
	Color raycolor = int_to_color(color);

	DrawCircle(0, 0, r, raycolor);
}

void draw_triangle(RunnerProps *props) {
	double r = 0;
	uint32_t color = 0xff;

	RunnerProp *p;
	if ((p = map_get(props, "radius")) && p->type == type_number)
		r = p->data.number;
	if ((p = map_get(props, "color")) && p->type == type_color)
		color = p->data.color;
		
	Color raycolor = int_to_color(color);
	
	const double COS30 = 0.86602540378; 
	const double SIN30 = 0.5; 

	//    top > /
	//   	  /  
	//  left >/____  < right
	Vector2 top   = {0, -r};
	Vector2 left  = {-r * COS30, r * SIN30};
	Vector2 right = {r * COS30, r * SIN30};

	DrawTriangle(top, left, right, raycolor);
}

void draw_rect(RunnerProps *props) {
	Pos dm = {0, 0};
	uint32_t color = 0xff;

	RunnerProp *p;
	if ((p = map_get(props, "dimensions")) && p->type == type_position)
		dm = p->data.pos;
	if ((p = map_get(props, "color")) && p->type == type_color)
		color = p->data.color;
	Color raycolor = int_to_color(color);
	DrawRectangle(0, 0, dm.x, dm.y, raycolor);
}

void draw_runner_node(RunnerNode *node, Camera2D transform) {
	if (node == NULL)
		return;
		
	RunnerProp *p;
	
	Camera2D old = transform;

	if ((p = map_get(&node->props, "position")) && p->type == type_position) {
		transform.offset.x += p->data.pos.x;
		transform.offset.y += p->data.pos.y;
	}
	if ((p = map_get(&node->props, "rotation")) && p->type == type_number) {
		transform.rotation = p->data.number;
	}
	
	BeginMode2D(transform);
	switch (node->type) {
		case element_triangle:
			draw_triangle(&node->props);	
			break;
		case element_circle:
			draw_circle(&node->props);	
			break;
		case element_rect:
			draw_rect(&node->props);
			break;
		case element_root:
			break;
	}
	
	EndMode2D();
	

	draw_runner_node(node->first_child, transform);
	
	
	transform = old;

	draw_runner_node(node->sibling, transform);
}

void draw_screenshot(char *path) {
	TakeScreenshot(path);
}

