#include "drawer.h"
#include "runner.h"
#include "raylib.h"

bool should_close = false;

Image screen_buffer;
Texture2D screen_render;

void draw_init(int canvas_size) {
	InitWindow(canvas_size, canvas_size, "TEMPL");
	SetTargetFPS(60);

	screen_buffer = GenImageColor(canvas_size, canvas_size, WHITE);
	screen_render = LoadTextureFromImage(screen_buffer);
}

bool draw_running() {
	return !should_close;
}

void draw_update() {
	if (WindowShouldClose())
		should_close = true;
	
	BeginDrawing();
	
	ClearBackground(RAYWHITE);
	
	UpdateTexture(screen_render, screen_buffer.data);
	DrawTexture(screen_render, 0, 0, WHITE);
	
	EndDrawing();
}

void draw_deinit() {
	// the render uses a pointer to buffer's data, do not invert order
	UnloadTexture(screen_render);
	UnloadImage(screen_buffer);
	
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

	ImageDrawCircle(&screen_buffer, pos.x, pos.y, r, *(Color *)&color);
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

	ImageDrawRectangle(&screen_buffer, pos.x, pos.y, dm.x, dm.y, *(Color *)&color);
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
