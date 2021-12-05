#include "drawer.h"
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
