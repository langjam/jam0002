#ifndef DRAWER_H
#define DRAWER_H

#include <stdbool.h>

void draw_init(int canvas_size);
void draw_update();
bool draw_running();
void draw_deinit();

#endif

