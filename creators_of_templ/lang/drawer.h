#ifndef DRAWER_H
#define DRAWER_H

#include "runner.h"
#include <stdbool.h>

void draw_init(int canvas_size);
void draw_update(Runner *runner);
bool draw_running();
void draw_deinit();
void draw_screenshot(char *path);

#endif

