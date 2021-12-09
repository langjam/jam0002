from unicurses import *

class Tui:
    """
    Flow:
    - create grid based on meta
    - accept user input based on aliases
    - user can choose to play, pause, or step forward through simulation
    - when playing, use MaxFPS from meta (default to 1FPS)
    - when paused, user can manually intervene in the simulation
    """

    def __init__(self, simulation):
        self.simulation = simulation

        stdscr = initscr()
        clear()
        noecho()
        cbreak()

        meta = simulation.get_meta()
        self.max_y = meta['rows']
        self.max_x = meta['cols']

        console_h, console_w = getmaxyx(stdscr)
        self.window = None
        self._resize_simulation_window(console_h, console_w)

    def run(self):
        y, x = self.max_y // 2, self.max_x // 2
        self._draw_simulation_window()
        wmove(self.window, y, x)

        while True:
            c = wgetch(self.window)
            if c == 27: # esc
                break
            elif c == 32:
                self.simulation.step()
            elif c == KEY_RESIZE:
                # TODO handle resize
                # console_h, console_w = getmaxyx(0)
                # self._resize_simulation_window(console_h, console_w)
                pass
            elif c == KEY_UP:
                if y > 1: y -=1
            elif c == KEY_DOWN:
                if y < self.max_y: y += 1
            elif c == KEY_LEFT:
                if x > 1: x -=1
            elif c == KEY_RIGHT:
                if x < self.max_x: x += 1
            elif ord('A') <= c <= ord('Z') or ord('a') <= c <= ord('z'):
                try:
                    self.simulation.update(y - 1, x - 1, chr(c))
                except Exception:
                    pass

            wmove(self.window, y, x)
            self._draw_simulation_window()
        endwin()

    def _resize_simulation_window(self, console_h, console_w):
        win_h = self.max_y + 2
        win_w = self.max_x + 2
        start_y = 2
        start_x = (console_w - (self.max_x + 2)) // 2
        if self.window is None:
            self.window = newwin(win_h, win_w, start_y, start_x)
            keypad(self.window, True)
            wborder(self.window)
            mvwaddstr(self.window, 0, 2, "[Simulation]")
        else:
            mvwin(self.window, start_y, start_x)

    def _draw_simulation_window(self):
        orig_y, orig_x = getyx(self.window)
        wclear(self.window)
        wborder(self.window)
        mvwaddstr(self.window, 0, 2, "[Simulation]")
        frame = self.simulation.get_frame()
        for j in range(self.max_y):
            for i in range(self.max_x):
                if frame[j][i] is not None:
                    # Add offset for border
                    mvwaddch(self.window, j + 1, i + 1, frame[j][i])
        # Restore cursor position
        wmove(self.window, orig_y, orig_x)
        wrefresh(self.window)

# Dummy simulation class for testing
from random import random
def _gen_frame(pct, rows, cols):
    return [['L' if random() > pct else None for x in range(cols)] for y in range(rows)]
class Simulation:
    rows = 20
    cols = 80
    pct = 0.9
    cur_frame = _gen_frame(pct, rows, cols)

    def get_meta(self):
        return {'rows': self.rows, 'cols': self.cols}

    def get_frame(self):
        return self.cur_frame

    def step(self):
        self.cur_frame = _gen_frame(self.pct, self.rows, self.cols)
        return self.cur_frame

    def update(self, y, x, alias):
        if alias != 'L':
            raise Exception(f"Bad alias {alias}")
        self.cur_frame[y][x] = alias
        return self.cur_frame
