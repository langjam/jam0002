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

    def __init__(self):
        stdscr = initscr()
        clear()
        noecho()
        cbreak()
        self.window = newwin(20, 40, 0, 0)
        wborder(self.window)
        keypad(self.window, True)

    def run(self):
        y, x =  getyx(self.window)
        max_y, max_x = 20, 40
        while True:
            c = wgetch(self.window)
            if c == ord("q"):
                break
            elif c == KEY_UP:
                if y > 0: y -=1
            elif c == KEY_DOWN:
                if y < max_y: y += 1
            elif c == KEY_LEFT:
                if x > 0: x -=1
            elif c == KEY_RIGHT:
                if x < max_x: x += 1

            wmove(self.window, y, x)
            wrefresh(self.window)

if __name__ == '__main__':
    Tui().run()
