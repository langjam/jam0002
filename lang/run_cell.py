from collections import namedtuple
from direction_help import direction_offsets
from ir import meta, cell as cell_def, aliases, selectors, rules
from time import sleep

class Simulation:
  def matchcount(self, dirs, selector, cell): 
    count = 0
    row = cell.row
    col = cell.col
    for deer in dirs:
      row_diff, col_diff = direction_offsets[deer]
      comp_cell = self.grid[(cell.row + row_diff) % self.rows][(cell.col + col_diff) % self.cols]
      if selector(self.selectors, comp_cell, self.builtins):
        count += 1
    return count

  def build_grid(self):
    self.grid = []
    for i in range(self.rows):
      row = []
      self.grid.append(row)
      for j in range(self.cols):
        row.append(self.cell._replace(row=i, col=j))

  def build_next_grid(self):
    self.next_grid = []
    for i in range(self.rows):
      row = []
      self.next_grid.append(row)
      for j in range(self.cols):
        row.append(None)

  def build_builtins(self):
    self.builtins = {
      "matchcount": self.matchcount,
    }

  def __init__(self):
    self.rows = meta["Rows"]
    self.cols = meta["Cols"]
    self.fps = meta["MaxFPS"]
    cell_def.update({"row": 0, "col": 0})
    cell_template = namedtuple("Cell", cell_def)
    self.cell = cell_template(**cell_def)
    self.selectors = selectors
    self.build_builtins()
    self.rules = rules
    self.build_grid()
    self.pprint()
    for _ in range(10):
      self.run()
      sleep(0.1)
      self.pprint()

  def pprint(self):
    for row in self.grid:
      row_vals = []
      for cell in row:
        row_vals.append(f"{int(cell.live)}")
      print(" ".join(row_vals))
    print()

  def run(self):
    self.build_next_grid()
    for row in self.grid:
      for cell in row:
        matched = False
        for rule in self.rules:
          if self.selectors[rule.selector](self.selectors, cell, self.builtins):
            next_state = rule.func(cell)
            matched = True
            break
        if matched:
          next_cell = cell._replace(**next_state)
        else:
          next_cell = cell._replace()
        self.next_grid[cell.row][cell.col] = next_cell
    self.grid = self.next_grid
