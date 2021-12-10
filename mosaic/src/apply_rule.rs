use crate::*;

pub fn match_pat(cell: Cell, pat: Cell) -> bool {
  (pat.0 == WILD || cell.0 == pat.0) && (pat.1 == WILD || cell.1 == pat.1)
}

pub fn apply_repl(cell: &mut Cell, repl: Cell) -> Cell {
  if repl.0 != WILD {
    cell.0 = repl.0
  }
  if repl.1 != WILD {
    cell.1 = repl.1
  }
  *cell
}

pub fn apply_rule(grid: &mut Grid, rule: &Rule) -> bool {
  for ox in grid.region.x_min - rule.pat_init.x_min..=grid.region.x_max - rule.pat_init.x_max {
    'search: for oy in
      grid.region.y_min - rule.pat_init.y_min..=grid.region.y_max - rule.pat_init.y_max
    {
      for (&(px, py), &pat) in &rule.pat.cells {
        if !match_pat(
          *grid
            .cells
            .get(&(px + ox, py + oy))
            .unwrap_or(&(BLANK, BLANK)),
          pat,
        ) {
          continue 'search;
        }
      }
      for (&(rx, ry), &repl) in &rule.repl.cells {
        let pos = (rx + ox, ry + oy);
        if apply_repl(grid.cells.entry(pos).or_insert((BLANK, BLANK)), repl) != (BLANK, BLANK) {
          grid.region.expand_to(pos);
        }
      }
      return true;
    }
  }
  false
}
