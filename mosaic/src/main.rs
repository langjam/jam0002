use std::io::Read;

include!("lib.rs");

pub fn main() {
  let mut input = Vec::new();
  std::io::stdin().lock().read_to_end(&mut input).unwrap();
  let path = std::env::args().skip(1).next().expect("Expected file");
  let content = std::fs::read_to_string(path).expect("Expected file");
  let (initial_grid, program) = parse_program(&content).unwrap();
  let mut program_state = init_program(initial_grid, &program, &input[..]);
  while step_program(&mut program_state, false) != StepProgramResult::End {
    print_grid(&program_state.grid)
  }
  print!(
    "{}",
    std::str::from_utf8(&program_state.output[..]).unwrap()
  )
}

pub fn print_grid(grid: &Grid) {
  for y in grid.region.y_min..=grid.region.y_max {
    for x in grid.region.x_min..=grid.region.x_max {
      let cell = grid.cells.get(&(x, y)).copied().unwrap_or((BLANK, BLANK));
      eprint!("{}{} ", cell.0, cell.1);
    }
    eprint!("\n")
  }
  eprint!("\n")
}
