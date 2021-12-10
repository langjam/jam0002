use crate::*;

pub struct ProgramState<'a> {
  pub grid: Grid,
  pub call_stack: Vec<ProgramCallStackEntry<'a>>,
  pub input: &'a [u8],
  pub output: Vec<u8>,
}

pub struct ProgramCallStackEntry<'a> {
  pub cont: bool,
  pub i: usize,
  pub statements: &'a Vec<Statement>,
}

pub fn init_program<'a>(
  grid: Grid,
  program: &'a Vec<Statement>,
  input: &'a [u8],
) -> ProgramState<'a> {
  ProgramState {
    grid,
    call_stack: vec![ProgramCallStackEntry {
      cont: false,
      i: 0,
      statements: program,
    }],
    input,
    output: Vec::new(),
  }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum StepProgramResult {
  ReplaceSuccess,
  Debug,
  End,
}

pub fn step_program(state: &mut ProgramState, pause_on_replace: bool) -> StepProgramResult {
  while let Some(stack_top) = state.call_stack.last_mut() {
    if let Some(statement) = stack_top.statements.get(stack_top.i) {
      stack_top.i += 1;
      match statement {
        Statement::Debug => {
          if stack_top.i - 1 == 0 || stack_top.cont || state.call_stack.len() == 1 {
            return StepProgramResult::Debug;
          }
        }
        Statement::Loop(statements) => {
          state.call_stack.push(ProgramCallStackEntry {
            cont: false,
            i: 0,
            statements,
          });
        }
        Statement::Rule(rule) => {
          let success = apply_rule(&mut state.grid, rule);
          stack_top.cont = stack_top.cont || success;
          if success && pause_on_replace {
            return StepProgramResult::ReplaceSuccess;
          }
        }
        &Statement::Io(io_kind, io_format, pat) => {
          let mut cell_count_remaining = match io_format {
            IoFormat::Bin => 8,
            IoFormat::Char => 1,
          };
          if state
            .grid
            .cells
            .values()
            .filter(|&&cell| match_pat(cell, pat))
            .count()
            < cell_count_remaining
          {
            continue;
          }
          let mut n: u8 = match io_kind {
            IoKind::Input => {
              let char = state.input.get(0).copied();
              if char != None {
                state.input = &state.input[1..];
              }
              let char = match char {
                Some(x) if io_format == IoFormat::Char && x.is_ascii_whitespace() => continue,
                Some(x) => x,
                None => continue,
              };
              char.reverse_bits()
            }
            IoKind::Output => 0,
          };
          'search: for x in state.grid.region.x_min..=state.grid.region.x_max {
            for y in state.grid.region.y_min..=state.grid.region.y_max {
              if cell_count_remaining == 0 {
                break 'search;
              }
              let pos = (x, y);
              let cell = state
                .grid
                .cells
                .get(&pos)
                .copied()
                .unwrap_or((BLANK, BLANK));
              if match_pat(cell, pat) {
                cell_count_remaining -= 1;
                match (io_kind, io_format) {
                  (IoKind::Input, IoFormat::Bin) => {
                    state
                      .grid
                      .cells
                      .insert(pos, (cell.0, if n % 2 == 1 { '1' } else { '0' }));
                    n /= 2;
                  }
                  (IoKind::Input, IoFormat::Char) => {
                    state
                      .grid
                      .cells
                      .insert(pos, (cell.0, n.reverse_bits().into()));
                  }
                  (IoKind::Output, IoFormat::Bin) => {
                    n *= 2;
                    n += (cell.1 == '1') as u8;
                  }
                  (IoKind::Output, IoFormat::Char) => {
                    n = cell.1 as u8;
                  }
                }
              }
            }
          }
          if io_kind == IoKind::Output {
            state.output.push(n);
          } else if pause_on_replace {
            return StepProgramResult::ReplaceSuccess;
          }
        }
      }
    } else {
      let cont = stack_top.cont;
      stack_top.cont = false;
      stack_top.i = 0;
      if state.call_stack.len() == 1 {
        state.call_stack.pop();
        break;
      } else if cont {
        let l = state.call_stack.len();
        state.call_stack[l - 2].cont = true;
      } else {
        state.call_stack.pop();
      }
    }
  }
  return StepProgramResult::End;
}
