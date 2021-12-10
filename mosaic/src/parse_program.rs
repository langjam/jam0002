use crate::*;

pub fn parse_program(input: &str) -> Result<(Grid, Vec<Statement>), &'static str> {
  let mut input = input.chars().peekable();
  while input.peek() == Some(&'\n') {
    input.next();
  }
  let mut initial_grid = Grid::default();

  'y: for y in 0.. {
    'x: for x in 0.. {
      if let Some('\n') | None = input.peek() {
        if x == 0 {
          break 'y;
        } else {
          break 'x;
        }
      }
      let a = input.next().ok_or("Expected cell")?;
      let b = input.next().ok_or("Expected cell")?;
      if a.is_whitespace() || b.is_whitespace() {
        return Err("Unexpected space");
      }
      let cell = (a, b);
      if (a, b) != (BLANK, BLANK) {
        initial_grid.cells.insert((x, y), cell);
        initial_grid.region.expand_to((x, y));
      }
      match input.next() {
        Some(' ') => {}
        Some('\n') => break 'x,
        None => break 'y,
        _ => return Err("Expected space or newline"),
      }
    }
  }
  let mut group_stack: Vec<Vec<Statement>> = vec![vec![]];
  while input.peek().is_some() {
    skip_whitespace(&mut input);
    if lookahead_command(&mut input) {
      match input.next().unwrap() {
        '.' => group_stack.last_mut().unwrap().push(Statement::Debug),
        '[' => group_stack.push(vec![]),
        ']' => {
          let statement = Statement::Loop(group_stack.pop().unwrap());
          if let Some(v) = group_stack.last_mut() {
            v.push(statement)
          } else {
            return Err("Unmatched ']'");
          }
        }
        '#' => {
          while input.peek().unwrap_or(&'\n') != &'\n' {
            input.next();
          }
        }
        char @ ('i' | 'I' | 'o' | 'O') => {
          let io_kind = match char {
            'i' | 'I' => IoKind::Input,
            _ => IoKind::Output,
          };
          let io_format = match char {
            'i' | 'o' => IoFormat::Char,
            _ => IoFormat::Bin,
          };
          skip_whitespace(&mut input);
          let a = input.next().ok_or("Expected cell")?;
          let b = input.next().ok_or("Expected cell")?;
          if a.is_whitespace() || b.is_whitespace() {
            return Err("Unexpected space");
          }
          let cell = (a, b);
          group_stack
            .last_mut()
            .unwrap()
            .push(Statement::Io(io_kind, io_format, cell));
        }
        _ => return Err("Unrecognized command"),
      }
    } else if input.peek() != None {
      parse_rule(&mut input, &mut group_stack)?;
    }
  }
  if group_stack.len() > 1 {
    return Err("Unmatched '['");
  }
  Ok((initial_grid, group_stack.pop().unwrap()))
}

fn parse_rule(
  input: &mut std::iter::Peekable<std::str::Chars>,
  group_stack: &mut Vec<Vec<Statement>>,
) -> Result<(), &'static str> {
  let mut pat = Grid::default();
  let mut pat_init = Region::default();
  let mut repl = Grid::default();
  let mut max_width = 0;
  let mut divider = None;
  'y: for y in 0.. {
    skip_single_line_whitespace(input);
    'x: for x in 0.. {
      let prev_max_width = max_width;
      max_width = std::cmp::max(max_width, x);
      if lookahead_command(input) {
        break 'y;
      }
      match input.next() {
        Some(' ') => {
          while input.peek() == Some(&' ') {
            input.next();
          }
          if divider.is_some() {
            if divider != Some(x) {
              return Err("Misaligned divider");
            }
          } else {
            if x <= prev_max_width {
              return Err("Misaligned divider");
            }
            divider = Some(x);
          }
        }
        Some('\n') => {
          if x == 0 {
            break 'y;
          }
          skip_single_line_whitespace(input);
          if lookahead_command(input) {
            break 'y;
          }
          break 'x;
        }
        None => break 'y,
        Some(a) => {
          let b = input.next().ok_or("Expected cell")?;
          if b.is_whitespace() {
            return Err("Unexpected whitespace");
          }
          if divider.map(|d| x > d) == Some(true) {
            if (a, b) != (WILD, WILD) {
              let pos = (x - divider.unwrap() - 1, y);
              repl.cells.insert(pos, (a, b));
              repl.region.expand_to(pos)
            }
          } else {
            let pos = (x, y);
            if (a, b) != (WILD, WILD) {
              pat.cells.insert(pos, (a, b));
              pat.region.expand_to(pos)
            }
            if !((a == WILD || a == BLANK) && (b == WILD || b == BLANK)) {
              pat_init.expand_to(pos)
            }
          }
          match input.next() {
            Some(' ') => {}
            Some('\n') => break 'x,
            None => break 'y,
            _ => return Err("Expected space or newline"),
          }
        }
      }
    }
  }
  if divider.is_none() {
    return Err("Missing divider");
  }
  Ok(group_stack.last_mut().unwrap().push(Statement::Rule(Rule {
    pat,
    pat_init,
    repl,
  })))
}

fn lookahead_command(input: &mut std::iter::Peekable<std::str::Chars>) -> bool {
  input.peek().cloned().map(char::is_whitespace) == Some(false)
    && input.clone().skip(1).next().map(char::is_whitespace) != Some(false)
}

fn skip_single_line_whitespace(input: &mut std::iter::Peekable<std::str::Chars>) {
  while input
    .peek()
    .copied()
    .map(|x| char::is_whitespace(x) && x != '\n')
    == Some(true)
  {
    input.next();
  }
}

fn skip_whitespace(input: &mut std::iter::Peekable<std::str::Chars>) {
  while input.peek().copied().map(char::is_whitespace) == Some(true) {
    input.next();
  }
}
