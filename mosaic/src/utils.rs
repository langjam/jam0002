use crate::*;

pub type Pos = (isize, isize);
pub type Cell = (char, char);

pub const WILD: char = '_';
pub const BLANK: char = '.';

#[derive(Default, Debug)]
pub struct Grid {
  pub cells: HashMap<Pos, Cell>,
  pub region: Region,
}

#[derive(Default, Debug)]
pub struct Region {
  pub set: bool,
  pub x_min: isize,
  pub x_max: isize,
  pub y_min: isize,
  pub y_max: isize,
}

impl Region {
  pub fn expand_to(&mut self, pos: Pos) {
    if !self.set {
      self.x_min = pos.0;
      self.x_max = pos.0;
      self.y_min = pos.1;
      self.y_max = pos.1;
      self.set = true;
    } else {
      self.x_min = std::cmp::min(self.x_min, pos.0);
      self.x_max = std::cmp::max(self.x_max, pos.0);
      self.y_min = std::cmp::min(self.y_min, pos.1);
      self.y_max = std::cmp::max(self.y_max, pos.1);
    }
  }
}

#[derive(Debug)]
pub struct Rule {
  pub pat: Grid,
  pub pat_init: Region,
  pub repl: Grid,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IoKind {
  Input,
  Output,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IoFormat {
  Char,
  Bin,
}

#[derive(Debug)]
pub enum Statement {
  Rule(Rule),
  Loop(Vec<Statement>),
  Io(IoKind, IoFormat, Cell),
  Debug,
}
