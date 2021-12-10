use mosaic::*;
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub struct Program {
  state: ProgramState<'static>,
  _program: Box<Vec<Statement>>,
  _input: String,
}

#[wasm_bindgen]
impl Program {
  #[wasm_bindgen]
  pub fn new(string: &str, input: &str) -> Result<Program, JsValue> {
    let (grid, program) = parse_program(string)?;
    let program = Box::new(program);
    let input = input.to_string();
    let state = init_program(grid, unsafe { std::mem::transmute(&*program) }, unsafe {
      std::mem::transmute(input.as_bytes())
    });
    Ok(Program {
      state,
      _program: program,
      _input: input,
    })
  }
  #[wasm_bindgen]
  pub fn step(&mut self) -> Option<bool> {
    match step_program(&mut self.state, true) {
      StepProgramResult::ReplaceSuccess => Some(false),
      StepProgramResult::Debug => Some(true),
      StepProgramResult::End => None,
    }
  }
  #[wasm_bindgen]
  pub fn grid_region(&self) -> JsRegion {
    let Region {
      x_min,
      y_min,
      x_max,
      y_max,
      ..
    } = self.state.grid.region;
    JsRegion {
      x_min,
      y_min,
      x_max,
      y_max,
    }
  }
  #[wasm_bindgen]
  pub fn grid_get(&self, x: isize, y: isize) -> Option<JsCell> {
    self
      .state
      .grid
      .cells
      .get(&(x, y))
      .map(|&(color, symbol)| JsCell { color, symbol })
  }
  #[wasm_bindgen]
  pub fn output(&self) -> String {
    (std::str::from_utf8(&self.state.output[..]).unwrap_or("[invalid utf8]")).to_string()
  }
}

#[wasm_bindgen]
pub struct JsCell {
  pub color: char,
  pub symbol: char,
}

#[wasm_bindgen]
pub struct JsRegion {
  pub x_min: isize,
  pub y_min: isize,
  pub x_max: isize,
  pub y_max: isize,
}

#[wasm_bindgen]
pub fn hi() -> String {
  "hi".to_string()
}
