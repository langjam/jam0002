mod condition;
mod hsl;
mod instruction;
mod matrix;
mod parser;
mod pixel;
mod vm;

pub use condition::Condition;
pub use instruction::Instruction;
pub use matrix::{Matrix, MatrixPoint};
pub use pixel::Pixel;
pub use vm::VM;

use parser::{parse, pixels};

use std::io::Write;

pub fn run<T: Write>(file: &str, pixel_size: u32, out: T) {
    let program = parse(pixels(file, pixel_size).unwrap());
    let mut vm = VM::new(out);
    vm.execute(program);
}
