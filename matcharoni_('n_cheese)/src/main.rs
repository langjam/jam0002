#![feature(box_patterns)]
#![feature(map_try_insert)]

mod interp;
mod parser;

fn main() -> anyhow::Result<()> {
    let filename = std::env::args()
        .nth(1)
        .ok_or(anyhow::anyhow!("no filename given"))?;

    let program_str = std::fs::read_to_string(filename)?;
    let program = parser::parse_program(&program_str)?;
    let mut interp = interp::Interpreter::new();
    for statement in &program.statements {
        interp.eval_statement(statement)?;
    }
    Ok(())
}
