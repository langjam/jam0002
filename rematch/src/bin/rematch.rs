use anyhow::Context;
use std::{path::PathBuf, str::FromStr};

use rematch::{compile::Compiler, repl::Repl};
use structopt::StructOpt;

#[derive(Debug, Clone)]
enum Input {
    Stdin,
    Path(PathBuf),
}

impl FromStr for Input {
    type Err = <PathBuf as FromStr>::Err;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.trim() {
            "-" => Ok(Self::Stdin),
            s => PathBuf::from_str(s).map(Self::Path),
        }
    }
}

#[derive(Debug, StructOpt)]
struct Args {
    /// Input file
    input: Option<Input>,
}

#[paw::main]
fn main(args: Args) -> anyhow::Result<()> {
    env_logger::init();

    match args.input {
        Some(input) => {
            let mut compiler = match input {
                Input::Stdin => Compiler::from_stdin().context("Parsing stdin"),
                Input::Path(p) => {
                    Compiler::from_file(&p).context(format!("Parsing from `{}`", p.display()))
                }
            }?;
            compiler.execute().map(|_| ()).unwrap_or_default();
        }
        None => {
            let mut repl = Repl::new();
            while repl.read_line() {}
        }
    }
    Ok(())
}
