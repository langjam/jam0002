use std::{rc::Rc, path::PathBuf};
use muncher::Intrinsics;
use structopt::StructOpt;

struct Intr;

impl Intrinsics for Intr {
    fn print(&self, value: &str) {
        print!("{}", value);
    }
}

#[derive(StructOpt)]
struct Opt {
    /// Source file
    #[structopt(parse(from_os_str))]
    file: PathBuf,

    /// Interpreter stack limit in megabytes, useful if
    /// you get stack overflow when running a program
    #[structopt(long = "stack")]
    stack: Option<usize>,
}

fn main() {
    let Opt { file, stack } = Opt::from_args();
    let source = match std::fs::read_to_string(&file) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("failed to read {}", file.display());
            eprintln!("{}", e);
            std::process::exit(1);
        }
    };
    if let Some(stack) = stack {
        std::thread::Builder::new()
            .stack_size(1024 * 1024 * stack)
            .name("worker".to_owned())
            .spawn(|| run(file, source))
            .expect("failed to start worker thread")
            .join()
            .expect("worker thread panicked");
    } else {
        run(file, source);
    }
}

fn run(file: PathBuf, source: String) {
    match muncher::eval(&source, Rc::new(Intr)) {
        Ok(()) => {}
        Err(e) => {
            e.to_stderr(&file, &source);
        }
    }
}
