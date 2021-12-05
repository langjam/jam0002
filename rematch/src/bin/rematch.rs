use std::path::PathBuf;

use codespan::Files;
use codespan_reporting::term::{
    self,
    termcolor::{ColorChoice, StandardStream},
};
use rematch::{
    loc::Located,
    parser::{parse, parse_repl, ReplParse},
    AstContext, ExecutionError,
};
use rustyline::{error::ReadlineError, Editor};
use structopt::StructOpt;

struct Repl {
    rl: Editor<()>,
    context: AstContext,
    sources: Files<String>,
    idx: usize,
}

impl Drop for Repl {
    fn drop(&mut self) {
        self.rl.save_history(Self::HISTORY).unwrap_or_default();
    }
}

impl Repl {
    const HISTORY: &'static str = "/tmp/gtx.history";
    pub fn new() -> Self {
        let mut rl = Editor::<()>::new();
        rl.load_history(Self::HISTORY).unwrap_or_default();

        Self {
            rl,
            context: AstContext::default(),
            sources: Files::new(),
            idx: 0,
        }
    }

    pub fn read_line(&mut self) -> bool {
        let mut writer = StandardStream::stderr(ColorChoice::Auto);
        let write_config = term::Config::default();
        match self.rl.readline(&format!("gtx:{}> ", self.idx)) {
            Ok(line) => {
                self.rl.add_history_entry(&line);
                let name = format!("<gtx:{}>", self.idx);
                self.idx += 1;
                let file_id = self.sources.add(name, line);
                match parse_repl(&self.sources, file_id) {
                    Ok(Located {
                        value: ReplParse::Decl(decl),
                        ..
                    }) => {
                        let name = decl.name.clone();
                        self.context.add_decl(decl);
                        println!("{:?}", self.context.binding(name.as_deref().into_inner()));
                    }
                    Ok(expr) => {
                        let ast = self.context.make_expr(expr.map(|p| {
                            if let ReplParse::Expr(e) = p {
                                e
                            } else {
                                unreachable!()
                            }
                        }));
                        match ast.run(&self.context) {
                            Ok(res) => println!("-> {}", res.display(&self.context)),
                            Err(err) => {
                                let diag = ExecutionError::make_diagnostic(err);
                                term::emit(&mut writer, &write_config, &self.sources, &diag)
                                    .unwrap();
                            }
                        }
                    }
                    Err(diag) => {
                        term::emit(&mut writer, &write_config, &self.sources, &diag).unwrap()
                    }
                }
                true
            }
            Err(ReadlineError::Interrupted) => true,
            Err(ReadlineError::Eof) => false,
            Err(err) => {
                eprintln!("Fatal error: {}", err);
                false
            }
        }
    }
}

#[derive(Debug, StructOpt)]
struct Args {
    input: Option<PathBuf>,
}

#[paw::main]
fn main(args: Args) {
    if let Some(input) = args.input {
        let mut files = Files::new();
        let mut writer = StandardStream::stderr(ColorChoice::Auto);
        let config = term::Config::default();
        let file_id = match std::fs::read_to_string(input.as_path()) {
            Ok(content) => files.add(input.display().to_string(), content),
            Err(err) => {
                eprintln!("Cannot read {}: {}", input.display(), err);
                return;
            }
        };
        let ctx = match parse(&files, file_id) {
            Ok(ast) => {
                let mut ctx = AstContext::default();
                for decl in ast {
                    ctx.add_decl(decl.value);
                }
                ctx
            }
            Err(err) => {
                term::emit(&mut writer, &config, &files, &err).unwrap();
                return;
            }
        };
        match ctx.binding("main") {
            Some(ast) => match ast.run(&ctx) {
                Ok(res) => println!("{}", res.display(&ctx)),
                Err(err) => {
                    let diag = ExecutionError::make_diagnostic(err);
                    term::emit(&mut writer, &config, &files, &diag).unwrap();
                }
            },
            None => {
                let diag = codespan_reporting::diagnostic::Diagnostic::error()
                    .with_message("No `main` binding found");
                term::emit(&mut writer, &config, &files, &diag).unwrap();
            }
        }
    } else {
        let mut repl = Repl::new();
        while repl.read_line() {}
    }
}
