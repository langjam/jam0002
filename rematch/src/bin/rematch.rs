use std::path::PathBuf;

use codespan::Files;
use codespan_reporting::term::{
    self,
    termcolor::{ColorChoice, StandardStream},
};
use rematch::{
    loc::Located,
    parser::{parse, parse_repl, ReplParse},
    AstContext,
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
                    Ok(parse) => match parse.value {
                        ReplParse::Decl(decl) => {
                            let name = decl.name.clone().into_inner();
                            self.context.add_decl(decl);
                            let ast = self.context.declaration(&name).unwrap();
                            println!("{} = {:?}", name, ast);
                        }
                        ReplParse::Expr(expr) => {
                            let ast = self.context.make_expr(expr);
                            if let Some(r) = ast.run(&self.context) {
                                println!("-> {:?}", r);
                            } else {
                                eprintln!("Error: cannot execute");
                            }
                        }
                    },
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
        let mut writer = StandardStream::stderr(ColorChoice::Auto);
        let config = term::Config::default();
        let mut files = Files::new();
        let file_id = files.add(input.clone(), std::fs::read_to_string(input).unwrap());
        let ctx = match parse(&files, file_id) {
            Ok(ast) => {
                let mut ctx = AstContext::default();
                for Located { value: decl, .. } in ast {
                    ctx.add_decl(decl);
                }
                ctx
            }
            Err(err) => {
                term::emit(&mut writer, &config, &files, &err).unwrap();
                return;
            }
        };
        if let Some(ast) = ctx
            .declaration("main")
            .and_then(|ast| ast.clone().run(&ctx))
        {
            println!("{:?}", ast);
        }
    } else {
        let mut repl = Repl::new();
        while repl.read_line() {}
    }
}
