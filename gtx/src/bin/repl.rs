use codespan::Files;
use codespan_reporting::term::{
    self,
    termcolor::{ColorChoice, StandardStream},
};
use gtx::{
    parser::{parse_repl, ReplParse, Spanned},
    AstContext,
};
use rustyline::{error::ReadlineError, Editor};

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
                    Ok(Spanned(_, ReplParse::Decl(decl))) => {
                        let name = decl.name.clone();
                        self.context.add_decl(decl);
                        println!("{:?}", self.context.binding(name.as_deref().into_inner()));
                    }
                    Ok(Spanned(_, ReplParse::Expr(expr))) => {
                        let ast = self.context.make_expr(expr);
                        println!("{:?}", ast);
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

fn main() {
    let mut repl = Repl::new();
    while repl.read_line() {}
}
