use crate::{
    loc::Located,
    parser::{ParseCtx, ReplParse},
    AstCtx,
};
use codespan::FileId;
use codespan_reporting::{
    diagnostic::Diagnostic,
    term::{
        self,
        termcolor::{ColorChoice, StandardStream},
    },
};
use rustyline::{error::ReadlineError, Editor};

pub struct Repl {
    rl: Editor<()>,
    context: AstCtx,
    parse_ctx: ParseCtx,
    idx: usize,
    writer: StandardStream,
    config: term::Config,
}

impl Drop for Repl {
    fn drop(&mut self) {
        self.rl.save_history(Self::HISTORY).unwrap_or_default();
    }
}

impl Repl {
    const HISTORY: &'static str = "/tmp/gtx.history";
    pub fn new() -> Self {
        Self::with_config(term::Config::default())
    }

    pub fn with_config(config: term::Config) -> Self {
        let mut rl = Editor::<()>::new();
        rl.load_history(Self::HISTORY).unwrap_or_default();

        Self {
            rl,
            context: AstCtx::default(),
            parse_ctx: ParseCtx::default(),
            idx: 0,
            writer: StandardStream::stderr(ColorChoice::Auto),
            config,
        }
    }

    pub fn read_line(&mut self) -> bool {
        let name = format!("<gtx:{}>", self.idx);
        match self.rl.readline(&name[1..]) {
            Ok(line) => {
                self.rl.add_history_entry(&line);
                self.idx += 1;
                let file_id = self.parse_ctx.add_source(name.clone(), line);
                match self.parse_ctx.parse_as_repl(file_id) {
                    Ok(ReplParse { name, expr }) => {
                        if let Some(Located { value: name, .. }) = name {
                            let ast = self.context.make_expr(expr.value);
                            println!("{} = {}", name, ast);
                            self.context.decls.insert(name.clone(), ast);
                        } else {
                            let ast = self.context.make_expr(expr.value);
                            match ast.run(&self.context) {
                                Ok(r) => println!("-> {}", r),
                                Err(err) => {
                                    let diag = Diagnostic::error()
                                        .with_message(format!("Runtime error: {}", err));
                                    self.emit_diagnostic(&diag);
                                }
                            }
                        }
                    }
                    Err(diag) => self.emit_diagnostic(&diag),
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

    fn emit_diagnostic(&mut self, diag: &Diagnostic<FileId>) {
        term::emit(&mut self.writer, &self.config, self.parse_ctx.files(), diag).unwrap()
    }
}
