use std::{
    io::{self, Read},
    path::Path,
};

use codespan::FileId;
use codespan_reporting::{
    diagnostic::Diagnostic,
    term::{self, termcolor::StandardStream},
};

use crate::{parser::ParseCtx, Ast, AstCtx};

pub struct Compiler {
    parse_ctx: ParseCtx,
    ast_ctx: AstCtx,
    writer: StandardStream,
    config: term::Config,
    main_file_id: Option<FileId>,
}

impl Compiler {
    pub fn from_stdin() -> io::Result<Self> {
        let mut this = Self::init();
        let source = {
            let reader = io::stdin();
            let mut reader = reader.lock();
            let mut buf = String::new();
            reader.read_to_string(&mut buf)?;
            buf
        };
        this.main_file_id = Some(this.parse_ctx.add_source("<stdin>", source));
        Ok(this)
    }

    pub fn from_file(path: &Path) -> io::Result<Self> {
        log::debug!("Compiler from {}", path.display());
        let mut this = Self::init();
        this.main_file_id = this.parse_file(path)?;
        Ok(this)
    }

    pub fn get_entrypoint(&self) -> Option<&Ast> {
        self.ast_ctx.decl("main")
    }

    pub fn execute(&mut self) -> Option<Ast> {
        match self.get_entrypoint() {
            Some(main) => match main.clone().run(&self.ast_ctx) {
                Ok(res) => Some(res),
                Err(err) => {
                    let diag = Diagnostic::error().with_message(format!("Runtime error: {}", err));
                    self.emit_diagnostic(&diag);
                    None
                }
            },
            None => {
                let diag = Diagnostic::error().with_message("entrypoing `main` binding not found");
                self.emit_diagnostic(&diag);
                None
            }
        }
    }

    fn parse_file(&mut self, path: &Path) -> io::Result<Option<FileId>> {
        let file_id = self.parse_ctx.add_file(path)?;
        log::debug!("Parsing {} -> {:?}", path.display(), file_id);

        match self.parse_ctx.parse_full(file_id) {
            Ok(ast) => {
                for decl in ast {
                    log::debug!("[{:?}] Adding declaraion for {}", file_id, decl.name.value);
                    self.ast_ctx.add_decl(decl.value);
                }
                Ok(Some(file_id))
            }
            Err(err) => {
                self.emit_diagnostic(&err);
                Ok(None)
            }
        }
    }

    fn init() -> Self {
        Self {
            parse_ctx: ParseCtx::default(),
            ast_ctx: AstCtx::default(),
            writer: StandardStream::stderr(term::termcolor::ColorChoice::Auto),
            config: term::Config::default(),
            main_file_id: None,
        }
    }

    fn emit_diagnostic(&mut self, diag: &Diagnostic<FileId>) {
        term::emit(&mut self.writer, &self.config, self.parse_ctx.files(), diag).unwrap()
    }
}
