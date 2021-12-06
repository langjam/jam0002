use std::{ffi::OsString, path::Path};

use codespan::{FileId, Files};
use codespan_reporting::diagnostic::{Diagnostic, Label};
use lalrpop_util::{lalrpop_mod, lexer::Token, ParseError};

use crate::{
    ast::{Decl, Expr},
    loc::Located,
};

lalrpop_mod!(grammar);

pub struct ReplParse {
    pub name: Option<Located<String>>,
    pub expr: Located<Expr>,
}

#[derive(Default)]
pub struct ParseCtx {
    files: Files<String>,
}

impl ParseCtx {
    pub fn add_source(&mut self, name: impl Into<OsString>, source: impl Into<String>) -> FileId {
        self.files.add(name, source.into())
    }

    pub fn add_file(&mut self, path: &Path) -> std::io::Result<FileId> {
        let source = std::fs::read_to_string(path)?;
        Ok(self.add_source(path, source))
    }

    pub fn parse_full(
        &mut self,
        file_id: FileId,
    ) -> Result<Vec<Located<Decl>>, Diagnostic<FileId>> {
        grammar::ToplevelParser::new()
            .parse(file_id, self.files.source(file_id).as_str())
            .map_err(|err| make_diagnostic(file_id, err))
    }

    pub fn parse_as_repl(&mut self, file_id: FileId) -> Result<ReplParse, Diagnostic<FileId>> {
        grammar::ReplParser::new()
            .parse(file_id, self.files.source(file_id).as_str())
            .map_err(|err| make_diagnostic(file_id, err))
    }

    pub(crate) fn files(&self) -> &Files<String> {
        &self.files
    }
}

pub fn parse(name: &str, source: &str) -> Result<ReplParse, Diagnostic<FileId>> {
    let mut ctx = ParseCtx::default();
    let file_id = ctx.add_source(name, source);
    ctx.parse_as_repl(file_id)
}

fn make_diagnostic<'a>(
    file_id: FileId,
    err: ParseError<usize, Token<'a>, &'a str>,
) -> Diagnostic<FileId> {
    let diag = Diagnostic::error().with_message("Parse error");
    match err {
        ParseError::InvalidToken { location } => {
            diag.with_labels(vec![
                Label::primary(file_id, location..location + 1).with_message("Invalid token")
            ])
        }
        ParseError::UnrecognizedEOF { location, expected } => diag.with_labels(vec![
            Label::primary(file_id, location..location + 1).with_message("Unexpected end of file"),
            Label::secondary(file_id, location..location + 1)
                .with_message(format!("Expecting one of {}", expected.join(", "))),
        ]),
        ParseError::UnrecognizedToken {
            token: (start, value, end),
            expected,
        } => diag.with_labels(vec![
            Label::primary(file_id, start..end).with_message(format!("Unexpected token {}", value)),
            Label::secondary(file_id, start..end)
                .with_message(format!("Expecting one of {}", expected.join(", "))),
        ]),
        ParseError::ExtraToken {
            token: (start, value, end),
        } => diag
            .with_labels(vec![Label::primary(file_id, start..end)
                .with_message(format!("Unexpected input {}", value))]),
        ParseError::User { .. } => unreachable!(),
    }
}
