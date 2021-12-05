use codespan::{FileId, Files};
use codespan_reporting::diagnostic::{Diagnostic, Label};
use lalrpop_util::{lalrpop_mod, lexer::Token, ParseError};

use crate::ast::Decl;
use crate::loc::Located;

lalrpop_mod!(grammar);

pub fn parse<S: AsRef<str>>(
    files: &Files<S>,
    file_id: FileId,
) -> Result<Vec<Located<Decl>>, Diagnostic<FileId>> {
    let source = files.source(file_id).as_ref();
    grammar::ToplevelParser::new()
        .parse(file_id, source)
        .map_err(|err| make_diagnostic(file_id, err))
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ReplParse {
    Decl(super::ast::Decl),
    Expr(super::ast::Expr),
}

pub fn parse_repl<S: AsRef<str>>(
    files: &Files<S>,
    file_id: FileId,
) -> Result<Located<ReplParse>, Diagnostic<FileId>> {
    let source = files.source(file_id).as_ref();
    grammar::ReplParser::new()
        .parse(file_id, source)
        .map_err(|err| make_diagnostic(file_id, err))
}
