use std::{
    hash::Hash,
    ops::{Deref, Range},
};

use codespan::{FileId, Files};
use codespan_reporting::diagnostic::{Diagnostic, Label};
use lalrpop_util::{lalrpop_mod, lexer::Token, ParseError};

use crate::ast::Decl;

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Span(pub usize, pub usize);

impl From<Range<usize>> for Span {
    fn from(r: Range<usize>) -> Self {
        Self(r.start, r.end)
    }
}

impl Into<Range<usize>> for Span {
    fn into(self) -> Range<usize> {
        self.0..self.1
    }
}

impl Span {
    pub fn merge(self, other: Self) -> Self {
        Self(self.0.min(other.0), self.1.max(other.1))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Spanned<T>(pub Span, pub T);

impl<T> std::ops::Deref for Spanned<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.1
    }
}

impl<T> Spanned<T> {
    pub fn map<U>(self, map: impl FnOnce(T) -> U) -> Spanned<U> {
        Spanned(self.0, map(self.1))
    }

    pub fn as_ref(&self) -> Spanned<&T> {
        Spanned(self.0, &self.1)
    }

    pub fn as_deref(&self) -> Spanned<&<T as Deref>::Target>
    where
        T: Deref,
    {
        Spanned(self.0, self.1.deref())
    }

    pub fn span(&self) -> Span {
        self.0
    }

    pub fn into_inner(self) -> T {
        self.1
    }
}

impl<T, E> Spanned<Result<T, E>> {
    pub fn transpose_result(self) -> Result<Spanned<T>, Spanned<E>> {
        match self.1 {
            Ok(x) => Ok(Spanned(self.0, x)),
            Err(err) => Err(Spanned(self.0, err)),
        }
    }
}

lalrpop_mod!(grammar);

pub fn parse<S: AsRef<str>>(
    files: &Files<S>,
    file_id: FileId,
) -> Result<Vec<Spanned<Decl>>, Diagnostic<FileId>> {
    let source = files.source(file_id).as_ref();
    grammar::ToplevelParser::new()
        .parse(source)
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
) -> Result<Spanned<ReplParse>, Diagnostic<FileId>> {
    let source = files.source(file_id).as_ref();
    grammar::ReplParser::new()
        .parse(source)
        .map_err(|err| make_diagnostic(file_id, err))
}
