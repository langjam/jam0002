use std::ops::{Deref, Range};

use codespan::FileId;
use codespan_reporting::diagnostic::{Label, LabelStyle};

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
pub struct Located<T> {
    pub file_id: FileId,
    pub span: Span,
    pub value: T,
}

impl<T> std::ops::Deref for Located<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

impl<T> Located<T> {
    pub fn map<U>(self, map: impl FnOnce(T) -> U) -> Located<U> {
        Located {
            value: map(self.value),
            file_id: self.file_id,
            span: self.span,
        }
    }

    pub fn and_then<U>(self, then: impl FnOnce(T) -> Located<U>) -> Located<U> {
        self.map(then).flatten()
    }

    pub fn as_ref(&self) -> Located<&T> {
        Located {
            value: &self.value,
            file_id: self.file_id,
            span: self.span,
        }
    }

    pub fn as_deref(&self) -> Located<&<T as Deref>::Target>
    where
        T: Deref,
    {
        Located {
            value: self.value.deref(),
            file_id: self.file_id,
            span: self.span,
        }
    }

    pub fn file_id(&self) -> FileId {
        self.file_id
    }

    pub fn span(&self) -> Span {
        self.span
    }

    pub fn into_inner(self) -> T {
        self.value
    }
}

impl<T> Located<Located<T>> {
    pub fn flatten(self) -> Located<T> {
        Located {
            span: self.span.merge(self.value.span),
            file_id: self.value.file_id,
            value: self.value.value,
        }
    }
}

impl<T> Located<Option<T>> {
    pub fn transpose_option(self) -> Option<Located<T>> {
        self.value.map(|value| Located {
            value,
            span: self.span,
            file_id: self.file_id,
        })
    }
}

impl<T, E> Located<Result<T, E>> {
    pub fn transpose_result(self) -> Result<Located<T>, Located<E>> {
        match self.value {
            Ok(value) => Ok(Located {
                value,
                file_id: self.file_id,
                span: self.span,
            }),
            Err(value) => Err(Located {
                value,
                file_id: self.file_id,
                span: self.span,
            }),
        }
    }
}

impl<T: ToString> Located<T> {
    pub fn to_label(self, style: LabelStyle) -> Label<FileId> {
        Label::new(style, self.file_id, self.span).with_message(self.value.to_string())
    }
}

impl<'a, T: Clone> Located<&'a T> {
    pub fn cloned(self) -> Located<T> {
        self.map(|x| x.clone())
    }
}

impl<'a, T: Copy> Located<&'a T> {
    pub fn copied(self) -> Located<T> {
        self.map(|&x| x)
    }
}
