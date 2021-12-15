use std::{
    fmt,
    ops::{Deref, Range},
};

use codespan::FileId;
use codespan_reporting::diagnostic::{Label, LabelStyle};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
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

impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.0, self.1)
    }
}

impl Span {
    pub fn merge(self, other: Self) -> Self {
        Self(self.0.min(other.0), self.1.max(other.1))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Location {
    pub span: Span,
    pub file_id: FileId,
}

impl Location {
    pub fn merge(self, other: Self) -> Option<Self> {
        if self.file_id == other.file_id {
            Some(Self {
                span: self.span.merge(other.span),
                file_id: self.file_id,
            })
        } else {
            None
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Located<T> {
    pub loc: Option<Location>,
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
            loc: self.loc,
        }
    }

    pub fn and_then<U>(self, then: impl FnOnce(T) -> Located<U>) -> Located<U> {
        let Located { value, loc } = then(self.value);
        Located {
            value,
            loc: self.loc.and_then(|a| a.merge(loc?)),
        }
    }

    pub fn as_ref(&self) -> Located<&T> {
        Located {
            loc: self.loc,
            value: &self.value,
        }
    }

    pub fn file_id(&self) -> Option<FileId> {
        Some(self.loc?.file_id)
    }

    pub fn span(&self) -> Option<Span> {
        Some(self.loc?.span)
    }
}

impl<T> Located<Located<T>> {
    pub fn flatten(self) -> Located<T> {
        self.and_then(|x| x)
    }
}

impl<T> Located<Option<T>> {
    pub fn transpose(self) -> Option<Located<T>> {
        self.value.map(|value| Located {
            value,
            loc: self.loc,
        })
    }
}

impl<T, E> Located<Result<T, E>> {
    pub fn transpose(self) -> Result<Located<T>, Located<E>> {
        match self.value {
            Ok(value) => Ok(Located {
                value,
                loc: self.loc,
            }),
            Err(value) => Err(Located {
                value,
                loc: self.loc,
            }),
        }
    }
}

impl<T: ToString> Located<T> {
    pub fn to_label(self, style: LabelStyle) -> Option<Label<FileId>> {
        self.loc.map(|Location { span, file_id }| {
            Label::new(style, file_id, span).with_message(self.value.to_string())
        })
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

impl<T: Deref> Located<T> {
    pub fn as_deref(&self) -> Located<&T::Target> {
        self.as_ref().map(Deref::deref)
    }
}

pub trait LocatedExt: Sized {
    fn located(self, loc: impl Into<Option<Location>>) -> Located<Self> {
        Located {
            value: self,
            loc: loc.into(),
        }
    }
}
impl<T> LocatedExt for T {}
