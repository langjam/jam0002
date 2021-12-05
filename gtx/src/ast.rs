use std::rc::Rc;

use crate::parser::Spanned;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Primary {
    Const(String),
    Var(String),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Pattern {
    Primary(Primary),
    Constructor {
        name: Spanned<String>,
        args: Vec<Spanned<Self>>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Clause {
    pub recursive: bool,
    pub pattern: Spanned<Pattern>,
    pub body: Spanned<Expr>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Binop {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    Primary(Primary),
    Binop {
        lhs: Spanned<Rc<Self>>,
        rhs: Spanned<Rc<Self>>,
        op: Spanned<Binop>,
    },
    Match {
        on: Spanned<Rc<Self>>,
        arms: Vec<Clause>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Decl {
    pub name: Spanned<String>,
    pub body: Spanned<Expr>,
}
