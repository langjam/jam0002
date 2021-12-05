use std::rc::Rc;

use crate::loc::Located;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Primary {
    Const(String),
    Var(String),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Pattern {
    Primary(Primary),
    Constructor {
        name: Located<String>,
        args: Vec<Located<Self>>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Clause {
    pub recursive: bool,
    pub pattern: Located<Pattern>,
    pub body: Located<Expr>,
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
        lhs: Located<Rc<Self>>,
        rhs: Located<Rc<Self>>,
        op: Located<Binop>,
    },
    Match {
        on: Located<Rc<Self>>,
        arms: Vec<Clause>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Decl {
    pub name: Located<String>,
    pub body: Located<Expr>,
}
