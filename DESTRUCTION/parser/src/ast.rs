use internment::LocalIntern;
use std::str::FromStr;
use std::{collections::HashMap, fmt};

use crate::{
    error::{LangError, LangErrorT},
    parser::{Lexer, Sp},
};

type Expression = Sp<Expr>;

#[derive(Debug)]
pub enum Transformation {
    Forced {
        destruct: Expr,
        construct: Expr,
    },
    Compound(Vec<Transformation>),
    Try {
        first: Box<Transformation>,
        otherwise: Box<Transformation>,
    },
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Type {
    String,
    Number,
    Tuple,
    Array,
    Bool,
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Type::String => write!(f, "string"),
            Type::Number => write!(f, "number"),
            Type::Tuple => write!(f, "tuple"),
            Type::Array => write!(f, "array"),
            Type::Bool => write!(f, "bool"),
        }
    }
}

impl FromStr for Type {
    type Err = LangErrorT;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "string" => Ok(Type::String),
            "number" => Ok(Type::Number),
            "tuple" => Ok(Type::Tuple),
            "array" => Ok(Type::Array),
            "bool" => Ok(Type::Bool),
            _ => Err(Self::Err::SyntaxError),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Expr {
    Number(f64),
    String(String, Option<StringFlag>),
    Array(Vec<Expr>),
    Tuple(Vec<Expr>),
    Ident(LocalIntern<String>),
    PolyIdent(LocalIntern<String>),
    Operator(Operator, Box<Expr>, Box<Expr>),
    UnaryOp(UnaryOperator, Box<Expr>),
    Cast(Box<Expr>, Type, Type),
    Bool(bool),
    Call(LocalIntern<String>, Box<Expr>),
    Any, // _
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum StringFlag {
    Format,
}

#[derive(Debug, Clone)]
pub enum Operator {
    Add,
    Sub,
    Mul,
    Div,
    And,
    Or,

    Eq,
    Neq,
    Lt,
    Gt,
    Le,
    Ge,
}

#[derive(Debug, Clone)]
pub enum UnaryOperator {
    Neg,
    Not,
}

#[derive(Debug)]
pub struct TopLevel {
    pub functions: HashMap<LocalIntern<String>, Vec<Transformation>>,
}

impl FromStr for TopLevel {
    type Err = LangError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut lexer = Lexer::new(s, None);

        lexer.parse()
    }
}
