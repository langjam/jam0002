#![forbid(unused_must_use)]

mod lexer;
mod interpreter;
mod muncher;
mod munch_trie;
mod pretty_errors;

use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::path::Path;
use std::rc::Rc;
use codespan_reporting::term::termcolor::{NoColor, StandardStream, ColorChoice};

use crate::interpreter::{Env, Interpreter};
use crate::lexer::Token;
use crate::muncher::Muncher;

type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, Clone)]
pub(crate) enum Value {
    Nil,
    Int(i64),
    Bool(bool),
    String(Rc<[char]>),
    Ident(Rc<str>),
    Block(Rc<Block>),
    Object(Rc<Object>),
}

impl From<bool> for Value {
    fn from(v: bool) -> Self {
        Self::Bool(v)
    }
}

impl From<i64> for Value {
    fn from(v: i64) -> Self {
        Self::Int(v)
    }
}

impl Value {
    fn get_muncher(&self) -> Rc<dyn Muncher> {
        match self {
            Value::Int(i) => Rc::new(muncher::NumMuncher { value: *i }),
            Value::Bool(b) => Rc::new(muncher::BoolMuncher { value: *b }),
            Value::Block(b) => Rc::new(muncher::BlockMuncher { value: b.clone() }),
            Value::Object(o) => o.muncher.clone(),
            Value::String(s) => Rc::new(muncher::StringMuncher { value: s.clone() }),
            _ => Rc::new(muncher::NoMuncher),
        }
    }

    fn on_field(&self, field: &str, f: impl FnOnce(&mut Value)) {
        if let Value::Object(o) = self {
            if let Some(prop) = o.properties.borrow_mut().get_mut(field) {
                f(prop);
            }
        }
    }

    fn has_field(&self, field: &str) -> bool {
        let mut has = false;
        self.on_field(field, |_| has = true);
        has
    }

    fn type_name(&self) -> String {
        match self {
            Value::Nil => "<Nil>".to_owned(),
            Value::Int(_) => "<Int>".to_owned(),
            Value::Bool(_) => "<Bool>".to_owned(),
            Value::String(_) => "<String>".to_owned(),
            Value::Ident(_) => "<Ident>".to_owned(),
            Value::Block(_) => "<Block>".to_owned(),
            Value::Object(o) => match &o.name {
                Some(name) => format!("<Object {}>", name),
                None => "<Object>".to_owned(),
            },
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Nil => write!(f, "nil"),
            Value::Int(i) => write!(f, "{}", i),
            Value::Bool(b) => write!(f, "{}", b),
            Value::String(s) => write!(f, "{}", s.iter().copied().collect::<String>()),
            Value::Ident(i) => write!(f, "<Ident {}>", i),
            Value::Block(_) => write!(f, "<Block>"),
            Value::Object(o) => match &o.name {
                Some(name) => write!(f, "<Object {}>", name),
                None => write!(f, "<Object>"),
            },
        }
    }
}

#[derive(Debug, Clone)]
enum Block {
    Source(SourceBlock),
    Intrinsic(Intrinsic),
}

#[derive(Clone)]
struct SourceBlock {
    closure: Env,
    tokens: Rc<[Token]>,
}

impl fmt::Debug for SourceBlock {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("SourceBlock")
            .field("tokens", &self.tokens)
            .finish()
    }
}

#[derive(Debug, Clone)]
enum Intrinsic {
    Print(String),
    Value(Value),
}

struct Object {
    name: Option<Rc<str>>,
    properties: RefCell<HashMap<Rc<str>, Value>>,
    muncher: Rc<dyn Muncher>,
}

impl fmt::Debug for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("Object")
            .field("name", &self.name)
            .field("properties", &self.properties)
            .finish()
    }
}

#[derive(Debug, Clone)]
pub struct Error {
    pub msg: String,
    pub span: Span,
    pub notes: Vec<Note>,
}

impl Error {
    pub fn pretty(&self, source_name: &Path, source: &str) -> String {
        let mut result = Vec::new();
        pretty_errors::emit(self, source_name, source, &mut NoColor::new(&mut result));
        String::from_utf8(result).unwrap()
    }

    pub fn to_stderr(&self, source_name: &Path, source: &str) {
        pretty_errors::emit(self, source_name, source, &mut StandardStream::stderr(ColorChoice::Auto));
    }
}

#[derive(Debug, Clone)]
pub struct Note {
    pub msg: String,
    pub span: Span,
}

#[derive(Debug, Clone, Copy)]
pub struct Span {
    pub start: Pos,
    pub end: Pos,
}

impl Span {
    pub(crate) fn union(self, other: Span) -> Span {
        Span {
            start: self.start,
            end: other.end,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Pos {
    pub line: u32,
    pub col: u32,
    pub(crate) offset: usize,
}

pub trait Intrinsics {
    fn print(&self, value: &str);
}

pub fn eval(source: &str, intrinsics: Rc<dyn Intrinsics>) -> Result<()> {
    let tokens = lexer::lex_program(source)?;
    let env = Env::new();
    env.define_raw("print", Value::Object(Rc::new(Object {
        name: Some("Print".into()),
        properties: Default::default(),
        muncher: Rc::new(muncher::PrintMuncher),
    })));
    let mut interp = Interpreter {
        env: env.clone(),
        intrinsics,
    };
    interp.block_with_env(
        &Block::Source(SourceBlock {
            closure: env.clone(),
            tokens: tokens.into(),
        }),
        env,
    )?;
    Ok(())
}

#[test]
fn basic_test() {
    struct Intr;
    impl Intrinsics for Intr {
        fn print(&self, _: &str) {}
    }
    eval(
        r#" print("Hello, world!"); "#,
        Rc::new(Intr),
    ).unwrap();
}
