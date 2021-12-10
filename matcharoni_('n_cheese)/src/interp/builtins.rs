use crate::interp::{Interpreter, Pattern, Value};
use dyn_partial_eq::DynPartialEq;
use gc::{Gc, GcCell};
use itertools::Itertools;
use std::fmt::Write;
use std::fs::read_to_string;

pub fn builtins() -> Vec<(&'static str, Box<dyn Pattern>)> {
    vec![
        ("read_to_string", Box::new(ReadToString {})),
        ("dbg", Box::new(DebugPrint {})),
        ("push", Box::new(Push {})),
        ("exit", Box::new(Exit {})),
    ]
}

#[derive(Debug, Clone, PartialEq, DynPartialEq)]
pub struct ReadToString {}

impl Pattern for ReadToString {
    fn name(&self) -> &str {
        "read_to_string"
    }

    fn match_full(&self, _: &mut Interpreter, arg: Gc<Value>) -> anyhow::Result<Gc<Value>> {
        let filename = arg.collect_string()?;
        Ok(Gc::new(Value::List(Gc::new(GcCell::new(
            read_to_string(filename)?
                .chars()
                .map(|c| Gc::new(GcCell::new(Gc::new(Value::Char(c)))))
                .collect(),
        )))))
    }
}

#[derive(Debug, Clone, PartialEq, DynPartialEq)]
pub struct DebugPrint {}

impl Pattern for DebugPrint {
    fn name(&self) -> &str {
        "dbg"
    }

    fn match_full(&self, _: &mut Interpreter, arg: Gc<Value>) -> anyhow::Result<Gc<Value>> {
        println!("{}", print(&arg));
        Ok(arg)
    }
}

pub fn print(arg: &Value) -> String {
    let mut s = String::new();
    match arg {
        Value::Void => write!(&mut s, "()"),
        Value::Char(c) => write!(&mut s, "'{}'", c),
        Value::Tuple(vals) => {
            write!(&mut s, "(").unwrap();
            if let Some((last, rest)) = vals.split_last() {
                for val in rest {
                    write!(&mut s, "{}", print(val)).unwrap();
                    write!(&mut s, ", ").unwrap();
                }
                write!(&mut s, "{}", print(last)).unwrap();
            }
            write!(&mut s, ")")
        }
        Value::List(vals) => {
            write!(&mut s, "[").unwrap();
            if let Some((last, rest)) = vals.borrow().split_last() {
                for val in rest {
                    write!(&mut s, "{}", print(&*val.borrow())).unwrap();
                    write!(&mut s, ", ").unwrap();
                }
                write!(&mut s, "{}", print(&*last.borrow())).unwrap();
            }
            write!(&mut s, "]")
        }
        Value::Int(n) => write!(&mut s, "{}", n),
        Value::Pattern(pat) => write!(&mut s, "<Pat {}>", pat.name()),
    }
    .unwrap();
    s
}

#[derive(Debug, Clone, PartialEq, DynPartialEq)]
pub struct Push {}

impl Pattern for Push {
    fn name(&self) -> &str {
        "push"
    }

    fn match_full(&self, _: &mut Interpreter, arg: Gc<Value>) -> anyhow::Result<Gc<Value>> {
        let tuple = arg.as_tuple()?;
        let (list, to_add) = tuple
            .into_iter()
            .collect_tuple()
            .ok_or_else(|| anyhow::anyhow!("wrong arguments"))?;
        let list = list.as_list_cell()?;
        list.borrow_mut().push(Gc::new(GcCell::new(to_add.clone())));
        Ok(Gc::new(Value::List(list)))
    }
}

#[derive(Debug, Clone, PartialEq, DynPartialEq)]
pub struct Exit {}

impl Pattern for Exit {
    fn name(&self) -> &str {
        "exit"
    }

    fn match_full(&self, _: &mut Interpreter, arg: Gc<Value>) -> anyhow::Result<Gc<Value>> {
        std::process::exit(arg.as_int()? as _);
    }
}
