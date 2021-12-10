use std::{collections::HashMap, fmt::Display};

use parser::{
    ast::{Expr, Transformation},
    internment::LocalIntern,
};

use crate::error::RuntimeError;

#[derive(Clone, PartialEq, Debug)]
pub enum Value {
    Number(f64),
    String(String),
    Array(Vec<Value>),
    Tuple(Vec<Value>),
    Bool(bool),
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Value::Number(n) => write!(f, "{}", n),
            Value::String(s) => write!(f, "\"{}\"", s),
            Value::Array(a) => {
                write!(f, "[")?;
                for (i, v) in a.iter().enumerate() {
                    if i != 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", v)?;
                }
                write!(f, "]")
            }
            Value::Tuple(a) => {
                write!(f, "(")?;
                for (i, v) in a.iter().enumerate() {
                    if i != 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", v)?;
                }
                write!(f, ")")
            }
            Value::Bool(b) => write!(f, "{}", b),
        }
    }
}

pub trait Maths {
    fn add(&self, other: &Self) -> Result<Value, RuntimeError>;
    fn sub(&self, other: &Self) -> Result<Value, RuntimeError>;
    fn div(&self, other: &Self) -> Result<Value, RuntimeError>;
    fn and(&self, other: &Self) -> Result<Value, RuntimeError>;
    fn or(&self, other: &Self) -> Result<Value, RuntimeError>;

    fn lt_op(&self, other: &Self) -> Result<Value, RuntimeError>;
    fn gt_op(&self, other: &Self) -> Result<Value, RuntimeError>;
    fn le_op(&self, other: &Self) -> Result<Value, RuntimeError>;
    fn ge_op(&self, other: &Self) -> Result<Value, RuntimeError>;
}

#[derive(Debug)]
pub struct Variables {
    idents: HashMap<LocalIntern<String>, Value>,
    pub polyidents: HashMap<LocalIntern<String>, Vec<Value>>,
}

pub type Functions = HashMap<LocalIntern<String>, Vec<Transformation>>;

pub enum DestructResult {
    Known(Value),
    Partial(PartialValue),
    Unknown,
}

#[derive(Clone)]
pub enum PartialStringPart {
    String(String),
    Expr(Expr),
}

pub enum PartialValue {
    Array {
        len: Option<usize>,
        known_elems: HashMap<usize, PartialValue>,
    },
    //String(Vec<PartialStringPart>),
    Value(Value),
}

impl PartialValue {
    pub fn to_value(&self) -> Result<Value, RuntimeError> {
        match self {
            PartialValue::Array { len, known_elems } => {
                let len = len.ok_or(RuntimeError::ValueError(
                    "Array length is not known".to_string(),
                ))?;
                let mut vals = Vec::with_capacity(len);
                for i in 0..len {
                    if let Some(val) = known_elems.get(&i) {
                        vals.push(val.to_value()?);
                    } else {
                        return Err(RuntimeError::ValueError(format!(
                            "Array element {} is not known",
                            i
                        )));
                    }
                }
                Ok(Value::Array(vals))
            }
            PartialValue::Value(v) => Ok(v.clone()),
            //PartialValue::String(_) => todo!(),
        }
    }
}

pub trait Structure {
    fn construct(
        &self,
        variables: &mut Variables,
        functions: &Functions,
    ) -> Result<Value, RuntimeError>;

    fn destruct_to_value(
        &self,
        functions: &Functions,
        variables: &Variables,
    ) -> Result<DestructResult, RuntimeError>;

    fn destruct(
        &self,
        value: &Value,
        variables: &mut Variables,
        functions: &Functions,
    ) -> Result<Option<Value>, RuntimeError>;
}

impl Variables {
    pub fn new() -> Variables {
        Variables {
            idents: HashMap::new(),
            polyidents: HashMap::new(),
        }
    }
    pub fn insert(&mut self, key: LocalIntern<String>, value: Value) -> Result<(), RuntimeError> {
        if let Some(a) = self.idents.get(&key) {
            if a != &value {
                Err(RuntimeError::ValueError(format!(
                    "Variable {} already has a value different from {}",
                    key, value
                )))
            } else {
                Ok(())
            }
        } else {
            self.idents.insert(key, value);
            Ok(())
        }
    }
    pub fn get(&self, i: LocalIntern<String>) -> Option<&Value> {
        self.idents.get(&i)
    }

    pub fn insert_polyident(
        &mut self,
        key: LocalIntern<String>,
        value: Value,
    ) -> Result<(), RuntimeError> {
        if let Some(a) = self.polyidents.get_mut(&key) {
            a.push(value);
            Ok(())
        } else {
            self.polyidents.insert(key, vec![value]);
            Ok(())
        }
    }

    pub fn take_polyident(
        &mut self,
        i: LocalIntern<String>,
    ) -> Result<Option<Value>, RuntimeError> {
        if let Some(v) = self.polyidents.get_mut(&i) {
            if v.is_empty() {
                Err(RuntimeError::ValueError(format!(
                    "Polyident {} is used up",
                    i
                )))
            } else {
                Ok(Some(v.remove(0)))
            }
        } else {
            Ok(None)
        }
    }
}

impl Default for Variables {
    fn default() -> Self {
        Self::new()
    }
}
impl DestructResult {
    pub fn unwrap(self) -> Value {
        if let DestructResult::Known(v) = self {
            v
        } else {
            panic!("DestructResult is not known")
        }
    }
}
