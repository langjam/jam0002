use std::collections::HashMap;

use parser::ast::{Transformation, Type, UnaryOperator};
use parser::internment::LocalIntern;

use crate::error::RuntimeError;
use crate::traits::{DestructResult, Functions, Maths, PartialValue, Structure, Value, Variables};
use parser::ast::Transformation::Forced;
use parser::ast::{Expr, TopLevel};

use crate::destruct_algebra;

impl Value {
    fn to_type(&self) -> &Type {
        match self {
            Value::String(_) => &Type::String,
            Value::Number(_) => &Type::Number,
            Value::Tuple(_) => &Type::Tuple,
            Value::Array(_) => &Type::Array,
            Value::Bool(_) => &Type::Bool,
        }
    }
    fn cast(&self, to: &Type, from: &Type) -> Result<Value, RuntimeError> {
        if from != self.to_type() {
            return Err(RuntimeError::TypeMismatch(
                from.to_string(),
                self.to_type().to_string(),
            ));
        }
        if to == self.to_type() {
            return Ok(self.clone());
        }
        match (to, self) {
            (Type::Number, Value::String(s)) => {
                Ok(Self::Number(s.parse::<f64>().ok().unwrap_or(f64::NAN)))
            }
            (Type::Number, Value::Array(_) | Value::Tuple(_)) => Err(RuntimeError::ValueError(
                "Cannot convert array or tuple to number".to_string(),
            )),
            (Type::Array | Type::Tuple, Value::Number(_)) => Err(RuntimeError::ValueError(
                "Cannot convert number to array or tuple".to_string(),
            )),
            (Type::String, v) => Ok(Self::String(format!("{}", v))),
            (Type::Array, Value::String(s)) => Ok(Self::Array(
                s.chars().map(|x| Value::String(String::from(x))).collect(),
            )),
            (Type::Tuple, Value::String(s)) => Ok(Self::Tuple(
                s.chars().map(|x| Value::String(String::from(x))).collect(),
            )),
            // boolean casting?
            _ => unreachable!(),
        }
    }
}

impl Maths for Value {
    fn add(&self, other: &Self) -> Result<Value, RuntimeError> {
        match (self, other) {
            (Value::Number(lhs), Value::Number(rhs)) => Ok(Value::Number(lhs + rhs)),
            (Value::String(lhs), Value::String(rhs)) => Ok(Value::String(lhs.to_owned() + rhs)),
            (Value::Array(lhs), Value::Array(rhs)) => {
                Ok(Value::Array([lhs.to_owned(), rhs.to_owned()].concat()))
            }
            (a, b) => Err(RuntimeError::ValueError(format!(
                "Cannot add {:?} and {:?}",
                a, b
            ))),
        }
    }

    fn sub(&self, other: &Self) -> Result<Value, RuntimeError> {
        match (self, other) {
            (Value::Number(lhs), Value::Number(rhs)) => Ok(Value::Number(lhs - rhs)),
            (a, b) => Err(RuntimeError::ValueError(format!(
                "Cannot subtract {:?} and {:?}",
                a, b
            ))),
        }
    }

    fn div(&self, other: &Self) -> Result<Value, RuntimeError> {
        match (self, other) {
            (Value::Number(lhs), Value::Number(rhs)) => Ok(Value::Number(lhs / rhs)),
            (Value::String(lhs), Value::String(rhs)) => Ok(Value::Array(
                lhs.to_owned()
                    .split(rhs)
                    .map(|a| Value::String(a.to_string()))
                    .collect(),
            )),
            _ => Err(RuntimeError::ValueError(format!(
                "Cannot divide {:?} and {:?}",
                self, other
            ))),
        }
    }

    fn and(&self, other: &Self) -> Result<Value, RuntimeError> {
        match (self, other) {
            (Value::Bool(lhs), Value::Bool(rhs)) => Ok(Value::Bool(*lhs && *rhs)),
            _ => Err(RuntimeError::ValueError(format!(
                "Cannot and {:?} and {:?}",
                self, other
            ))),
        }
    }

    fn or(&self, other: &Self) -> Result<Value, RuntimeError> {
        match (self, other) {
            (Value::Bool(lhs), Value::Bool(rhs)) => Ok(Value::Bool(*lhs || *rhs)),
            _ => Err(RuntimeError::ValueError(format!(
                "Cannot or {:?} and {:?}",
                self, other
            ))),
        }
    }

    fn lt_op(&self, other: &Self) -> Result<Value, RuntimeError> {
        match (self, other) {
            (Value::Number(lhs), Value::Number(rhs)) => Ok(Value::Bool(lhs < rhs)),
            (a, b) => Err(RuntimeError::ValueError(format!(
                "Cannot compare {:?} and {:?}",
                a, b
            ))),
        }
    }

    fn gt_op(&self, other: &Self) -> Result<Value, RuntimeError> {
        match (self, other) {
            (Value::Number(lhs), Value::Number(rhs)) => Ok(Value::Bool(lhs > rhs)),
            (a, b) => Err(RuntimeError::ValueError(format!(
                "Cannot compare {:?} and {:?}",
                a, b
            ))),
        }
    }

    fn le_op(&self, other: &Self) -> Result<Value, RuntimeError> {
        match (self, other) {
            (Value::Number(lhs), Value::Number(rhs)) => Ok(Value::Bool(lhs <= rhs)),
            (a, b) => Err(RuntimeError::ValueError(format!(
                "Cannot compare {:?} and {:?}",
                a, b
            ))),
        }
    }

    fn ge_op(&self, other: &Self) -> Result<Value, RuntimeError> {
        match (self, other) {
            (Value::Number(lhs), Value::Number(rhs)) => Ok(Value::Bool(lhs >= rhs)),
            (a, b) => Err(RuntimeError::ValueError(format!(
                "Cannot compare {:?} and {:?}",
                a, b
            ))),
        }
    }
}

fn mul(
    left: &Expr,
    right: &Value,
    variables: &mut Variables,
    functions: &Functions,
) -> Result<Value, RuntimeError> {
    let factor = *match right {
        Value::Number(n) => n,
        _ => {
            return Err(RuntimeError::TypeMismatch(
                "number".to_string(),
                right.to_type().to_string(),
            ))
        }
    };
    let mut out = left.construct(variables, functions)?;
    if let Value::Number(n) = out {
        return Ok(Value::Number(n * factor));
    }
    if factor.fract() != 0.0 {
        return Err(RuntimeError::ValueError(
            "Can only multiply numbers by fractional number".to_string(),
        ));
    }
    let n = factor as usize;
    for _ in 0..(n - 1) {
        out = out.add(&left.construct(variables, functions)?)?;
    }
    Ok(out)
}

pub fn interpret(top_level: TopLevel, input: Value) -> Result<Value, RuntimeError> {
    run_func(
        LocalIntern::new("main".to_string()),
        input,
        &top_level.functions,
    )
}

fn run_func(
    func: LocalIntern<String>,
    value: Value,
    functions: &Functions,
) -> Result<Value, RuntimeError> {
    let transforms = functions
        .get(&func)
        .ok_or_else(|| RuntimeError::ValueError(format!("Missing `{}` function", func)))?;
    run_tranforms(transforms, value, functions)
}

fn run_tranforms(
    transforms: &[Transformation],
    mut value: Value,
    functions: &HashMap<LocalIntern<String>, Vec<parser::ast::Transformation>>,
) -> Result<Value, RuntimeError> {
    for trans in transforms {
        value = run_single_transform(trans, value, functions)?
    }
    Ok(value)
}

fn run_single_transform(
    trans: &Transformation,
    value: Value,
    functions: &HashMap<LocalIntern<String>, Vec<Transformation>>,
) -> Result<Value, RuntimeError> {
    Ok(match trans {
        Forced {
            destruct,
            construct,
        } => {
            let mut env = Variables::new();
            destruct.destruct(&value, &mut env, functions)?;
            let out = construct.construct(&mut env, functions)?;

            for (name, value) in env.polyidents.iter() {
                if !value.is_empty() {
                    return Err(RuntimeError::ValueError(format!(
                        "Polyident {} was used more times in the destruct pattern than in the construct pattern",
                        name
                    )));
                }
            }
            out
        }
        parser::ast::Transformation::Compound(v) => run_tranforms(v, value, functions)?,
        parser::ast::Transformation::Try { first, otherwise } => {
            match run_single_transform(first, value.clone(), functions) {
                Ok(v) => v,
                Err(_) => run_single_transform(otherwise, value, functions)?,
            }
        }
    })
}

fn reverse_run_func(
    func: LocalIntern<String>,
    output: Value,
    functions: &Functions,
) -> Result<Value, RuntimeError> {
    let transforms = functions
        .get(&func)
        .ok_or_else(|| RuntimeError::ValueError(format!("Missing `{}` function", func)))?;
    reverse_run_transforms(transforms, output, functions)
}

fn reverse_run_transforms(
    transforms: &[Transformation],
    mut output: Value,
    functions: &Functions,
) -> Result<Value, RuntimeError> {
    for trans in transforms.iter().rev() {
        output = reverse_run_singe_tranform(trans, output, functions)?
    }
    Ok(output)
}

fn reverse_run_singe_tranform(
    trans: &Transformation,
    output: Value,
    functions: &HashMap<LocalIntern<String>, Vec<Transformation>>,
) -> Result<Value, RuntimeError> {
    Ok(match trans {
        Forced {
            destruct,
            construct,
        } => {
            let mut env = Variables::new();
            construct.destruct(&output, &mut env, functions)?;
            let out = destruct.construct(&mut env, functions)?;

            for (name, value) in env.polyidents.iter() {
                if !value.is_empty() {
                    return Err(RuntimeError::ValueError(format!(
                        "Polyident {} was used more times in the construct pattern than in the destruct pattern",
                        name
                    )));
                }
            }
            out
        }
        parser::ast::Transformation::Compound(v) => reverse_run_transforms(v, output, functions)?,
        parser::ast::Transformation::Try { first, otherwise } => {
            match reverse_run_singe_tranform(first, output.clone(), functions) {
                Ok(v) => v,
                Err(_) => reverse_run_singe_tranform(otherwise, output, functions)?,
            }
        }
    })
}

impl Structure for Expr {
    fn construct(
        &self,
        variables: &mut Variables,
        functions: &Functions,
    ) -> Result<Value, RuntimeError> {
        match self {
            Expr::Number(n) => Ok(Value::Number(*n)),
            Expr::String(s, _) => Ok(Value::String(s.to_owned())), // btw we can make strings localintern
            Expr::Array(arr) => Ok(Value::Array(
                arr.iter()
                    .map(|e| -> Result<_, _> { e.construct(variables, functions) })
                    .collect::<Result<_, _>>()?,
            )),
            Expr::Tuple(t) => Ok(Value::Tuple(
                t.iter()
                    .map(|e| -> Result<_, _> { e.construct(variables, functions) })
                    .collect::<Result<_, _>>()?,
            )),
            Expr::Ident(i) => variables
                .get(*i)
                .cloned()
                .ok_or_else(|| RuntimeError::ValueError(format!("Identifier {} not found", i))),

            Expr::PolyIdent(i) => variables.take_polyident(*i)?.ok_or_else(|| {
                RuntimeError::ValueError(format!("Poly-identifier {} not found", i))
            }),
            Expr::Operator(op, a, b) => {
                use parser::ast::Operator::*;
                match op {
                    Add => Ok(a
                        .construct(variables, functions)?
                        .add(&b.construct(variables, functions)?)?),
                    Sub => Ok(a
                        .construct(variables, functions)?
                        .sub(&b.construct(variables, functions)?)?),
                    Mul => Ok(mul(
                        a,
                        &b.construct(variables, functions)?,
                        variables,
                        functions,
                    )?),
                    Div => Ok(a
                        .construct(variables, functions)?
                        .div(&b.construct(variables, functions)?)?),
                    And => Ok(a
                        .construct(variables, functions)?
                        .and(&b.construct(variables, functions)?)?),
                    Or => Ok(a
                        .construct(variables, functions)?
                        .or(&b.construct(variables, functions)?)?),

                    Eq => Ok(Value::Bool(
                        a.construct(variables, functions)? == b.construct(variables, functions)?,
                    )),
                    Neq => Ok(Value::Bool(
                        a.construct(variables, functions)? != b.construct(variables, functions)?,
                    )),
                    Lt => Ok(a
                        .construct(variables, functions)?
                        .lt_op(&b.construct(variables, functions)?)?),
                    Gt => Ok(a
                        .construct(variables, functions)?
                        .gt_op(&b.construct(variables, functions)?)?),
                    Le => Ok(a
                        .construct(variables, functions)?
                        .le_op(&b.construct(variables, functions)?)?),
                    Ge => Ok(a
                        .construct(variables, functions)?
                        .ge_op(&b.construct(variables, functions)?)?),
                }
            }
            Expr::Cast(exp, to, from) => exp.construct(variables, functions)?.cast(to, from),
            Expr::Bool(b) => Ok(Value::Bool(*b)),
            Expr::UnaryOp(op, val) => {
                let val = val.construct(variables, functions)?;
                match (op, val) {
                    (UnaryOperator::Neg, Value::Number(n)) => Ok(Value::Number(-n)),
                    (UnaryOperator::Not, Value::Bool(b)) => Ok(Value::Bool(!b)),
                    (a, val) => Err(RuntimeError::ValueError(format!(
                        "Cannot apply unary operator {:?} to {}",
                        a, val
                    ))),
                }
            }
            Expr::Any => Err(RuntimeError::ValueError("Cannot construct `_`".to_string())),
            Expr::Call(f, a) => run_func(*f, a.construct(variables, functions)?, functions),
        }
    }

    fn destruct(
        &self,
        value: &Value,
        variables: &mut Variables,
        functions: &Functions,
    ) -> Result<Option<Value>, RuntimeError> {
        match &self {
            Expr::Number(n) => {
                if value == &Value::Number(*n) {
                    Ok(Some(Value::Number(*n)))
                } else {
                    Err(RuntimeError::PatternMismatch(format!(
                        "Expected number {:?}, got {}",
                        n, value
                    )))
                }
            }
            Expr::Bool(b) => {
                if value == &Value::Bool(*b) {
                    Ok(Some(Value::Bool(*b)))
                } else {
                    Err(RuntimeError::PatternMismatch(format!(
                        "Expected bool {:?}, got {}",
                        b, value
                    )))
                }
            }
            Expr::String(s, _) => {
                if let Value::String(s2) = value {
                    if s == s2 {
                        Ok(Some(Value::String(s.to_owned())))
                    } else {
                        Err(RuntimeError::PatternMismatch(format!(
                            "Expected string {:?}, got {:?}",
                            s, s2
                        )))
                    }
                } else {
                    Err(RuntimeError::PatternMismatch(format!(
                        "Expected string {:?}, got {}",
                        s, value
                    )))
                }
            }
            Expr::Array(arr) => {
                // i fugured out the destruct thing!!
                match value {
                    Value::Array(arr2) => {
                        if arr.len() != arr2.len() {
                            return Err(RuntimeError::PatternMismatch(format!(
                                "Expected array of length {}",
                                arr.len()
                            )));
                        }
                        let mut arr_val = Some(Vec::new());

                        for (e, v) in arr.iter().zip(arr2.iter()) {
                            if let (Some(val), Some(arr_val)) =
                                (e.destruct(v, variables, functions)?, &mut arr_val)
                            {
                                arr_val.push(val);
                            } else {
                                arr_val = None;
                            }
                        }

                        Ok(arr_val.map(Value::Array))
                    }
                    a => Err(RuntimeError::PatternMismatch(format!(
                        "Expected array, got {}",
                        a
                    ))),
                }
            }
            Expr::Tuple(t) => match value {
                Value::Tuple(t2) => {
                    if t.len() != t2.len() {
                        return Err(RuntimeError::PatternMismatch(format!(
                            "Expected tuple of length {}",
                            t.len()
                        )));
                    }
                    let mut arr_val = Some(Vec::new());

                    for (e, v) in t.iter().zip(t2.iter()) {
                        if let (Some(val), Some(arr_val)) =
                            (e.destruct(v, variables, functions)?, &mut arr_val)
                        {
                            arr_val.push(val);
                        } else {
                            arr_val = None;
                        }
                    }

                    Ok(arr_val.map(Value::Tuple))
                }
                a => Err(RuntimeError::PatternMismatch(format!(
                    "Expected tuple, got {}",
                    a
                ))),
            },
            Expr::Ident(i) => {
                variables.insert(*i, value.clone())?;
                Ok(Some(value.clone()))
            }
            Expr::PolyIdent(i) => {
                variables.insert_polyident(*i, value.clone())?;
                Ok(Some(value.clone()))
            }

            Expr::Operator(op, left, right) => {
                use parser::ast::Operator::*;
                use DestructResult::*;
                match (
                    left.destruct_to_value(functions, variables)?,
                    right.destruct_to_value(functions, variables)?,
                ) {
                    (
                        Known(a) | Partial(PartialValue::Value(a)),
                        Known(b) | Partial(PartialValue::Value(b)),
                    ) => {
                        // incase some patterns both destruct and give a value (like @ in rust)
                        left.destruct(&a, variables, functions)?;
                        right.destruct(&b, variables, functions)?;

                        let res = match op {
                            Add => a.add(&b)?,
                            Sub => a.sub(&b)?,
                            Mul => mul(left, &b, variables, functions)?,
                            Div => a.div(&b)?,
                            And => a.and(&b)?,
                            Or => a.or(&b)?,
                            Eq => Value::Bool(a == b),
                            Neq => Value::Bool(a != b),
                            Lt => a.lt_op(&b)?,
                            Gt => a.gt_op(&b)?,
                            Le => a.le_op(&b)?,
                            Ge => a.ge_op(&b)?,
                        };
                        if &res == value {
                            Ok(Some(res))
                        } else {
                            Err(RuntimeError::PatternMismatch(format!(
                                "Expected {} from destruct expression, found {}",
                                value, res
                            )))
                        }
                    }

                    (Known(left) | Partial(PartialValue::Value(left)), Unknown) => {
                        match op {
                            Add => destruct_algebra::add_left_destruct(
                                &left, &*right, value, variables, functions,
                            )?,
                            Sub => destruct_algebra::sub_left_destruct(
                                &left, &*right, value, variables, functions,
                            )?,
                            Mul => destruct_algebra::mul_left_destruct(
                                &left, &*right, value, variables, functions,
                            )?,
                            Div => destruct_algebra::div_left_destruct(
                                &left, &*right, value, variables, functions,
                            )?,
                            And => destruct_algebra::and_destruct(
                                &left, &*right, value, variables, functions,
                            )?,
                            Or => destruct_algebra::or_destruct(
                                &left, &*right, value, variables, functions,
                            )?,
                            Eq => destruct_algebra::eq_destruct(
                                &left, &*right, value, variables, functions,
                            )?,
                            a => {
                                return Err(RuntimeError::PatternMismatch(format!(
                                    "This operator can not be destructed: {:?}",
                                    a
                                )))
                            }
                        };
                        Ok(None)
                    }
                    (Unknown, Known(right) | Partial(PartialValue::Value(right))) => {
                        match op {
                            Add => destruct_algebra::add_right_destruct(
                                &right, &*left, value, variables, functions,
                            )?,
                            Sub => destruct_algebra::sub_right_destruct(
                                &right, &*left, value, variables, functions,
                            )?,
                            Mul => destruct_algebra::mul_right_destruct(
                                &right, &*left, value, variables, functions,
                            )?,
                            Div => destruct_algebra::div_right_destruct(
                                &right, &*left, value, variables, functions,
                            )?,
                            And => destruct_algebra::and_destruct(
                                &right, &*left, value, variables, functions,
                            )?,
                            Or => destruct_algebra::or_destruct(
                                &right, &*left, value, variables, functions,
                            )?,
                            Eq => destruct_algebra::eq_destruct(
                                &right, &*left, value, variables, functions,
                            )?,
                            a => {
                                return Err(RuntimeError::PatternMismatch(format!(
                                    "This operator can not be destructed: {:?}",
                                    a
                                )))
                            }
                        };
                        Ok(None)
                    }
                    (Partial(partial_left), _) => match (partial_left, op) {
                        (
                            PartialValue::Array {
                                len: Some(len_a), ..
                            },
                            Add,
                        ) => match value {
                            Value::Array(arr) => {
                                let target_val1 = Value::Array(arr[..len_a].to_vec());
                                let target_val2 = Value::Array(arr[len_a..].to_vec());
                                left.destruct(&target_val1, variables, functions)?;
                                right.destruct(&target_val2, variables, functions)?;
                                Ok(None)
                            }
                            a => {
                                return Err(RuntimeError::PatternMismatch(format!(
                                    "Cannot add two arrays to get {}",
                                    a
                                )))
                            }
                        },
                        (
                            PartialValue::Array {
                                len: Some(len_a), ..
                            },
                            Mul,
                        ) => match value {
                            Value::Array(arr) => {
                                if arr.len() % len_a != 0 {
                                    return Err(RuntimeError::PatternMismatch(format!(
                                            "Cannot multiply array of length {} by anything to get an array of length {}",
                                            len_a, arr.len(),
                                        )));
                                }
                                let num = Value::Number((arr.len() / len_a) as f64);
                                right.destruct(&num, variables, functions)?;
                                destruct_algebra::mul_right_destruct(
                                    &num, &*left, value, variables, functions,
                                )?;
                                Ok(None)
                            }
                            a => {
                                return Err(RuntimeError::PatternMismatch(format!(
                                    "Cannot multiply array to get {}",
                                    a
                                )))
                            }
                        },
                        // (PartialValue::String(a), Add) => match value {
                        //     Value::String(v) => {
                        //         unimplemented!()
                        //     }
                        //     a => {
                        //         return Err(RuntimeError::PatternMismatch(format!(
                        //             "Cannot add two strings to get {}",
                        //             a
                        //         )))
                        //     }
                        // },

                        // (PartialValue::String(a), Mul) => match value {
                        //     Value::String(v) => {
                        //         let mut counter = 0;
                        //         let mut str = v.clone();
                        //         let mut iter = a.into_iter();
                        //         for part in iter {
                        //             match part {
                        //                 PartialStringPart::String(s) => {
                        //                     if !str.starts_with(s) {
                        //                         return Err(RuntimeError::PatternMismatch("String does not contain the expected substring".to_string()));
                        //                     }
                        //                     str = str[s.len()..].to_string();
                        //                 }
                        //                 PartialStringPart::Expr(mut e) => {
                        //                     let mut next = iter.next();
                        //                     if next.is_none() {
                        //                         e.destruct(
                        //                             &Value::String(str),
                        //                             variables,
                        //                             functions,
                        //                         );
                        //                     } else {
                        //                         while let Some(PartialStringPart::Expr(e2)) = next {
                        //                             e = Expr::Operator(
                        //                                 parser::ast::Operator::Add,
                        //                                 Box::new(e),
                        //                                 Box::new(e2),
                        //                             );
                        //                             next = iter.next();
                        //                         }
                        //                         if let Some(PartialStringPart::String(s)) = next {
                        //                             e.destruct(
                        //                                 &Value::String(str),
                        //                                 variables,
                        //                                 functions,
                        //                             );
                        //                             str = str[s.len()..].to_string();
                        //                         } else {
                        //                             unreachable!();
                        //                         }
                        //                     }
                        //                 }
                        //             }
                        //         }
                        //         todo!()
                        //     }
                        //     a => {
                        //         return Err(RuntimeError::PatternMismatch(format!(
                        //             "Cannot multiply string to get {}",
                        //             a
                        //         )))
                        //     }
                        // },
                        _ => Err(RuntimeError::ValueError(
                            "Cannot destruct expression with two unknowns".to_string(),
                        )),
                    },

                    (_, Partial(partial_right)) => match (partial_right, op) {
                        (
                            PartialValue::Array {
                                len: Some(len_b), ..
                            },
                            Add,
                        ) => match value {
                            Value::Array(arr) => {
                                let len = arr.len() - len_b;
                                let target_val1 = Value::Array(arr[..len].to_vec());
                                let target_val2 = Value::Array(arr[len..].to_vec());
                                left.destruct(&target_val1, variables, functions)?;
                                right.destruct(&target_val2, variables, functions)?;
                                Ok(None)
                            }
                            a => {
                                return Err(RuntimeError::PatternMismatch(format!(
                                    "Cannot add two arrays to get {}",
                                    a
                                )))
                            }
                        },

                        _ => Err(RuntimeError::ValueError(
                            "Cannot destruct expression with two unknowns".to_string(),
                        )),
                    },

                    _ => Err(RuntimeError::ValueError(
                        "Cannot destruct expression with two unknowns".to_string(),
                    )),
                }
            }
            Expr::Cast(exp, to, from) => exp.destruct(&value.cast(from, to)?, variables, functions),
            Expr::UnaryOp(op, val) => {
                let target_value = match (op, value) {
                    // -x = n
                    (UnaryOperator::Neg, Value::Number(n)) => Value::Number(-n),
                    // !x = b
                    (UnaryOperator::Not, Value::Bool(b)) => Value::Bool(!b),
                    (op, v) => {
                        return Err(RuntimeError::ValueError(format!(
                            "Cannot apply unary operator {:?} to {}",
                            op, v
                        )))
                    }
                };
                val.destruct(&target_value, variables, functions)
            }
            Expr::Any => Ok(None),
            Expr::Call(f, a) => match a.destruct_to_value(functions, variables)? {
                DestructResult::Known(v) => Ok(Some(run_func(*f, v, functions)?)),
                _ => {
                    let target_val = reverse_run_func(*f, value.clone(), functions)?;
                    a.destruct(&target_val, variables, functions)
                }
            },
        }
    }

    fn destruct_to_value(
        &self,
        functions: &Functions,
        variables: &Variables,
    ) -> Result<DestructResult, RuntimeError> {
        use DestructResult::*;
        match &self {
            Expr::Number(n) => Ok(Known(Value::Number(*n))),
            Expr::Bool(b) => Ok(Known(Value::Bool(*b))),
            Expr::String(s, _) => Ok(Known(Value::String(s.clone()))),
            Expr::Array(arr) => {
                let mut arr_val = Vec::new();
                let mut known = true;
                for e in arr {
                    let val = e.destruct_to_value(functions, variables)?;
                    if let Unknown = val {
                        known = false;
                    }
                    arr_val.push(val);
                }
                if known {
                    Ok(Known(Value::Array(
                        arr_val.into_iter().map(|v| v.unwrap()).collect(),
                    )))
                } else {
                    Ok(Partial(PartialValue::Array {
                        len: Some(arr_val.len()),
                        known_elems: arr_val
                            .into_iter()
                            .enumerate()
                            .filter_map(|(a, b)| match b {
                                Known(v) => Some((a, PartialValue::Value(v))),
                                Partial(v) => Some((a, v)),
                                _ => None,
                            })
                            .collect(),
                    }))
                }
            }
            Expr::Tuple(t) => {
                let mut arr_val = Vec::new();
                let mut known = true;
                for e in t {
                    let val = e.destruct_to_value(functions, variables)?;
                    if let Unknown = val {
                        known = false;
                        break;
                    }
                    arr_val.push(val);
                }
                if known {
                    Ok(Known(Value::Array(
                        arr_val.into_iter().map(|v| v.unwrap()).collect(),
                    )))
                } else {
                    Ok(Unknown)
                }
            }
            Expr::Ident(i) => {
                if let Some(v) = variables.get(*i) {
                    Ok(Known(v.clone()))
                } else {
                    Ok(Unknown)
                }
            }
            Expr::PolyIdent(i) => {
                if let Some(v) = variables.get(*i) {
                    Ok(Known(v.clone()))
                } else {
                    Ok(Unknown)
                }
            }

            Expr::Operator(op, left, right) => {
                use parser::ast::Operator::*;
                match (
                    left.destruct_to_value(functions, variables)?,
                    right.destruct_to_value(functions, variables)?,
                ) {
                    (
                        Known(a) | Partial(PartialValue::Value(a)),
                        Known(b) | Partial(PartialValue::Value(b)),
                    ) => {
                        // incase some patterns both destruct and give a value (like @ in rust)
                        let res = match op {
                            Add => a.add(&b)?,
                            Sub => a.sub(&b)?,
                            Mul => match (a, b) {
                                (Value::Number(a), Value::Number(b)) => Value::Number(a * b),
                                (Value::String(a), Value::Number(b)) => {
                                    Value::String(a.repeat(b as usize))
                                } // ensure b is int?
                                (Value::Array(a), Value::Number(b)) => Value::Array(
                                    a.iter()
                                        .cloned()
                                        .cycle()
                                        .take(b as usize * a.len())
                                        .collect(),
                                ),
                                (a, b) => {
                                    return Err(RuntimeError::ValueError(format!(
                                        "Cannot multiply {} and {}",
                                        a, b
                                    )))
                                }
                            },
                            Div => a.div(&b)?,
                            And => a.and(&b)?,
                            Or => a.or(&b)?,
                            Eq => Value::Bool(a == b),
                            Neq => Value::Bool(a != b),
                            Lt => a.lt_op(&b)?,
                            Gt => a.gt_op(&b)?,
                            Le => a.le_op(&b)?,
                            Ge => a.ge_op(&b)?,
                        };

                        Ok(Known(res))
                    }

                    // (Partial(PartialValue::String(a)), Partial(PartialValue::String(b))) => {
                    //     if let Add = op {
                    //         let mut out = a;
                    //         out.extend(b);
                    //         Ok(Partial(PartialValue::String(out)))
                    //     } else {
                    //         Err(RuntimeError::ValueError(format!(
                    //             "Cannot apply operator {:?} to two strings",
                    //             op
                    //         )))
                    //     }
                    // }

                    // (
                    //     Partial(PartialValue::String(a)),
                    //     Known(b) | Partial(PartialValue::Value(b)),
                    // ) => match (op, b) {
                    //     (Mul, Value::Number(n)) => {
                    //         if n.fract() != 0.0 {
                    //             return Err(RuntimeError::ValueError(format!(
                    //                 "Cannot multiply string by non-integer {}",
                    //                 n
                    //             )));
                    //         }
                    //         let mut out = Vec::new();
                    //         for _ in 0..(n as usize) {
                    //             out.extend(a.clone());
                    //         }
                    //         Ok(Partial(PartialValue::String(out)))
                    //     }
                    //     (Add, Value::String(s)) => {
                    //         let mut out = a;
                    //         out.push(PartialStringPart::String(s));
                    //         Ok(Partial(PartialValue::String(out)))
                    //     }
                    //     (op, b) => Err(RuntimeError::ValueError(format!(
                    //         "Cannot apply operator {:?} to string and {}",
                    //         op, b
                    //     ))),
                    // },

                    // (
                    //     Known(a) | Partial(PartialValue::Value(a)),
                    //     Partial(PartialValue::String(b)),
                    // ) => match (op, a) {
                    //     (Add, Value::String(s)) => {
                    //         let mut out = vec![PartialStringPart::String(s)];
                    //         out.extend(b);
                    //         Ok(Partial(PartialValue::String(out)))
                    //     }
                    //     (op, a) => Err(RuntimeError::ValueError(format!(
                    //         "Cannot apply operator {:?} to string and {}",
                    //         op, a
                    //     ))),
                    // },
                    _ => Ok(Unknown),
                }
            }
            Expr::Cast(exp, to, from) => match exp.destruct_to_value(functions, variables)? {
                Known(v) => Ok(Known(v.cast(from, to)?)),
                _ => Ok(Unknown),
            },
            Expr::UnaryOp(op, val) => {
                match (op, val.destruct_to_value(functions, variables)?) {
                    // -x = n
                    (UnaryOperator::Neg, Known(Value::Number(n))) => Ok(Known(Value::Number(-n))),
                    // !x = b
                    (UnaryOperator::Not, Known(Value::Bool(b))) => Ok(Known(Value::Bool(!b))),
                    (op, Known(v)) => Err(RuntimeError::ValueError(format!(
                        "Cannot apply unary operator {:?} to {}",
                        op, v
                    ))),
                    _ => Ok(Unknown),
                }
            }
            Expr::Any => Ok(Unknown),
            Expr::Call(f, a) => match a.destruct_to_value(functions, variables)? {
                Known(v) => Ok(Known(run_func(*f, v, functions)?)), // run function normally because the value is known
                _ => Ok(Unknown),
            }, // ??
        }
    }
}
