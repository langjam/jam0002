use crate::{
    error::RuntimeError,
    traits::{Functions, Structure, Value, Variables},
};
use parser::ast::Expr;

pub fn add_left_destruct(
    left: &Value,
    right: &Expr,
    target_val: &Value,
    variables: &mut Variables,
    functions: &Functions,
) -> Result<(), RuntimeError> {
    // (a * 4 - 6) + 10 -> a // input 15 output 5
    let target_val = match (left, target_val) {
        // n1 + x = n2
        (Value::Number(n1), Value::Number(n2)) => Value::Number(n2 - n1),
        // s1 + x = s2
        (Value::String(s1), Value::String(s2)) => {
            if !s2.starts_with(s1) {
                return Err(RuntimeError::PatternMismatch(format!(
                    "Expected {} to start with {}",
                    s2, s1
                )));
            }
            Value::String(s2[s1.len()..].to_string())
        }
        // a1 + x = a2
        (Value::Array(a1), Value::Array(a2)) => {
            if !a2.starts_with(a1) {
                return Err(RuntimeError::PatternMismatch(format!(
                    "Expected {} to start with {}",
                    Value::Array(a2.clone()),
                    Value::Array(a1.clone())
                )));
            }
            Value::Array(a2[a1.len()..].to_vec())
        }
        _ => {
            return Err(RuntimeError::ValueError(format!(
                "Cannot add {} with something to get {}",
                left, target_val
            )))
        }
    };
    right.destruct(&target_val, variables, functions)?;
    Ok(())
}

pub fn add_right_destruct(
    right: &Value,
    left: &Expr,
    target_val: &Value,
    variables: &mut Variables,
    functions: &Functions,
) -> Result<(), RuntimeError> {
    let target_val = match (right, target_val) {
        // x + n1 = n2
        (Value::Number(n1), Value::Number(n2)) => Value::Number(n2 - n1),
        // x + s1 = s2
        (Value::String(s1), Value::String(s2)) => {
            if !s2.ends_with(s1) {
                return Err(RuntimeError::PatternMismatch(format!(
                    "Expected {} to end with {}",
                    s2, s1
                )));
            }
            Value::String(s2[..(s2.len() - s1.len())].to_string())
        }
        // x + a1 = a2
        (Value::Array(a1), Value::Array(a2)) => {
            if !a2.ends_with(a1) {
                return Err(RuntimeError::PatternMismatch(format!(
                    "Expected {} to end with {}",
                    Value::Array(a2.clone()),
                    Value::Array(a1.clone())
                )));
            }
            Value::Array(a2[..(a2.len() - a1.len())].to_vec())
        }
        _ => {
            return Err(RuntimeError::ValueError(format!(
                "Cannot add something with {} to get {}",
                right, target_val
            )))
        }
    };

    left.destruct(&target_val, variables, functions)?;
    Ok(())
}

pub fn sub_left_destruct(
    left: &Value,
    right: &Expr,
    target_val: &Value,
    variables: &mut Variables,
    functions: &Functions,
) -> Result<(), RuntimeError> {
    let target_val = match (left, target_val) {
        // n1 - x = n2
        (Value::Number(n1), Value::Number(n2)) => Value::Number(n1 - n2),

        _ => {
            return Err(RuntimeError::ValueError(format!(
                "Cannot subtract {} from something to get {}",
                left, target_val
            )))
        }
    };
    right.destruct(&target_val, variables, functions)?;
    Ok(())
}

pub fn sub_right_destruct(
    right: &Value,
    left: &Expr,
    target_val: &Value,
    variables: &mut Variables,
    functions: &Functions,
) -> Result<(), RuntimeError> {
    let target_val = match (right, target_val) {
        // x - n1 = n2
        (Value::Number(n1), Value::Number(n2)) => Value::Number(n1 + n2),

        _ => {
            return Err(RuntimeError::ValueError(format!(
                "Cannot subtract something from {} to get {}",
                right, target_val
            )))
        }
    };

    left.destruct(&target_val, variables, functions)?;
    Ok(())
}

pub fn mul_left_destruct(
    left: &Value,
    right: &Expr,
    target_val: &Value,
    variables: &mut Variables,
    functions: &Functions,
) -> Result<(), RuntimeError> {
    let target_val = match (left, target_val) {
        // n1 * x = n2
        (Value::Number(n1), Value::Number(n2)) => Value::Number(n2 / n1),
        (Value::Array(a1), Value::Array(a2)) => {
            if a2.len() % a1.len() != 0 {
                return Err(RuntimeError::PatternMismatch(format!(
                    "Length of target array {} is not divisible by length of destruct array {}",
                    Value::Array(a2.clone()),
                    Value::Array(a1.clone())
                )));
            }
            let repeats = a2.len() / a1.len();
            for (i, el) in a2.iter().enumerate() {
                if a1[i % a1.len()] != *el {
                    return Err(RuntimeError::PatternMismatch(format!("Element {} at index {} of target array {} does not match element {} at index {} of destruct array {}", el, i, Value::Array(a2.clone()), a1[i % a1.len()], i % a1.len(), Value::Array(a1.clone()))));
                }
            }

            Value::Number(repeats as f64)
        }
        (Value::String(s1), Value::String(s2)) => {
            if s2.len() % s1.len() != 0 {
                return Err(RuntimeError::PatternMismatch(format!(
                    "Length of target string {} is not divisible by length of destruct string {}",
                    s2, s1
                )));
            }
            let repeats = s2.len() / s1.len();
            for (i, c) in s2.bytes().enumerate() {
                // since .len() is the bytes
                if s1.as_bytes()[i % s1.len()] != c {
                    return Err(RuntimeError::PatternMismatch(format!("Character {} at index {} of target string {} does not match character {} at index {} of destruct string {}", c as char, i, s2, s1.as_bytes()[i % s1.len()] as char, i % s1.len(), s1)));
                }
            }
            Value::Number(repeats as f64)
        }

        _ => {
            return Err(RuntimeError::ValueError(format!(
                "Cannot multiply {} with something to get {}",
                left, target_val
            )))
        }
    };
    right.destruct(&target_val, variables, functions)?;
    Ok(())
}

pub fn mul_right_destruct(
    right: &Value,
    left: &Expr,
    target_val: &Value,
    variables: &mut Variables,
    functions: &Functions,
) -> Result<(), RuntimeError> {
    match (right, target_val) {
        // x * n1 = n2
        (Value::Number(n1), Value::Number(n2)) => {
            let target_val = Value::Number(n2 / n1);
            left.destruct(&target_val, variables, functions)?;
            Ok(())
        }
        (Value::Number(n1), Value::Array(a2)) => {
            if n1.fract() != 0.0 {
                return Err(RuntimeError::ValueError(format!(
                    "Cannot multiply array with non-integer number {}",
                    n1
                )));
            }
            let n = *n1 as usize;
            if n > a2.len() {
                return Err(RuntimeError::PatternMismatch(format!(
                    "Cannot multiply array with number {} greater than length of array {}",
                    n,
                    a2.len()
                )));
            }
            if a2.len() % n != 0 {
                return Err(RuntimeError::PatternMismatch(format!(
                    "Length of array {} is not divisible by number {}",
                    Value::Array(a2.clone()),
                    n
                )));
            }
            let len = a2.len() / n;

            for i in 0..n {
                let target = &Value::Array(a2[i * len..(i + 1) * len].to_vec());
                left.destruct(target, variables, functions)?;
            }

            Ok(())
        }

        (Value::Number(n1), Value::String(s2)) => {
            if n1.fract() != 0.0 {
                return Err(RuntimeError::ValueError(format!(
                    "Cannot multiply string with non-integer number {}",
                    n1
                )));
            }
            let n = *n1 as usize;
            if n > s2.len() {
                return Err(RuntimeError::PatternMismatch(format!(
                    "Cannot multiply string with number {} greater than length of string {}",
                    n,
                    s2.len()
                )));
            }
            if s2.len() % n != 0 {
                return Err(RuntimeError::PatternMismatch(format!(
                    "Length of string {} is not divisible by number {}",
                    s2, n
                )));
            }
            let len = s2.len() / n;

            for i in 0..n {
                let target = &Value::String(s2[i * len..(i + 1) * len].to_string());
                left.destruct(target, variables, functions)?;
            }

            Ok(())
        }

        _ => {
            return Err(RuntimeError::ValueError(format!(
                "Cannot multiply something with {} to get {}",
                right, target_val
            )))
        }
    }
}

pub fn div_left_destruct(
    left: &Value,
    right: &Expr,
    target_val: &Value,
    variables: &mut Variables,
    functions: &Functions,
) -> Result<(), RuntimeError> {
    let target_val = match (left, target_val) {
        // n1 / x = n2
        (Value::Number(n1), Value::Number(n2)) => Value::Number(n1 / n2),

        (Value::String(s1), Value::Array(arr)) => {
            // arr is a list of substrings in the order they appear in in s1
            // s1 has the same substrings, but delimitered by a specific substring
            // find the delimiter substring

            let mut strings = Vec::new();

            for el in arr {
                match el {
                    Value::String(s) => strings.push(s.clone()),
                    a => {
                        return Err(RuntimeError::ValueError(format!(
                            "Cannot divide string by array containing non-string {}",
                            a
                        )))
                    }
                }
            }

            if !s1.starts_with(&strings[0]) {
                return Err(RuntimeError::PatternMismatch(format!(
                    "First element of array {} is not a prefix of string {}",
                    Value::Array(arr.clone()),
                    Value::String(s1.clone())
                )));
            }

            let rest = &s1[strings[0].len()..];

            // find the second element of the array in the `rest` string
            let mut i = 0;
            while i < rest.len() {
                if rest[i..].starts_with(&strings[1]) {
                    break;
                }
                i += 1;
            }
            let delim = &rest[0..i];

            if &strings.join(delim) != s1 {
                return Err(RuntimeError::PatternMismatch(format!(
                    "Cannot find delimiter that fits between array {} and string {}",
                    Value::Array(arr.clone()),
                    Value::String(s1.clone())
                )));
            }

            Value::String(delim.to_string())
        }

        _ => {
            return Err(RuntimeError::ValueError(format!(
                "Cannot divide {} with something to get {}",
                left, target_val
            )))
        }
    };
    right.destruct(&target_val, variables, functions)?;
    Ok(())
}

pub fn div_right_destruct(
    right: &Value,
    left: &Expr,
    target_val: &Value,
    variables: &mut Variables,
    functions: &Functions,
) -> Result<(), RuntimeError> {
    let target_val = match (right, target_val) {
        // x / n1 = n2
        (Value::Number(n1), Value::Number(n2)) => Value::Number(n1 * n2),

        (Value::String(delim), Value::Array(arr)) => {
            let mut strings = Vec::new();

            for el in arr {
                match el {
                    Value::String(s) => strings.push(s.clone()),
                    a => {
                        return Err(RuntimeError::ValueError(format!(
                            "Cannot divide string by array containing non-string {}",
                            a
                        )))
                    }
                }
            }

            Value::String(strings.join(delim))
        }

        _ => {
            return Err(RuntimeError::ValueError(format!(
                "Cannot divide something with {} to get {}",
                right, target_val
            )))
        }
    };

    left.destruct(&target_val, variables, functions)?;
    Ok(())
}

pub fn and_destruct(
    val: &Value,
    expr: &Expr,
    target_val: &Value,
    variables: &mut Variables,
    functions: &Functions,
) -> Result<(), RuntimeError> {
    match (val, target_val) {
        (Value::Bool(true), Value::Bool(true)) => {
            expr.destruct(&Value::Bool(true), variables, functions)?;
            Ok(())
        }
        (Value::Bool(_), Value::Bool(false)) => {
            expr.destruct(&Value::Bool(false), variables, functions)?;
            Ok(())
        }
        _ => {
            return Err(RuntimeError::ValueError(format!(
                "Cannot && {} with something to get {}",
                val, target_val
            )))
        }
    }
}

pub fn or_destruct(
    val: &Value,
    expr: &Expr,
    target_val: &Value,
    variables: &mut Variables,
    functions: &Functions,
) -> Result<(), RuntimeError> {
    match (val, target_val) {
        (Value::Bool(false), Value::Bool(true)) => {
            expr.destruct(&Value::Bool(true), variables, functions)?;
            Ok(())
        }
        (Value::Bool(false), Value::Bool(false)) => {
            expr.destruct(&Value::Bool(false), variables, functions)?;
            Ok(())
        }
        (Value::Bool(true), Value::Bool(true)) => Err(RuntimeError::ValueError(
            "Cannot destruct variable that can be either true or false".to_string(),
        )),
        _ => {
            return Err(RuntimeError::ValueError(format!(
                "Cannot || {} with something to get {}",
                val, target_val
            )))
        }
    }
}

pub fn eq_destruct(
    val: &Value,
    expr: &Expr,
    target_val: &Value,
    variables: &mut Variables,
    functions: &Functions,
) -> Result<(), RuntimeError> {
    match target_val {
        Value::Bool(true) => {
            expr.destruct(val, variables, functions)?;
            Ok(())
        }
        Value::Bool(false) => Err(RuntimeError::ValueError(format!(
            "Cannot destruct variable that can be any value other than {}",
            val,
        ))),
        _ => {
            return Err(RuntimeError::ValueError(format!(
                "Cannot == {} with something to get {}",
                val, target_val
            )))
        }
    }
}
