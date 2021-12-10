use std::{error::Error, fmt::Display};

#[derive(Debug, Clone)]
pub enum RuntimeError {
    PatternMismatch(String),
    ValueError(String),
    TypeMismatch(String, String),
}

impl Display for RuntimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RuntimeError::PatternMismatch(t) => write!(f, "PATTERN MISSMATCH: {}", t),
            RuntimeError::ValueError(t) => write!(f, "ERROR: {}", t),
            RuntimeError::TypeMismatch(from, to) => {
                write!(f, "TYPE MISMATCH: cannot convert from {} to {}", from, to)
            }
        }
    }
}

impl Error for RuntimeError {}
