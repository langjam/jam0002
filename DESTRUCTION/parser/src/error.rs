use std::{error::Error, fmt::Display, path::PathBuf};

#[derive(Debug)]
pub enum LangError {
    SyntaxError {
        pos: (usize, usize),
        message: String,
        file: Option<PathBuf>,
    },
}



pub enum LangErrorT {
    SyntaxError,
}

impl Error for LangError {}

impl Display for LangError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LangError::SyntaxError {
                pos,
                message,
                file,
            } => {
                if let Some(file) = file {
                    write!(f, "Syntax Error: {}:{}:{}: {}", file.display(), pos.0, pos.1, message)
                } else {
                    write!(f, "Syntax Error: {}:{}: {}", pos.0, pos.1, message)
                }
            }
        }
    }
}
