use crate::core::Env;

use super::value::Value;
use core::fmt::Debug;
use std::{
    hash::Hash,
    sync::{PoisonError, RwLockReadGuard},
};

#[derive(Debug, Clone)]
pub enum LankError {
    DivideByZero,
    SyntaxError,
    EmptyList,
    FunctionFormat,
    NoChildren,
    NotANumber,
    NumArguments(String, usize),
    ParseError(String),
    ReadlineError(String),
    WrongType(String),
    UnknownFunction(String),
    Other(String),
}

pub type EvalResult = std::result::Result<Value, LankError>;
pub type IterResult = Result<Vec<Value>, LankError>;

impl<T> From<pest::error::Error<T>> for LankError
where
    T: Debug + Ord + Copy + Hash,
{
    fn from(error: pest::error::Error<T>) -> Self {
        LankError::ParseError(format!("{}", error))
    }
}

impl From<std::io::Error> for LankError {
    fn from(error: std::io::Error) -> Self {
        LankError::ReadlineError(error.to_string())
    }
}

impl<'a> From<PoisonError<RwLockReadGuard<'a, Env>>> for LankError {
    fn from(value: PoisonError<RwLockReadGuard<'a, Env>>) -> Self {
        LankError::Other(value.to_string())
    }
}

impl From<&str> for LankError {
    fn from(value: &str) -> Self {
        LankError::Other(value.to_owned())
    }
}

impl From<String> for LankError {
    fn from(value: String) -> Self {
        LankError::ParseError(value)
    }
}

impl From<LankError> for String {
    fn from(value: LankError) -> Self {
        match value {
            LankError::DivideByZero => "Divide by zero!".to_owned(),
            LankError::EmptyList => "Empty list!".to_owned(),
            LankError::FunctionFormat => "Syntax error in fn".to_owned(),
            LankError::NoChildren => "Form has no children / insufficient args !".to_owned(),
            LankError::NotANumber => "Not a Number!".to_owned(),
            LankError::SyntaxError => "Syntax error".to_owned(),
            LankError::NumArguments(name, args) => {
                format!("{name} expected {args} or more arguments!")
            }
            LankError::Other(s)
            | LankError::ParseError(s)
            | LankError::ReadlineError(s)
            | LankError::UnknownFunction(s)
            | LankError::WrongType(s) => s,
        }
    }
}

impl std::fmt::Display for LankError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LankError::DivideByZero => write!(f, "Divide by zero!"),
            LankError::EmptyList => write!(f, "Empty list!"),
            LankError::FunctionFormat => write!(f, "Syntax error in fn"),
            LankError::NoChildren => write!(f, "Form has no children / insufficient args !"),
            LankError::NotANumber => write!(f, "Not a Number!"),
            LankError::SyntaxError => write!(f, "Syntax error!"),
            LankError::NumArguments(name, args) => {
                write!(f, "{name} expected {args} or more arguments!")
            }
            LankError::Other(s) | LankError::ParseError(s) | LankError::ReadlineError(s) => {
                write!(f, "{s}")
            }
            LankError::UnknownFunction(s) => write!(f, "{s} is not a function!"),
            LankError::WrongType(s) => write!(f, "{s}: wrong type!"),
        }
    }
}
