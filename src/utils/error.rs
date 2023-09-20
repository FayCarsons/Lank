use super::value::Value;
use core::fmt::Debug;
use std::hash::Hash;

pub enum LankError {
    DivideByZero,
    EmptyList,
    FunctionFormat,
    NoChildren,
    NotANumber,
    NumArguments(usize, usize),
    ParseError(String),
    ReadlineError(String),
    WrongType(String, String),
    UnknownFunction(String),
}

pub type LankResult<T> = std::result::Result<T, LankError>;
pub type EvalResult = std::result::Result<Value, String>;

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
        LankError::ParseError(error.to_string())
    }
}
