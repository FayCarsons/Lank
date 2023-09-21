use std::{
    collections::{HashMap, VecDeque},
    fmt,
    rc::Rc,
};

use crate::core::eval_value;

pub type Seq = Rc<Vec<Value>>;
pub type Vector = Rc<VecDeque<Value>>;

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Form {
    Quoted(Seq),
    Unquoted(Seq),
}

impl std::fmt::Display for Form {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Quoted(tokens) | Self::Unquoted(tokens) => tokens.iter().map(|token| {
                write!(f, "{token}")
            }).collect::<Result<(), std::fmt::Error>>()
        }
    }
}

impl Value {
    pub const NIL: Value = Value::Void;

    pub fn type_of(&self) -> &str {
        match self {
            Value::Void => "Void",
            Value::Form(form) => match form {
                Form::Quoted(_) => "Quoted form",
                Form::Unquoted(_) => "Unquoted form",
            },
            Value::Vec(_) => "Vector",
            Value::Number(_) => "Number",
            Value::String(_) => "String",
            Value::Symbol(_) => "Symbol",
            Value::Char(_) => "Char",
            Value::Bool(_) => "Bool",
            Value::Fun(_, _) => "Function",
        }
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Value {
    Void,
    Form(Form),
    Number(f64),
    String(Rc<str>),
    Char(char),
    Symbol(Rc<str>),
    Bool(bool),
    Vec(Vector),
    Fun(Rc<Vec<String>>, Rc<Vec<Value>>),
}

impl From<Rc<[Value]>> for Value {
    fn from(ptr: Rc<[Value]>) -> Self {
        Value::Form(Form::Unquoted(Rc::from(ptr.to_vec())))
    }
}

impl From<Vec<Value>> for Value {
    fn from(coll: Vec<Self>) -> Self {
        Self::Form(Form::Unquoted(Rc::from(coll)))
    }
}

impl From<String> for Value {
    fn from(value: String) -> Self {
        Value::String(Rc::from(value))
    }
}

impl From<&str> for Value {
    fn from(value: &str) -> Self {
        Value::String(Rc::from(value))
    }
}

impl From<Vec<String>> for Value {
    fn from(value: Vec<String>) -> Self {
        Value::from(
            value
                .iter()
                .map(|s| Value::from(s.clone()))
                .collect::<Vec<Value>>(),
        )
    }
}

impl From<VecDeque<Value>> for Value {
    fn from(coll: VecDeque<Self>) -> Self {
        Value::Vec(Rc::new(coll))
    }
}

impl From<Rc<VecDeque<Value>>> for Value {
    fn from(coll: Rc<VecDeque<Self>>) -> Self {
        Value::Vec(coll)
    }
}

impl From<f64> for Value {
    fn from(value: f64) -> Self {
        Value::Number(value)
    }
}

impl From<i64> for Value {
    fn from(value: i64) -> Self {
        Value::Number(value as f64)
    }
}

impl From<bool> for Value {
    fn from(value: bool) -> Self {
        Value::Bool(value)
    }
}

impl AsRef<Value> for Value {
    fn as_ref(&self) -> &Self {
        self
    }
}

impl From<Value> for String {
    fn from(value: Value) -> Self {
        match value {
            Value::Char(c) => c.to_string(),
            Value::String(s) => s.to_string(),
            Value::Form(form) => format!("{form}"),
            Value::Bool(b) => (if b {"true"} else {"false"}).to_string(),
            Value::Number(n) => n.to_string(),
            Value::Fun(_, _) => format!("{value}"),
            Value::Symbol(s) => s.to_string(),
            Value::Void => "".to_string(),
            Value::Vec(v) => format!("{v:?}")
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Void => write!(f, ""),
            Self::Form(form) => match form {
                Form::Quoted(fr) | Form::Unquoted(fr) => {
                    write!(f, "(")?;
                    for token in fr.iter() {
                        write!(f, "{token} ")?;
                    }
                    write!(f, ") ")
                }
            },
            Self::Number(n) => write!(f, "{n}"),
            Self::Bool(b) => write!(f, "{b}"),
            Self::String(s) => write!(f, "\"{s}\""),
            Self::Symbol(s) => write!(f, "{s}"),
            Self::Char(c) => write!(f, "\'{c}\'"),
            Self::Fun(params, body) => {
                write!(f, "(fn (")?;
                for param in params.iter() {
                    write!(f, "{param}")?;
                }
                write!(f, ") (")?;
                for expr in body.iter() {
                    write!(f, "{expr} ")?;
                }
                write!(f, "))")
            }
            Self::Vec(v) => {
                write!(f, "[")?;
                for val in v.iter() {
                    write!(f, "{val} ")?;
                }
                write!(f, "]")
            }
        }
    }
}
