use std::collections::{HashMap, VecDeque};
use std::fmt;

use super::error::PrimitveResult;
use super::{
    env::Env,
    error::{LankError, LankResult},
};

pub type NativeOp = fn(Primitive, Env) -> LankResult<Primitive>;
pub type Children = Vec<Box<Primitive>>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Function {
    Builtin(String, NativeOp),
    Lambda(
        HashMap<String, Box<Primitive>>,
        Box<Primitive>,
        Box<Primitive>,
    ),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Primitive {
    None,
    Some(Box<Primitive>),
    Num(i64),
    Bool(bool),
    String(String),
    Vec(VecDeque<Box<Primitive>>),
    Symbol(String),
    Func(Function),
    Sexpr(Children),
    Qexpr(Children),
    Vexpr(Children),
    Lank(Children)
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Function::Builtin(name, _) => write!(f, "Native Fn {name}"),
            Function::Lambda(_, args, body) => write!(f, "(({args}) {body})"),
        }
    }
}

impl Primitive {
    pub fn new_num(num: i64) -> Box<Self> {
        Box::new(Primitive::Num(num))
    }

    pub fn new_string(string: &str) -> Box<Self> {
        Box::new(Primitive::String(string.to_owned()))
    }

    pub fn conj(&self, other: &Primitive) -> LankResult<Children> {
        match *self {
            Self::Sexpr(children)
            | Self::Qexpr(children)
            | Self::Vexpr(children)
            | Self::Lank(children) => Ok([children, vec![Box::new(other.clone())]].concat()),
            _ => return Err(LankError::NoChildren),
        }
    }

    pub fn pop(&self, i: usize) -> PrimitveResult {
        match *self {
            Self::Sexpr(ref children)
            | Self::Qexpr(ref children)
            | Self::Vexpr(ref children)
            | Self::Lank(ref children) => {
                let ret = children[i];
                let children = children.remove(i);
                Ok(children)
            }
            _ => Err(LankError::NoChildren),
        }
    }

    pub fn concat(&self, other: Box<Primitive>) -> LankResult<Children> {
        match (*self, *other) {
            (Self::Sexpr(a), Self::Sexpr(b)) | (Self::Vexpr(a), Self::Vexpr(b)) => {
                Ok([a, b].concat())
            }
            (a, b) => Err(LankError::WrongType(
                "Expected List or Vec".to_owned(),
                format!("Got {a} and {b}"),
            )),
        }
    }
}

impl fmt::Display for Primitive {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::None => write!(f, "Void"),
            Self::Some(x) => write!(f, "{x:?}"),
            Self::Func(fun) => write!(f, "{fun}"),
            Self::Sexpr(list) => {
                write!(f, "(")?;
                for x in list {
                    write!(f, "{} ", x.to_string())?;
                }
                write!(f, ")")
            }
            Self::Qexpr(list) => {
                write!(f, "{{")?;
                for x in list {
                    write!(f, "{} ", x.to_string())?;
                }
                write!(f, "}}")
            }
            Self::Vexpr(list) => {
                write!(f, "[")?;
                for x in list {
                    write!(f, "{} ", x.to_string())?;
                }
                write!(f, "]")
            }
            Self::Num(num) => write!(f, "{num}"),
            Self::Bool(b) => write!(f, "{b}"),
            Self::Symbol(s) => write!(f, "{s}"),
            Self::Vec(vec) => {
                write!(f, "(")?;
                for x in vec {
                    write!(f, "{} ", x.to_string())?;
                }
                write!(f, ")")
            }
            Self::String(s) => write!(f, "\"{s}\""),
            Self::Lank(children) => {
                children.iter().for_each(|child| {
                    write!(f, "{child}");
            });
            Ok(())
        }
        }
    }
}
