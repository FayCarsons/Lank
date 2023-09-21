use std::{
    collections::{HashMap, VecDeque},
    fmt,
    rc::Rc,
};

pub type Seq = Rc<Vec<Value>>;
pub type Vector = Rc<VecDeque<Value>>;

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Form {
    Quoted(Seq),
    Unquoted(Seq),
}

impl Value {
    pub const NIL: Value = Value::Void;
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
        Value::Symbol(Rc::from(value))
    }
}

impl From<Vec<String>> for Value {
    fn from(value: Vec<String>) -> Self {
        Value::from(value.iter().map(|s| Value::from(s.clone())).collect::<Vec<Value>>())
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
            Self::String(s) => write!(f, "{s}"),
            Self::Symbol(s) => write!(f, "{s}"),
            Self::Char(c) => write!(f, "{c}"),
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
