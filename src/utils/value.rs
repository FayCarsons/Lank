use std::{
    collections::{VecDeque, HashMap},
    fmt,
    rc::Rc,
};

pub type Seq = Rc<Vec<Value>>;
pub type Vector = Rc<VecDeque<Value>>;
pub type Map = Box<HashMap<Value, Value>>;

pub enum Form {
    Quoted(Seq),
    Unquted(Seq),
    Data(Vector),
}

impl Value {
    pub const NIL: Value = Value::Void;

    pub fn new_unquoted(tokens: Vec<Value>) -> Self {
        Self::Form {
            quoted: false,
            tokens: Rc::new(tokens),
        }
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Value {
    Void,
    Form { quoted: bool, tokens: Seq },
    Number(f64),
    String(Rc<str>),
    Char(char),
    Symbol(Rc<str>),
    Bool(bool),
    Vec(Vector),
    Map(Seq),
    Fun(Rc<Vec<String>>, Rc<Vec<Value>>),
}

impl From<Rc<Vec<Value>>> for Value {
    fn from(ptr: Rc<Vec<Self>>) -> Self {
        Value::Form {
            quoted: false,
            tokens: ptr,
        }
    }
}

impl From<Vec<Value>> for Value {
    fn from(coll: Vec<Self>) -> Self {
        Value::Form{quoted: false, tokens: Rc::new(coll)}
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
        Value::Number(value as f64   )
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
            Self::Form { tokens, .. } => {
                write!(f, "(")?;
                for token in tokens.iter() {
                    write!(f, "{token} ")?;
                }
                write!(f, ") ")
            }
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
            Self::Map(map) => write!(f, "{map:#?}"),
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
