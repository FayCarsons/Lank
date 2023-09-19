use std::{collections::{VecDeque}, fmt};

pub type Seq = Vec<Value>;

impl Value {
    pub fn new_unquoted(tokens: Vec<Value>) -> Self {
        Self::Form{quoted: false, tokens}
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Value {
    Void,
    Form{quoted: bool, tokens: Seq},
    Number(f64),
    String(String),
    Char(char),
    Symbol(String),
    Bool(bool),
    Vec(VecDeque<Value>),
    Map(Seq),
    Fun(Vec<String>, Vec<Value>)
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Void => write!(f,""),
            Self::Form{tokens, ..} => {
                write!(f, "(")?;
                for token in tokens {
                    write!(f,"{token} ")?;
                }
                write!(f, ") ")
            },
            Self::Number(n) => write!(f, "{n}"),
            Self::Bool(b) => write!(f, "{b}"),
            Self::String(s) => write!(f, "{s}"),
            Self::Symbol(s) => write!(f, "{s}"),
            Self::Char(c) => write!(f, "{c}"),
            Self::Fun(params, body) => {
                write!(f,"(fn (")?;
                for param in params {
                    write!(f, "{param}")?;
                }
                write!(f, ") (")?;
                for expr in body {
                    write!(f, "{expr} ")?;
                }
                write!(f, "))")
            }
            Self::Map(map) => write!(f, "{map:#?}"),
            Self::Vec(v) => {
                write!(f, "[")?;
                for val in v {
                    write!(f, "{val} ")?;
                }
                write!(f, "]")
            }
        }
    }
}