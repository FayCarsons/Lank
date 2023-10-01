use std::{
    collections::{HashMap, VecDeque},
    fmt,
    hash::Hash,
    ops::Deref,
    rc::Rc,
};

pub type Seq = Rc<Vec<Value>>;
pub type Vector = Rc<VecDeque<Value>>;
pub type Map = Box<HashMap<Value, Value>>;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    None,
    Form(Form),
    Number(f64),
    String(Rc<str>),
    Char(char),
    Symbol(Rc<str>),
    Bool(bool),
    Vec(Vector),
    BitSeq(u16),
    Quoted(Box<Value>),
    Map(Map),
    Fun(Rc<Vec<String>>, Form),
}

unsafe impl Send for Value {}
unsafe impl Sync for Value {}

#[derive(Debug, Clone, PartialEq)]
pub enum Form {
    Quoted(Seq),
    Unquoted(Seq),
}

unsafe impl Send for Form {}
unsafe impl Sync for Form {}

impl From<&[Value]> for Form {
    fn from(value: &[Value]) -> Self {
        Form::Unquoted(Rc::new(value.to_vec()))
    }
}

impl Deref for Form {
    type Target = Seq;

    fn deref(&self) -> &Self::Target {
        match self {
            Form::Quoted(form) => form,
            Form::Unquoted(form) => form,
        }
    }
}

impl std::fmt::Display for Form {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "(")?;
        match self {
            Self::Quoted(tokens) | Self::Unquoted(tokens) => {
                tokens.iter().enumerate().try_for_each(|(i, token)| {
                    write!(f, "{token}{}", if i < tokens.len() - 1 { " " } else { "" })
                })
            }
        }?;
        write!(f, ")")
    }
}

impl Value {
    pub const NIL: Value = Value::None;

    pub fn type_of(&self) -> &str {
        match self {
            Self::None => "None",
            Self::Form(form) => match form {
                Form::Quoted(_) => "Quoted form",
                Form::Unquoted(_) => "Unquoted form",
            },
            Self::Vec(_) => "Vector",
            Self::Number(_) => "Number",
            Self::String(_) => "String",
            Self::Symbol(_) => "Symbol",
            Self::Char(_) => "Char",
            Self::Bool(_) => "Bool",
            Self::Fun(_, _) => "Function",
            Self::BitSeq(_) => "Bit-seq",
            Self::Quoted(val) => val.as_ref().type_of(),
            Self::Map(_) => "Map",
        }
    }
}

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Self::Bool(a), Self::Bool(b)) => a.partial_cmp(b),
            (Self::Number(a), Self::Number(b)) => a.to_bits().partial_cmp(&b.to_bits()),
            (Self::Form(a), Self::Form(b)) => a.partial_cmp(b),
            (Self::BitSeq(a), Self::BitSeq(b)) => a.partial_cmp(b),
            (Self::Vec(a), Self::Vec(b)) => a.partial_cmp(b),
            (Self::String(a), Self::String(b)) => a.partial_cmp(b),
            (Self::Char(a), Self::Char(b)) => a.partial_cmp(b),
            (_, Self::None) => Some(std::cmp::Ordering::Greater),
            (Self::None, _) => Some(std::cmp::Ordering::Less),
            _ => Some(std::cmp::Ordering::Equal),
        }
    }
}

impl Ord for Value {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.partial_cmp(other).unwrap()
    }
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

impl From<char> for Value {
    fn from(value: char) -> Self {
        Value::Char(value)
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

impl From<HashMap<Value, Value>> for Value {
    fn from(value: HashMap<Value, Value>) -> Self {
        Value::Map(Box::new(value))
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

impl From<u32> for Value {
    fn from(value: u32) -> Self {
        Value::Number(value as f64)
    }
}

impl From<usize> for Value {
    fn from(value: usize) -> Self {
        Value::Number(value as f64)
    }
}

impl From<u8> for Value {
    fn from(value: u8) -> Self {
        Value::Number(value as f64)
    }
}

impl From<i16> for Value {
    fn from(value: i16) -> Self {
        Value::BitSeq(value.unsigned_abs())
    }
}

impl From<bool> for Value {
    fn from(value: bool) -> Self {
        Value::Bool(value)
    }
}

impl<T> From<Option<T>> for Value
where
    Value: From<T>,
{
    fn from(value: Option<T>) -> Self {
        match value {
            Some(v) => Value::from(v),
            None => Value::None,
        }
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
            Value::Bool(b) => (if b { "true" } else { "false" }).to_string(),
            Value::Number(n) => n.to_string(),
            Value::Fun(_, _) => format!("{value}"),
            Value::Symbol(s) => s.to_string(),
            Value::None => "".to_string(),
            Value::Vec(_) => format!("{}", value.clone()),
            Value::BitSeq(b) => format!("{:#018b}", b),
            Value::Quoted(val) => String::from(*val),
            Value::Map(map) => format!("{map:#?}"),
        }
    }
}

impl Hash for Value {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Self::BitSeq(i) => i.hash(state),
            Self::Bool(b) => b.hash(state),
            Self::Char(c) => c.hash(state),
            Self::Form(Form::Unquoted(form)) | Self::Form(Form::Quoted(form)) => form.hash(state),
            Self::Fun(params, body) => {
                params.hash(state);
                body.hash(state);
            }
            Self::Quoted(x) => x.hash(state),
            Self::Number(n) => n.to_bits().hash(state),
            Self::String(s) | Self::Symbol(s) => s.hash(state),
            Self::Vec(v) => v.hash(state),
            Self::None => 0.hash(state),
            Self::Map(map) => map
                .values()
                .zip(map.keys())
                .collect::<Vec<(&Value, &Value)>>()
                .hash(state),
        }
    }
}

impl Eq for Value {}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::None => write!(f, "void"),
            Self::Form(form) => write!(f, "{form}"),
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
                for (i, val) in body.iter().enumerate() {
                    write!(f, "{val}{}", if i < body.len() - 1 { " " } else { "" })?;
                }
                write!(f, "))")
            }
            Self::Vec(v) => {
                write!(f, "[")?;
                for (i, val) in v.iter().enumerate() {
                    write!(f, "{val}{}", if i < v.len() - 1 { " " } else { "" })?;
                }
                write!(f, "]")
            }
            Self::BitSeq(b) => write!(f, "{:#018b}", b),
            Self::Quoted(val) => write!(f, "{}", *val),
            Self::Map(map) => {
                write!(f, "{{")?;
                for (idx, (k, v)) in map.iter().enumerate() {
                    if idx > 0 {
                        write!(f, "\n ")?;
                    }
                    write!(f, "{k} {v}")?;
                }
                write!(f, "}}")
            }
        }
    }
}
