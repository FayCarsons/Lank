type Seq = VecDeque<Value>;



impl Value {
    pub fn from_pest(pair: Pair<Rule>) -> EvalResult {
        let rule = pair.as_rule();
       let val: Value =  match rule {
        Rule::Form => match Value::from_pest(pair.into_inner().next().unwrap()) {
            Ok(v) => v,
            Err(e) => return Err(e)
        },
        Rule::Vec | Rule::QuotedForm | Rule::NonQuotedForm => {
            let tokens = pair.into_inner().map(Self::from_pest).collect::<Vec<EvalResult>>();
            for token in tokens.iter() {
                if token.is_err() {
                    return token.clone()
                }
            }

            let tokens = tokens.iter().map(|v| v.clone().unwrap()).collect::<VecDeque<Value>>();
            match rule {
                Rule::Vec => Value::Vec(tokens),
                Rule::NonQuotedForm => Value::Form { quoted: false, tokens},
                Rule::QuotedForm => Value::Form{quoted: true, tokens},
                _ => unreachable!()
            }
        }
        Rule::Map => {
            Value::String("This is a hashmap".to_owned())
        }
        Rule::Primitive => Self::from_pest(pair.into_inner().next().unwrap())?,
        Rule::Bool => Value::Bool(matches!(pair.as_str(), "true")),
        Rule::String => Value::String(pair.as_str().to_owned()),
        Rule::Symbol => Value::Symbol(pair.as_str().to_owned()),
        Rule::Char => Value::Char(char::from(pair.as_str().chars().nth(0).unwrap())),
        Rule::Number => {
            let can_parse = str::parse::<f64>(pair.as_str());
            if let Ok(num) = can_parse {
                Value::Number(num)
            } else {
                return Err("cannot parse number")
            }
        },
        _ => Value::from_pest(pair).unwrap()
    };

        Ok(val)
    }
    pub fn new_unquoted(tokens: Vec<Value>) -> Self {
        Self::Form{quoted: false, tokens: VecDeque::from(tokens)}
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
    Map(HashMap<Value, Value>),
    Fun(Vec<String>, Self::Form)
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Void => write!(f,""),
            Self::Form => {
                write!(f, "(");
                let Form {_quoted, tokens} = self;
                for token in tokens {
                    write!(f,"{token} ");
                }
                write!(f, ") ")
            },
            Self::Number(n) => write!(f, "{n}"),
            Self::Bool(b) => write!(f, "{b}"),
            Self::String(s) => write!(f, s),
            Self::Symbol(s) => write!(f, s),
            Self::Char(c) => write!(f, "{c}"),
            Self::Fun(params, body) => {

            }
        }
    }
}