use super::{
    error::{EvalResult, LankError, IterResult},
    value::{Form, Value},
};

use std::{collections::{VecDeque, HashMap}, rc::Rc, sync::OnceLock};

pub struct Config {
    print_tokens: bool,
}
static CONFIG: OnceLock<Config> = OnceLock::new();

use pest::{iterators::Pair, Parser};

#[derive(pest_derive::Parser)]
#[grammar = "lank.pest"]
struct LankParser;

pub trait FromPest {
    fn from_pest(pair: Pair<Rule>) -> EvalResult;
}

impl FromPest for Value {
    fn from_pest(pair: Pair<Rule>) -> EvalResult {
        let rule = pair.as_rule();
        let val: Value = match rule {
            Rule::Void => Value::Void,
            Rule::Form => Value::from_pest(pair.into_inner().next().unwrap())?,
            Rule::Vec | Rule::QuotedForm | Rule::NonQuotedForm => {
                let tokens = pair
                    .into_inner()
                    .map(Self::from_pest)
                    .collect::<IterResult>()?;

                match rule {
                    Rule::Vec => Value::from(VecDeque::from(tokens)),
                    Rule::NonQuotedForm => Value::from(tokens),
                    Rule::QuotedForm => {
                        println!("received quoted form!");
                        Value::Form(Form::Quoted(Rc::new(tokens)))
                    }
                    _ => unreachable!(),
                }
            }
            Rule::Map => {
                let mut map = HashMap::new();
                let vals = pair.into_inner().map(Self::from_pest).collect::<IterResult>()?;
                vals.chunks_exact(2).try_for_each(|chunk| {
                    if chunk.len() != 2 {
                        Err(LankError::SyntaxError)
                    } else {
                        map.insert(chunk[0].clone(), chunk[1].clone());
                        Ok(())
                    }
                })?;
                Value::from(map)
            },
            Rule::QuotedPrimitive => Value::Quoted(Box::new(Self::from_pest(pair.into_inner().next().unwrap())?)),
            Rule::Primitive => Self::from_pest(pair.into_inner().next().unwrap())?,
            Rule::Bool => Value::from(matches!(pair.as_str(), "true")),
            Rule::String => {
                let str = pair.as_str().replace('\"', "");
                Value::from(str.to_owned())
            }
            Rule::Symbol => Value::Symbol(Rc::from(pair.as_str().to_owned())),
            Rule::Char => {
                let str = pair.as_str().replace('\'', "");
                Value::Char(char::from(str.as_bytes()[0]))
            }
            Rule::Number => {
                if let Ok(num) = str::parse::<f64>(pair.as_str()).map_err(|err| err.to_string()) {
                    Value::Number(num)
                } else if let Ok(num) =
                    u16::from_str_radix(&pair.as_str().replace("0b", ""), 2).map_err(|err| err.to_string())
                {
                    Value::BitSeq(num)
                } else if let Ok(num) =
                    u64::from_str_radix(&pair.as_str().replace("0x", ""), 16).map_err(|err| err.to_string())
                {
                    Value::Number(num as f64)
                } else {
                    return Err(LankError::ParseError(format!(
                        "Invalid number: {}",
                        pair.as_str()
                    )));
                }
            }
            _ => Value::from_pest(pair)?,
        };

        Ok(val)
    }
}

pub fn parse(program: &str) -> EvalResult {
    let print_tokens = CONFIG
        .get_or_init(|| {
            let env_args: Vec<String> = std::env::args().collect();
            Config {
                print_tokens: env_args.contains(&"--print-ast".to_string())
                    || env_args.contains(&"-p".to_string()),
            }
        })
        .print_tokens;

    let parsed = LankParser::parse(Rule::Program, program).map_err(|e| e.to_string());

    if print_tokens {
        for token in parsed.iter() {
            println!("{:#?}", *token);
        }
    }

    Value::from_pest(parsed?.next().unwrap())
}

#[test]
fn parse_test() {
    let program = "(+ 1 1)";
    let res = parse(program).unwrap();
    assert_eq!(res, Value::Form(Form::Unquoted(Rc::new(vec![
        Value::Symbol(Rc::from("+".to_string())),
        Value::Number(1.),
        Value::Number(1.)
    ]))))
}
