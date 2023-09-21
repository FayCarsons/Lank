use super::{
    error::{EvalResult, LankError},
    value::{Form, Value},
};

use std::{collections::VecDeque, rc::Rc, sync::OnceLock};

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
            Rule::Form => Value::from_pest(pair.into_inner().next().unwrap())?,
            Rule::Vec | Rule::QuotedForm | Rule::NonQuotedForm => {
                let tokens = pair
                    .into_inner()
                    .map(Self::from_pest)
                    .collect::<Vec<EvalResult>>();
                for token in tokens.iter() {
                    if token.is_err() {
                        return token.to_owned();
                    }
                }
                let tokens = tokens.iter().map(|v| v.clone().unwrap());

                match rule {
                    Rule::Vec => Value::Vec(Rc::new(tokens.collect::<VecDeque<Value>>())),
                    Rule::NonQuotedForm => Value::from(tokens.collect::<Vec<Value>>()),
                    Rule::QuotedForm => Value::from(tokens.collect::<Vec<Value>>()),
                    _ => unreachable!(),
                }
            }
            Rule::Map => Value::String(Rc::from("This is a hashmap")),
            Rule::Primitive => Self::from_pest(pair.into_inner().next().unwrap())?,
            Rule::Bool => Value::Bool(matches!(pair.as_str(), "true")),
            Rule::String => {
                let str = pair.as_str();
                Value::from(str[1..str.len() - 1].to_owned())
            }
            Rule::Symbol => Value::Symbol(Rc::from(pair.as_str().to_owned())),
            Rule::Char => {
                let str = pair.as_str();
                Value::Char(char::from(str[1..str.len() - 1].as_bytes()[0]))
            }
            Rule::Number => {
                Value::Number(str::parse::<f64>(pair.as_str()).map_err(|err| err.to_string())?)
            }

            _ => Value::from_pest(pair)?,
        };

        Ok(val)
    }
}

pub fn parse(program: &str) -> EvalResult {
    //let program = program.replace("(", " ( ").replace(")", " ) ").replace("[", " [ ").replace("]", " ] ");

    let print_tokens = CONFIG
        .get_or_init(|| {
            let env_args: Vec<String> = std::env::args().collect();
            Config {
                print_tokens: env_args.contains(&"--print-ast".to_owned())
                    || env_args.contains(&"-p".to_owned()),
            }
        })
        .print_tokens;

    let parsed = LankParser::parse(Rule::Program, program).map_err(|e| e.to_string());

    if print_tokens {
        for token in parsed.iter() {
            println!("{:#?}", *token);
        }
    }

    Ok(Value::from_pest(parsed?.next().unwrap())?)
}
