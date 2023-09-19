use super::{
    error::EvalResult,
    value::{Seq, Value},
};

use std::{collections::VecDeque, sync::OnceLock};

pub struct Config {
    print_tokens: bool
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
                    Rule::Vec => Value::Vec(tokens.collect::<VecDeque<Value>>()),
                    Rule::NonQuotedForm => Value::Form {
                        quoted: false,
                        tokens: tokens.collect::<Seq>(),
                    },
                    Rule::QuotedForm => Value::Form {
                        quoted: true,
                        tokens: tokens.collect::<Seq>(),
                    },
                    _ => unreachable!(),
                }
            }
            Rule::Map => Value::String("This is a hashmap".to_owned()),
            Rule::Primitive => Self::from_pest(pair.into_inner().next().unwrap())?,
            Rule::Bool => Value::Bool(matches!(pair.as_str(), "true")),
            Rule::String => Value::String(pair.as_str().to_owned()),
            Rule::Symbol => Value::Symbol(pair.as_str().to_owned()),
            Rule::Char => Value::Char(pair.as_str().chars().nth(0).unwrap()),
            Rule::Number => {
                let can_parse = str::parse::<f64>(pair.as_str());
                if let Ok(num) = can_parse {
                    Value::Number(num)
                } else {
                    return Err("cannot parse number".to_owned());
                }
            }
            _ => Value::from_pest(pair)?,
        };

        Ok(val)
    }
}

pub fn parse(program: &str) -> EvalResult {
    //let program = program.replace("(", " ( ").replace(")", " ) ").replace("[", " [ ").replace("]", " ] ");

    let print_tokens = CONFIG.get_or_init(|| {
        let env_args: Vec<String> = std::env::args().collect();
        Config {
            print_tokens: env_args.contains(&"--print-ast".to_owned()) || env_args.contains(&"-p".to_owned())
        }
    }).print_tokens;
    

    let parsed = LankParser::parse(Rule::Program, program).map_err(|e| e.to_string());
    
    if print_tokens {
        for token in parsed.iter() {
            println!("{token:#?}");
        }
    }

    Value::from_pest(parsed?.next().unwrap())
}
