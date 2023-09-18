use super::value::Value;

use env::Env;
use std::{
    collections::VecDeque,
    sync::Mutex, fs,
};

use pest::{iterators::Pair, Parser};

#[derive(pest_derive::Parser)]
#[grammar = "lank.pest"]
struct LankParser;

type EvalResult<'a> = std::result::Result<Value, &'a str>;

pub fn parse(program: &str) -> EvalResult {
    let program = format!("({program})");
    let mut parsed = LankParser::parse(Rule::Program, &program).unwrap();
    let tokens = Value::from_pest(parsed.next().unwrap());
    tokens
}
