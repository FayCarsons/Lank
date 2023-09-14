use super::lexer::{tokenize, Token, NativeOp};

use std::collections::VecDeque;
use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum Object {
    Void,
    Int(i32),
    Bool(bool),
    Symbol(String),
    String(String),
    NativeOp(NativeOp),
    Func(Vec<String>, Vec<Object>),
    List(Vec<Object>),
    Vec(VecDeque<Object>),
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Void => write!(f, "Void"),
            Self::Func(args, body) => {
                write!(f, "Func(")?;
                for arg in args {
                    write!(f, "{arg}")?;
                }
                write!(f, ")")?;
                for expr in body {
                    write!(f, "{expr}")?;
                }
                Ok(())
            }
            Self::List(list) => {
                write!(f, "(")?;
                for obj in list {
                    write!(f, "{obj} ")?;
                }
                write!(f, ")")
            }
            Object::Int(num) => write!(f, "{num}"),
            Object::Bool(b) => write!(f, "{b}"),
            Object::Symbol(s) => write!(f, "{s}"),
            Object::Vec(v) => write!(f, "{v:?}"),
            Object::String(s) => write!(f, "\"{s}\""),
            Object::NativeOp(s) => write!(f, "Native Op {s}"),
        }
    }
}

#[derive(Debug)]
pub struct ParseError {
    err: String,
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Parse Error: {}", self.err)
    }
}

fn parse_list(tokens: &mut Vec<Token>) -> Result<Object, ParseError> {
    let token = tokens.pop();
    if token != Some(Token::LParen) {
        return Err(ParseError {
            err: format!("Expeced Left Paren found {}", token.unwrap()),
        });
    }

    let mut list: Vec<Object> = Vec::new();
    while !tokens.is_empty() {
        let token = tokens.pop();
        if token.is_none() {
            return Err(ParseError {
                err: "Not Enough Tokens".to_string(),
            });
        }
        match token.unwrap() {
            Token::Int(n) => list.push(Object::Int(n)),
            Token::Symbol(s) => list.push(Object::Symbol(s)),
            Token::Bool(b) => list.push(Object::Bool(b)),
            Token::NativeOp(op) => list.push(Object::NativeOp(op)),
            Token::LParen => {
                tokens.push(Token::LParen);
                let rest = parse_list(tokens).unwrap();
                list.push(rest);
            }
            Token::RParen => {
                return Ok(Object::List(list));
            }
            Token::LSquare => {
                tokens.push(Token::LSquare);
                let rest = parse_list(tokens).unwrap();
                list.push(rest);
            }
            Token::RSquare => return Ok(Object::Vec(VecDeque::from(list))),
            
            Token::Sugar => list.push(Object::Void),
        }
    }
    Ok(Object::List(list))
}

pub fn parse(program: &str) -> Result<Object, ParseError> {
    match tokenize(program) {
        Ok(t) => {
            let mut tokens = t.into_iter().rev().collect::<Vec<Token>>();
            parse_list(&mut tokens)
        }
        Err(e) => Err(ParseError {
            err: e.ch.to_string(),
        }),
    }
}

#[test]
fn parse_test() {
    let program = "(
        (def r 10)
        (def pi 314)
        (* pi (* r r))
        )";
    let list = parse(program).unwrap();
    assert_eq!(
        list,
        Object::List(vec![
            Object::List(vec![
                Object::Symbol("def".to_string()),
                Object::Symbol("r".to_string()),
                Object::Int(10),
            ]),
            Object::List(vec![
                Object::Symbol("def".to_string()),
                Object::Symbol("pi".to_string()),
                Object::Int(314),
            ]),
            Object::List(vec![
                Object::NativeOp(NativeOp::Binary("*".to_string())),
                Object::Symbol("pi".to_string()),
                Object::List(vec![
                    Object::NativeOp(NativeOp::Binary("*".to_string())),
                    Object::Symbol("r".to_string()),
                    Object::Symbol("r".to_string()),
                ]),
            ]),
        ])
    );
}
