use std::error::Error;
use std::{fmt, str::FromStr};

use super::{BINARY_OPS, UNARY_OPS, BOOL_OPS};

#[derive(Debug, Clone, PartialEq)]

pub enum NativeOp {
    Binary(String),
    Unary(String),
    Boolean(String)
}

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Int(i32),
    Symbol(String),
    Bool(bool),
    NativeOp(NativeOp),
    LParen,
    RParen,
    LSquare,
    RSquare,
    Sugar,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Int(n) => write!(f, "{n}"),
            Self::Symbol(s) => write!(f, "{s}"),
            Self::Bool(b) => write!(f, "{b}"),
            Self::LParen => write!(f, "("),
            Self::RParen => write!(f, ")"),
            Self::LSquare => write!(f, "["),
            Self::RSquare => write!(f, "]"),
            Self::NativeOp(op) => write!(f, "{op}"),
            Self::Sugar => Ok(()),
        }
    }
}

impl<'a> fmt::Display for NativeOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Binary(op) => write!(f, "{op}"),
            Self::Unary(op) => write!(f, "{op}"),
            Self::Boolean(op) => write!(f, "{op}")
        }
    }
}

#[derive(Debug)]
pub struct TokenError {
    pub ch: char,
}

impl Error for TokenError {}
impl fmt::Display for TokenError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "unexpected character: {}", self.ch)
    }
}

pub fn tokenize(program: &str) -> Result<Vec<Token>, TokenError> {
    let expanded = program
        .replace('(', " ( ")
        .replace(')', " ) ")
        .replace('[', " [ ")
        .replace(']', " ] ");
    let words = expanded.split_whitespace();
    Ok(words
        .map(|word| {
            if BINARY_OPS.contains(&word) {
                Token::NativeOp(NativeOp::Binary(word.to_string()))
            } else if UNARY_OPS.contains(&word) {
                Token::NativeOp(NativeOp::Unary(word.to_string()))
            } else if BOOL_OPS.contains(&word) {
                Token::NativeOp(NativeOp::Boolean(word.to_string()))
            } else {
                match word {
                    "(" => Token::LParen,
                    ")" => Token::RParen,
                    "[" => Token::LSquare,
                    "]" => Token::RSquare,
                    "=>" => Token::Sugar,
                    s => {
                        if let Ok(num) = i32::from_str(s) {
                            Token::Int(num)
                        } else if let Ok(b) = bool::from_str(s) {
                            Token::Bool(b)
                        } else {
                            Token::Symbol(s.to_string())
                        }
                    }
                }
            }
        })
        .collect::<Vec<Token>>())
}

#[test]
fn lex_test() {
    let tokens = tokenize("(+ 1 2)");
    assert_eq!(
        tokens.unwrap(),
        vec![
            Token::LParen,
            Token::Symbol("+".to_string()),
            Token::Int(1),
            Token::Int(2),
            Token::RParen
        ]
    );
}
