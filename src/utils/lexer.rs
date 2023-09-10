use std::error::Error;
use std::{fmt, str::FromStr};

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Int(i32),
    Symbol(String),
    Bool(bool),
    LParen,
    RParen,
    LSquare,
    RSquare,
    Sugar,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::Int(n) => write!(f, "{n}"),
            Token::Symbol(s) => write!(f, "{s}"),
            Token::Bool(b) => write!(f, "{b}"),
            Token::LParen => write!(f, "("),
            Token::RParen => write!(f, ")"),
            Token::LSquare => write!(f, "["),
            Token::RSquare => write!(f, "]"),
            Token::Sugar => Ok(()),
        }
    }
}

unsafe impl Send for Token {}
unsafe impl Sync for Token {}

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
        .map(|word| match word {
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
