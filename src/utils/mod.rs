pub mod env;
pub mod eval;
pub mod lexer;
pub mod parser;

const BINARY_OPS: [&str; 17] = ["+", "-", "*" , "/" , "mod" , ">" , ">=" , "<" , "<=" , "!=" , "==" , "<<" , ">>"
, "exp" , "&" , "," , "^"];
const UNARY_OPS: [&str; 4] = ["abs" , "neg" , "bit-flip" , "not"];
const BOOL_OPS: [&str; 4] = ["xor" , "eq" , "or" , "and"];