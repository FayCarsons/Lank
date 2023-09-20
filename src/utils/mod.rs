pub mod env;
pub mod error;
pub mod value;

pub mod parser;

pub const BINARY_OPS: [&str; 17] = [
    "+", "-", "*", "/", "mod", ">", ">=", "<", "<=", "!=", "==", "<<", ">>", "exp", "&", ",", "^",
];
pub const UNARY_OPS: [&str; 4] = ["abs", "neg", "bit-flip", "not"];
pub const BOOL_OPS: [&str; 4] = ["xor", "eq", "or", "and"];
