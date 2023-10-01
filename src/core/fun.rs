#![allow(non_upper_case_globals)]
use rand::{thread_rng, Rng};
use std::{
    fs::File,
    io::{Read, Write},
    rc::Rc,
};

use crate::{
    core::args::assert_string,
    utils::error::{IterResult, LankError},
};

use super::{
    args::{assert_num, assert_symbol, get_args},
    eval, eval_value, Env, EnvPtr, EvalResult, Value,
};

// Fix this!! could be handled much more elegantly
pub fn eval_binary_op(list: &[Value], env: &mut EnvPtr) -> EvalResult {
    let Value::Symbol(symbol) = &list[0] else {
        return Err(LankError::SyntaxError);
    };

    let operation: fn(f64, f64) -> f64 = match &**symbol {
        "+" => |a, b| a + b,
        "-" => |a, b| a - b,
        "*" => |a, b| a * b,
        "/" => |a, b| a / b,
        "mod" => |a, b| a % b,
        ">>" => |x, shift| {
            let x = (x as i64).wrapping_shr(shift as u32);
            x as f64
        },
        "<<" => |x, shift| {
            let x = (x as i64).wrapping_shl(shift as u32);
            x as f64
        },
        "exp" => |x, pow| {
            let x = (x as i64).saturating_pow(pow as u32);
            x as f64
        },
        "&" => |a, b| (a as i64 & b as i64) as f64,
        "|" => |a, b| (a as i64 | b as i64) as f64,
        "^" => |a, b| (a as i64 ^ b as i64) as f64,
        ">" => |a, b| if a > b { 1. } else { 0. },
        ">=" => |a, b| {
            if a >= b {
                1.
            } else {
                0.
            }
        },
        "<" => |a, b| if a < b { 1. } else { 0. },
        "<=" => |a, b| {
            if a < b {
                1.
            } else {
                0.
            }
        },
        "!=" => |a, b| if a != b { 1. } else { 0. },
        "==" => |a, b| if a == b { 1. } else { 0. },
        _ => return Err(LankError::WrongType(symbol.to_string())),
    };

    let args = &list[1..];
    let args = args
        .iter()
        .map(|a| eval_value(a, env))
        .collect::<IterResult>()?;

    let res = args
        .into_iter()
        .map(|arg| {
            if let Value::Number(num) = arg {
                num
            } else {
                0.
            }
        })
        .reduce(operation);

    if let Some(result) = res {
        match &**symbol {
            ">" | ">=" | "<" | "<=" | "!=" | "==" => Ok(Value::Bool(result != 0.)),
            _ => Ok(Value::Number(result)),
        }
    } else {
        Err(LankError::NotANumber)
    }
}

pub fn eval_unary(list: &[Value], env: &mut EnvPtr) -> EvalResult {
    let head = &list[0];

    let Value::Symbol(operator) = head else {
        return Err(LankError::FunctionFormat);
    };

    let operation = match &**operator {
        "abs" => |o: Value| -> EvalResult {
            if let Value::Number(num) = o {
                Ok(Value::Number(num.abs()))
            } else if let Value::BitSeq(_) = o {
                Ok(o)
            } else {
                Err(LankError::NotANumber)
            }
        },
        "neg" => |o: Value| -> EvalResult {
            if let Value::Number(num) = o {
                Ok(Value::Number(-num))
            } else {
                Err(LankError::NotANumber)
            }
        },
        "bit-flip" => |o: Value| -> EvalResult {
            if let Value::Number(num) = o {
                Ok(Value::Number((!(num as i64)) as f64))
            } else if let Value::BitSeq(num) = o {
                Ok(Value::BitSeq(!num))
            } else {
                Err(LankError::NotANumber)
            }
        },
        "not" => |o: Value| Ok(Value::Bool(none(&o))),
        _ => return Err(LankError::UnknownFunction(operator.to_string())),
    };

    let arg = eval_value(&list[1], env)?;

    operation(arg)
}

// TODO figure out float equality
// in the meantime this (kind of :/) works
pub fn none(x: &Value) -> bool {
    *x == Value::None || *x == Value::Bool(false) || *x == Value::Number(0.)
}

// IMPLEMENT ARBITRARY ARITIES
pub fn eval_bool(list: &[Value], env: &mut EnvPtr) -> EvalResult {
    let operator = assert_symbol(&list[0], LankError::SyntaxError)?;
    let args = &list[1..];

    let args = args
        .iter()
        .map(|a| eval_value(a, env))
        .collect::<IterResult>()?;

    let [lhs, rhs] = get_args::<2>(&args, LankError::NumArguments(operator.to_string(), 2))?;
    let [lhs, rhs] = [none(&lhs), none(&rhs)];

    match &*operator {
        "xor" => Ok(Value::Bool(lhs ^ rhs)),
        "or" => Ok(Value::Bool(lhs | rhs)),
        "eq" => Ok(Value::Bool(lhs == rhs)),
        "and" => Ok(Value::Bool(lhs & rhs)),
        _ => Err(LankError::UnknownFunction(operator.to_string())),
    }
}

pub fn display(list: &[Value], env: &mut EnvPtr) -> EvalResult {
    list.iter().for_each(|x| {
        let res = eval_value(x, env);
        print!(
            "{} ",
            match res {
                Ok(v) => v,
                Err(_) => x.clone(),
            }
        );
    });
    println!();
    std::io::stdout().flush().unwrap();
    Ok(Value::None)
}

pub fn gen_rand(list: &[Value], env: &mut EnvPtr) -> EvalResult {
    let mut rng = thread_rng();

    let list = list
        .iter()
        .map(|v| eval_value(v, env))
        .collect::<IterResult>()?;

    match &list[..] {
        [high] => {
            let num = assert_num(high, LankError::WrongType("rand".to_owned()))?;
            if num <= 1. {
                return Ok(Value::Number(0.));
            }
            Ok(Value::Number(rng.gen_range(0..num as usize) as f64))
        }
        [low, high] => {
            let (Value::Number(low), Value::Number(high)) = (low, high) else {
                return Err(LankError::WrongType("Rand".to_owned()));
            };
            Ok(Value::Number(
                rng.gen_range(*low as usize..*high as usize) as f64
            ))
        }
        _ => Err(LankError::NumArguments("Rand".to_owned(), 1)),
    }
}

pub fn run_file(list: &[Value]) -> EvalResult {
    let filename = assert_string(&list[0], LankError::WrongType("run-file".to_owned()))?;
    let mut file = File::open(filename.as_ref())?;

    let mut buffer = String::new();
    file.read_to_string(&mut buffer).unwrap();

    let env = &mut Env::new_ptr();

    let begin = std::time::Instant::now();
    let result = eval(&format!("({buffer})"), env);
    let end = begin.elapsed();
    println!("Program Duration: {end:?}");
    result
}

pub fn eval_type_of(list: &[Value], env: &mut EnvPtr) -> EvalResult {
    let list = list
        .iter()
        .map(|v| {
            let evald = eval_value(v, env)?;
            let _type = evald.type_of();
            Ok(Value::from(_type))
        })
        .collect::<IterResult>()?;

    if list.len() == 1 {
        Ok(list[0].clone())
    } else {
        Ok(Value::from(list))
    }
}

pub fn eval_long(list: &[Value], env: &mut EnvPtr) -> EvalResult {
    let arg = eval_value(&list[0], env)?;

    let n = assert_num(&arg, LankError::WrongType("Long".to_owned()))?;

    Ok(Value::Number(n.floor()))
}

pub fn eval_char(list: &[Value], env: &mut EnvPtr) -> EvalResult {
    let arg = eval_value(&list[0], env)?;

    match arg {
        Value::Number(n) => {
            let int = u8::try_from(n as u32).map_err(|err| err.to_string())?;
            let char = char::from(int);
            Ok(Value::Char(char))
        }
        Value::String(s) => {
            if s.len() == 1 {
                Ok(Value::Char(
                    s.chars()
                        .next()
                        .ok_or_else(|| LankError::WrongType("Char".to_owned()))?,
                ))
            } else {
                Err(LankError::WrongType("Char".to_owned()))
            }
        }
        Value::Char(_) => Ok(arg),
        _ => Err(LankError::WrongType("Char".to_owned())),
    }
}
