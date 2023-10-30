#![allow(non_upper_case_globals)]
use std::io::Write;

use model::{
    error::{IterResult, LankError},
    value::Args,
};
use rand::{thread_rng, Rng};

use super::{
    args::{assert_num, assert_symbol, eval_args, get_args},
    eval_value, Env, EnvPtr, EvalResult, Value,
};

// Fix this!! could be handled much more elegantly
pub fn eval_binary_op(list: Args, env: &mut EnvPtr) -> EvalResult {
    let arg = list.first().ok_or_else(|| LankError::SyntaxError)?;
    let symbol = assert_symbol(arg, LankError::SyntaxError)?;

    let operation: fn(f64, f64) -> f64 = match &*symbol {
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

    let args = eval_args(&list[1..], env)?;

    let res = args
        .iter()
        .map(|arg| assert_num(arg, LankError::WrongType(symbol.to_string())))
        .collect::<Result<Vec<f64>, LankError>>()?
        .into_iter()
        .reduce(operation)
        .ok_or_else(|| LankError::NotANumber)?;

    match &*symbol {
        ">" | ">=" | "<" | "<=" | "!=" | "==" => Ok(Value::Bool(res != 0.)),
        _ => Ok(Value::Number(res)),
    }
}

pub fn eval_unary(list: Args, env: &mut EnvPtr) -> EvalResult {
    let head = list.first().ok_or_else(|| LankError::SyntaxError)?;

    let operator = assert_symbol(head, LankError::SyntaxError)?;

    let operation = match &*operator {
        "abs" => |o: Value| -> EvalResult {
            match o {
                Value::Number(num) => Ok(Value::Number(num.abs())),
                Value::BitSeq(_) => Ok(o),
                _ => Err(LankError::NotANumber),
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
            match o {
                Value::Number(num) => Ok(Value::Number((!(num as i64)) as f64)),
                Value::BitSeq(num) => Ok(Value::BitSeq(!num)),
                _ => Err(LankError::NotANumber),
            }
        },
        "not" => |o: Value| Ok(Value::Bool(o.is_none())),
        _ => unreachable!(),
    };

    let arg = list
        .get(1)
        .ok_or_else(|| LankError::NumArguments("Unary Function".to_owned(), 1))?;
    let arg = eval_value(arg, env)?;
    operation(arg)
}

// IMPLEMENT ARBITRARY ARITIES
pub fn eval_bool(list: Args, env: &mut EnvPtr) -> EvalResult {
    let arg = list.first().ok_or_else(|| LankError::SyntaxError)?;
    let operator = assert_symbol(arg, LankError::SyntaxError)?;

    let [lhs, rhs] = get_args::<2>(
        &list[1..],
        env,
        LankError::NumArguments(operator.to_string(), 2),
    )?;
    let [lhs, rhs] = [lhs.is_some(), rhs.is_some()];

    match &*operator {
        "xor" => Ok(Value::Bool(lhs ^ rhs)),
        "or" => Ok(Value::Bool(lhs | rhs)),
        "eq" => Ok(Value::Bool(lhs == rhs)),
        "and" => Ok(Value::Bool(lhs & rhs)),
        _ => Err(LankError::UnknownFunction(operator.to_string())),
    }
}

pub fn display(list: Args, env: &mut EnvPtr) -> EvalResult {
    list.iter().try_for_each(|x| -> Result<(), LankError> {
        let res = eval_value(x, env)?;
        print!("{res} ");
        Ok(())
    })?;
    println!();
    std::io::stdout().flush().unwrap();
    Ok(Value::None)
}

pub fn gen_rand(list: Args, env: &mut EnvPtr) -> EvalResult {
    let mut rng = thread_rng();

    let list = eval_args(list, env)?;

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
            if high - low <= 1. {
                return Ok(Value::Number(*low));
            }
            Ok(Value::Number(
                rng.gen_range(*low as usize..*high as usize) as f64
            ))
        }
        _ => Err(LankError::NumArguments("Rand".to_owned(), 1)),
    }
}

pub fn eval_type_of(list: Args, env: &mut EnvPtr) -> EvalResult {
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

pub fn eval_long(list: Args, env: &mut EnvPtr) -> EvalResult {
    let arg = list.first().ok_or_else(|| LankError::SyntaxError)?;
    let arg = eval_value(arg, env)?;

    let n = assert_num(&arg, LankError::WrongType("Long".to_owned()))?;

    Ok(Value::Number(n.floor()))
}

pub fn eval_char(list: Args, env: &mut EnvPtr) -> EvalResult {
    let arg = list.first().ok_or_else(|| LankError::SyntaxError)?;
    let arg = eval_value(arg, env)?;

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
