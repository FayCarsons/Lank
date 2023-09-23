#![allow(non_upper_case_globals)]
use rand::{thread_rng, Rng};
use std::{
    collections::VecDeque,
    convert,
    fs::File,
    io::{Read, Write},
    rc::Rc,
};

use crate::utils::error::{IterResult, LankError};

use super::{eval, eval_form, eval_value, Env, EnvPtr, EvalResult, Value};

pub fn eval_lambda_call(
    params: &Rc<Vec<String>>,
    body: &Rc<Vec<Value>>,
    args: &[Value],
    env: &mut EnvPtr,
) -> EvalResult {
    let args = args
        .iter()
        .map(|v| eval_value(v, env))
        .collect::<IterResult>()?;

    let mut temp_env = Env::new_extended(env.clone());

    params
        .iter()
        .zip(args.iter())
        .for_each(|(param, val)| temp_env.borrow_mut().set(param, val.clone()));
    eval_form(body, &mut temp_env)
}

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
            } else {
                Err(LankError::NotANumber)
            }
        },
        "not" => |o: Value| Ok(Value::Bool(nil(&o))),
        _ => return Err(LankError::UnknownFunction(operator.to_string())),
    };

    let arg = eval_value(&list[1], env)?;

    operation(arg)
}

// TODO figure out float nil, maybe epsilon ?
pub fn nil(x: &Value) -> bool {
    match x {
        Value::Void => true,
        Value::Bool(b) if !b => true,
        Value::Number(n) if *n == 0. => true,
        _ => false,
    }
}

// IMPLEMENT ARBITRARY ARITIES
pub fn eval_bool(list: &[Value], env: &mut EnvPtr) -> EvalResult {
    let head = &list[0];

    let Value::Symbol(operator) = head else {
        return Err(LankError::SyntaxError);
    };

    let args = &list[1..];

    let args = args
        .iter()
        .map(|a| eval_value(a, env))
        .collect::<IterResult>()?;

    let [lhs, rhs] = &args[..] else {
        return Err(LankError::NumArguments(operator.to_string(), 2));
    };

    let (lhs, rhs) = match (lhs, rhs) {
        (Value::Bool(a), Value::Bool(b)) => (a, b),
        _ => return Err(LankError::WrongType(operator.to_string())),
    };

    match &**operator {
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
    Ok(Value::Void)
}

pub fn gen_rand(list: &[Value], env: &mut EnvPtr) -> EvalResult {
    let mut rng = thread_rng();

    let list = list
        .iter()
        .map(|v| eval_value(v, env))
        .collect::<IterResult>()?;

    match &list[..] {
        [end] => {
            let Value::Number(num) = end else {
                return Err(LankError::NumArguments("Rand".to_owned(), 1));
            };
            Ok(Value::Number(rng.gen_range(0..*num as usize) as f64))
        }
        [start, end] => {
            let (Value::Number(st), Value::Number(en)) = (start, end) else {
                return Err(LankError::NumArguments("Rand".to_owned(), 1));
            };
            Ok(Value::Number(
                rng.gen_range(*st as usize..*en as usize) as f64
            ))
        }
        _ => Err(LankError::NumArguments("Rand".to_owned(), 1)),
    }
}

pub fn run_file(list: &[Value]) -> EvalResult {
    let Value::Symbol(filename) = &list[0] else {
        return Err(LankError::WrongType("Run-file".to_owned()));
    };

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
        .map(|v| eval_value(v, env))
        .collect::<IterResult>()?;

    Ok(Value::Symbol(Rc::from(list[0].type_of())))
}

pub fn eval_long(list: &[Value], env: &mut EnvPtr) -> EvalResult {
    let arg = eval_value(&list[0], env)?;

    let Value::Number(n) = arg else {
        return Err(LankError::WrongType("Long".to_owned()));
    };

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
                        .nth(0)
                        .ok_or(LankError::WrongType("Char".to_owned()))?,
                ))
            } else {
                Err(LankError::WrongType("Char".to_owned()))
            }
        }
        Value::Char(_) => Ok(arg),
        _ => Err(LankError::WrongType("Char".to_owned())),
    }
}
