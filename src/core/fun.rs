#![allow(non_upper_case_globals)]
use rand::{thread_rng, Rng};
use std::{
    fs::File,
    io::{Read, Write},
    rc::Rc,
};

use crate::utils::error::IterResult;

use super::{eval, eval_form, eval_value, Env, EnvPtr, EvalResult, Value};

const ArithmeticError: &str = "Incorrect Arithmetic Args";

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
        todo!()
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
        _ => return Err(format!("Unrecognized Arithmmetic Operation {symbol}")),
    };

    let args = &list[1..];
    let args = args
        .iter()
        .map(|a| eval_value(a, env))
        .collect::<Result<Vec<Value>, String>>()?;

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
        Err(ArithmeticError.to_owned())
    }
}

pub fn eval_unary(list: &[Value], env: &mut EnvPtr) -> EvalResult {
    let head = &list[0];

    let Value::Symbol(operator) = head else {
        return Err(format!("Invalid unary function {head}"));
    };

    let operation = match &**operator {
        "abs" => |o: Value| -> EvalResult {
            if let Value::Number(num) = o {
                Ok(Value::Number(num.abs()))
            } else {
                Err(ArithmeticError.to_owned())
            }
        },
        "neg" => |o: Value| -> EvalResult {
            if let Value::Number(num) = o {
                Ok(Value::Number(-num))
            } else {
                Err(ArithmeticError.to_owned())
            }
        },
        "bit-flip" => |o: Value| -> EvalResult {
            if let Value::Number(num) = o {
                Ok(Value::Number((!(num as i64)) as f64))
            } else {
                Err(ArithmeticError.to_owned())
            }
        },
        "not" => |o: Value| Ok(Value::Bool(nil(&o))),
        _ => return Err(format!("Unknown Unary Operator {operator}")),
    };

    let arg = eval_value(&list[1], env)?;

    operation(arg)
}

// TODO figure out float nil, maybe epsilon ?
pub fn nil(x: &Value) -> bool {
    matches!(x, Value::Void | Value::Number(0f64) | Value::Bool(false))
}

// IMPLEMENT ARBITRARY ARITIES
pub fn eval_bool(list: &[Value], env: &mut EnvPtr) -> EvalResult {
    let head = &list[0];

    let Value::Symbol(operator) = head else {
        return Err(format!("Invalid boolean function {head}"));
    };

    let args = &list[1..];

    let args = args
        .iter()
        .map(|a| eval_value(a, env))
        .collect::<Result<Vec<Value>, String>>()?;

    let [lhs, rhs] = &args[..] else {
        return Err("Invalid args for boolean fn!".to_owned())
    };

    let (lhs, rhs) = match (lhs, rhs) {
        (Value::Bool(a), Value::Bool(b)) => (a, b),
        _ => return Err("Incorrect args for boolean function".to_owned()),
    };

    match &**operator {
        "xor" => Ok(Value::Bool(lhs ^ rhs)),
        "or" => Ok(Value::Bool(lhs | rhs)),
        "eq" => Ok(Value::Bool(lhs == rhs)),
        "and" => Ok(Value::Bool(lhs & rhs)),
        _ => Err(format!("Unrecognized Boolean Operator {operator}!")),
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

    let list: Vec<EvalResult> = list.iter().map(|v| eval_value(v, env)).collect();
    for val in list.iter() {
        if val.is_err() {
            return val.clone();
        }
    }
    let list: Vec<Value> = list.iter().map(|v| v.clone().unwrap()).collect();
    match &list[..] {
        [end] => {
            let Value::Number(num) = end else {
                return Err("Incorrect argument type in Rand".to_owned());
            };
            Ok(Value::Number(rng.gen_range(0..*num as usize) as f64))
        }
        [start, end] => {
            let (Value::Number(st), Value::Number(en)) = (start, end) else {
                return Err("Incorrect argument type in Rand".to_owned());
            };
            Ok(Value::Number(
                rng.gen_range(*st as usize..*en as usize) as f64
            ))
        }
        _ => Err("Rand requires 1-2 args!".to_owned()),
    }
}

pub fn run_file(list: &[Value]) -> EvalResult {
    let filename = if let Value::Symbol(s) = &list[0] {
        s
    } else {
        return Err("Invalid Filename".to_owned());
    };

    let file = File::open(filename.as_ref());
    if file.is_err() {
        return Err(format!("Cannot Read File {filename}"));
    }
    let mut buffer = String::new();
    file.unwrap().read_to_string(&mut buffer).unwrap();

    let env = &mut Env::new_ptr();

    let begin = std::time::Instant::now();
    let result = eval(&format!("({buffer})"), env);
    let end = begin.elapsed();
    println!("Program Duration: {end:?}");
    result
}
