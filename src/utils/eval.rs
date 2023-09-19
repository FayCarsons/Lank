#![allow(non_upper_case_globals)]

use super::{
    env::{Env, EnvPtr},
    error::EvalResult,
    parser::parse,
    value::{Seq, Value},
    BINARY_OPS, BOOL_OPS, UNARY_OPS,
};
use rand::{thread_rng, Rng};
use std::{
    fs::File,
    io::{Read, Write}, rc::Rc,
};

const ArithmeticError: &str = "Incorrect Arithmetic Args";

pub fn eval(program: &str, env: &mut EnvPtr) -> EvalResult {
    match parse(program) {
        Ok(list) => eval_obj(&list, env),
        Err(e) => Err(e),
    }
}

fn eval_obj(obj: &Value, env: &mut EnvPtr) -> EvalResult {
    match obj {
        Value::Symbol(s) => eval_symbol(s, env),
        Value::Form { tokens, .. } => eval_list(tokens, env),
        //Value::Vec(tokens) => create_vec(tokens, env),
        Value::Fun(params, body) => Ok(Value::Fun(params.to_owned(), body.to_owned())),
        x => Ok(x.clone()),
    }
}

fn eval_list(list: &[Value], env: &mut EnvPtr) -> EvalResult {
    let head = match list.first() {
        Some(op) => op,
        None => return Err("Empty Parens".to_owned()),
    };

    match head {
        Value::Symbol(s) => {
            let s = &&**s;
            if BINARY_OPS.contains(s) {
                eval_binary_op(list, env)
            } else if UNARY_OPS.contains(&s) {
                eval_unary(list, env)
            } else if BOOL_OPS.contains(&s) {
                eval_bool(list, env)
            } else {
                match s.as_ref() {
                    "def" => eval_def(&list[1..], env),
                    "defn" => defn(&list[1..], env),
                    "if" | "?" => eval_ternary(&list[1..], env),
                    "when" => eval_when(&list[1..], env),
                    "match" => eval_match(&list[1..], env),
                    "do" => eval_do(&list[1..], env),
                    "fn" => eval_fn_def(&list[1..]),
                    "display" => {
                        display(&list[1..], env);
                        Ok(Value::Void)
                    }
                    "run-file" => run_file(&list[1..]),
                    "rand" => gen_rand(&list[1..], env),
                    "nil?" => eval_nil(&list[1..], env),
                    _ => eval_fn_call(s, &list[1..], env),
                }
            }
        }
        _ => {
            let xs: Vec<EvalResult> = list.iter().map(|v| eval_obj(v, env)).collect();
            for x in xs.iter() {
                if x.is_err() {
                    return x.clone();
                }
            }
            let xs = xs
                .into_iter()
                .map(|x| x.unwrap())
                .filter(|x| *x != Value::Void)
                .collect();
            Ok(Value::Form {
                quoted: false,
                tokens: Rc::new(xs),
            })
        }
    }
}

// Fix this!! could be handled much more elegantly
fn eval_binary_op(list: &[Value], env: &mut EnvPtr) -> EvalResult {
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
        .map(|a| eval_obj(a, env))
        .collect::<Vec<EvalResult>>();

    for arg in args.iter() {
        if arg.is_err() {
            return arg.clone();
        }
    }

    let res = args
        .iter()
        .map(|arg| {
            if let Value::Number(num) = arg.as_ref().unwrap() {
                num
            } else {
                &0.
            }
        })
        .copied()
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

fn eval_unary(list: &[Value], env: &mut EnvPtr) -> EvalResult {
    let Value::Symbol(operator) = &list[0] else {
        todo!()
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
        "not" => |o: Value| Ok(Value::Bool(!nil(&o))),
        _ => return Err(format!("Unknown Unary Operator {operator}")),
    };

    let arg = match eval_obj(&list[1], env) {
        Ok(val) => val,
        x => return x,
    };

    operation(arg)
}

fn nil(x: &Value) -> bool {
    !matches!(x, Value::Void | Value::Number(0f64) | Value::Bool(false))
}

fn eval_bool(list: &[Value], env: &EnvPtr) -> EvalResult {
    let Value::Symbol(operator) = &list[0] else {
        unreachable!()
    };

    let args = &list[1..=2];
    let args = args
        .iter()
        .map(|a| {
            let val = if let Value::Symbol(s) = a {
                eval_symbol(s, env).unwrap()
            } else {
                a.clone()
            };

            nil(&val)
        })
        .collect::<Vec<bool>>();
    let [lhs, rhs] = &args[..] else {
        return Err("Insufficient Condiitional Args!".to_owned());
    };

    match &**operator {
        "xor" => Ok(Value::Bool(lhs ^ rhs)),
        "or" => Ok(Value::Bool(*lhs | *rhs)),
        "eq" => Ok(Value::Bool(lhs == rhs)),
        "and" => Ok(Value::Bool(*lhs & *rhs)),
        _ => Err(format!("Unrecognized Boolean Operator {operator}!")),
    }
}

fn eval_def(list: &[Value], env: &mut EnvPtr) -> EvalResult {
    let def_err = Err("Invalid Def!".to_owned());

    let [name, value] = list else {
        return def_err;
    };

    let name = match name {
        Value::Symbol(s) => s,
        _ => return def_err,
    };

    let value = eval_obj(value, env)?;
    env.borrow_mut().set(name, value);
    Ok(Value::Void)
}

fn eval_nil(list: &[Value], env: &mut EnvPtr) -> EvalResult {
    let val = list.first();
    if val.is_none() {
        return Err("No arguments passed to 'Nil?'".to_owned());
    }

    let val = eval_obj(val.unwrap(), env)?;

    Ok(Value::Bool(nil(&val)))
}

fn eval_ternary(list: &[Value], env: &mut EnvPtr) -> EvalResult {
    let cond = eval_obj(&list[0], env).unwrap_or(Value::Void);
    let bool = nil(&cond);
    if bool {
        eval_obj(&list[1], env)
    } else {
        eval_obj(&list[2], env)
    }
}

fn eval_when(list: &[Value], env: &mut EnvPtr) -> EvalResult {
    let cond = eval_obj(&list[0], env).unwrap_or(Value::Void);
    let bool = nil(&cond);
    if bool {
        eval_obj(&list[1], env)
    } else {
        Ok(Value::Void)
    }
}

fn eval_match(list: &[Value], env: &mut EnvPtr) -> EvalResult {
    let var = eval_obj(&list[0], env);

    match var {
        Ok(val) => {
            let rest: Vec<Value> = list[1..]
                .iter()
                .filter(|&obj| *obj != Value::Void && *obj != Value::Symbol(Rc::from("=>")))
                .cloned()
                .collect();
            for pair in rest.chunks(2) {
                let [cond, expr] = pair else {
                    return Err("Malformed Match Arm!".to_owned());
                };

                let cond = match eval_obj(cond, env) {
                    Ok(v) => v,
                    x => return x,
                };
                if val == cond {
                    return eval_obj(expr, env);
                }
            }
        }
        Err(e) => return Err(e),
    }

    Ok(Value::Void)
}

fn eval_do(list: &[Value], env: &mut EnvPtr) -> EvalResult {
    let exprs = list
        .iter()
        .map(|expr| {
            if let Value::Form { quoted: _, tokens } = expr {
                eval_list(tokens, env)
            } else {
                eval_obj(expr, env)
            }
        })
        .collect::<Vec<EvalResult>>();

    for expr in exprs.iter() {
        if expr.is_err() {
            return expr.clone();
        }
    }

    Ok(exprs.iter().last().unwrap().clone().unwrap().clone())
}

fn eval_symbol(s: &str, env: &EnvPtr) -> EvalResult {
    let val = env.borrow_mut().get(s);
    match val {
        Some(v) => Ok(v.clone()),
        None => Err(s.to_owned()),
    }
}

fn eval_fn_def(list: &[Value]) -> EvalResult {
    let params = if let Value::Form { quoted: _, tokens } = &list[0] {
        Rc::new(tokens
            .iter()
            .map(|o| {
                match o {
                    Value::Symbol(s) => Ok(String::from(&**s)),
                    _ => Err(format!("invalid function params")),
                }
                .unwrap()
            })
            .collect())
    } else {
        return Err("Invalid function params!".to_owned());
    };

    let body = match &list[1] {
        Value::Form { quoted: _, tokens } => tokens.clone(),
        _ => return Err("Invalid function body!".to_owned()),
    };
    Ok(Value::Fun(params, body))
}

fn defn(list: &[Value], env: &mut EnvPtr) -> EvalResult {
    let name = &list[0];
    let fun = &list[1..];

    let fun = match eval_fn_def(fun) {
        Ok(f) => f,
        Err(e) => return Err(e),
    };

    eval_def([name.clone(), fun].as_ref(), env)
}

fn eval_fn_call(name: &str, list: &[Value], env: &mut EnvPtr) -> EvalResult {
    let func = env.borrow_mut().get(name);
    if func.is_none() {
        return Err(format!("{name} Is Not A Function!"));
    }

    match func.unwrap() {
        Value::Fun(params, body) => {
            let mut temp_env = Env::new_extended(env.clone());
            for (i, param) in params.iter().enumerate() {
                let val = eval_obj(&list[i], env);
                val.as_ref()?;
                temp_env.borrow_mut().set(param, val.unwrap())
            }
            eval_obj(
                &Value::Form {
                    quoted: false,
                    tokens: body,
                },
                &mut temp_env,
            )
        }
        _ => Err(format!("{name} Is Not A Function!")),
    }
}

fn display(list: &[Value], env: &mut EnvPtr) {
    print!("Lank> ");
    list.iter().for_each(|x| {
        let res = eval_obj(x, env);
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
}

fn gen_rand(list: &[Value], env: &mut EnvPtr) -> EvalResult {
    let mut rng = thread_rng();

    let list: Vec<EvalResult> = list.iter().map(|v| eval_obj(v, env)).collect();
    for val in list.iter() {
        if val.is_err() {
            return val.clone();
        }
    }
    let list: Seq = list.iter().map(|v| v.clone().unwrap()).collect();
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

fn run_file(list: &[Value]) -> EvalResult {
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

#[test]
fn match_test() {
    let program = "(
                            (def a true)
                            (match a
                                true => 1
                                false => 0)
                        )";
    let result = eval(program, &mut Env::new_ptr());
    assert_eq!(
        result,
        Ok(Value::Form {
            quoted: false,
            tokens: Rc::new(vec!(Value::Number(1.)))
        })
    )
}

#[test]
fn when_test() {
    let program = "(
                            (def a true)
                            (when a 1)
                        )";
    let result = eval(program, &mut Env::new_ptr());
    assert_eq!(
        result.unwrap(),
        Value::Form {
            quoted: false,
            tokens: Rc::new(vec!(Value::Number(1.)))
        }
    )
}

#[test]
fn if_test() {
    let program = "(
                            (def a true)
                            (if a 1 0)
                        )";
    let result = eval(program, &mut Env::new_ptr());
    assert_eq!(
        result.unwrap(),
        Value::Form {
            quoted: false,
            tokens: Rc::new(vec!(Value::Number(1.)))
        }
    )
}

#[test]
fn fn_test() {
    let program = "(
                            (defn inc (x) (+ x 1))
                            (inc 0)
                        )";
    let result = eval(program, &mut Env::new_ptr());
    assert_eq!(
        result.unwrap(),
        Value::Form {
            quoted: false,
            tokens: Rc::new(vec!(Value::Number(1.)))
        }
    );

    let program = "(
        (defn inc (x) (+ x 1))
        (inc (- 2 2))
    )";
    let result = eval(program, &mut Env::new_ptr());
    assert_eq!(
        result.unwrap(),
        Value::Form {
            quoted: false,
            tokens: Rc::new(vec!(Value::Number(1.)))
        }
    );
}
