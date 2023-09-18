#![allow(non_upper_case_globals)]

use super::{
    env::{Env, EnvPtr},
    parser::parse, value::Value, BINARY_OPS, UNARY_OPS, BOOL_OPS,
};
use rand::Rng;
use std::{io::{Write, Read}, fs::File};

type EvalResult = Result<Value, String>;

const ArithmeticError: &str = "Incorrect Arithmetic Args";

pub fn eval(program: &str, env: &mut EnvPtr) -> EvalResult {
    match parse(program) {
        Ok(list) => eval_obj(&list, env),
        Err(e) => Err(format!("{e}")),
    }
}

fn eval_obj(obj: &Value, env: &mut EnvPtr) -> EvalResult {
    match obj {
        Value::Symbol(s) => eval_symbol(s, env),
        Value::Form(list) => eval_list(list, env),
        Value::Fun(params, body) => Ok(Value::Fun(params.clone(), body.clone())),
        x => Ok(*x),
    }
}

fn eval_list(list: &[Value], env: &mut EnvPtr) -> EvalResult {
    let head = match list.first() {
        Some(op) => op,
        None => return Err("Empty Parens".to_owned()),
    };
    if BINARY_OPS.contains(head) {
        eval_binary_op(list, env)
    } else if UNARY_OPS.contains(head) {
        eval_unary(list, env)
    } else if BOOL_OPS.contains(head) {
        eval_bool(list, env)
    } else {
        match head {
            Value::Symbol(s) => match s.as_str() {
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
                _ => eval_fn_call(s, &list[1..], env),
            },
            _ => {
                let mut new_list = Vec::new();
                for obj in list {
                    let res = eval_obj(&obj, env)?;
                    match res {
                        Value::Void => {}
                        _ => new_list.push(res),
                    }
                }
                Ok(Value::Form(new_list))
            }
        }
    }
    
}
// Fix this!! could be handled much more elegantly
fn eval_binary_op(list: &[Value], env: &mut EnvPtr) -> EvalResult {
    let Value::Symbol(symbol) = &list[0]; 

    let operation: fn(i32, i32) -> i32 = match symbol {
        "+" => i32::saturating_add,
        "-" => i32::saturating_sub,
        "*" => i32::saturating_mul,
        "/" => i32::saturating_div,
        "mod" => |a, b| a % b,
        ">>" => |x, shift| x.wrapping_shr(shift as u32),
        "<<" => |x, shift| x.wrapping_shl(shift as u32),
        "exp" => |x, pow| x.saturating_pow(pow as u32),
        "&" => |a, b| a & b,
        "|" => |a, b| a | b,
        "^" => |a, b| a ^ b,
        ">" => |a, b| if a > b { 1 } else { 0 },
        ">=" => |a, b| {
            if a >= b {
                1
            } else {
                0
            }
        },
        "<" => |a, b| if a < b { 1 } else { 0 },
        "<=" => |a, b| {
            if a < b {
                1
            } else {
                0
            }
        },
        "!=" => |a, b| if a != b { 1 } else { 0 },
        "==" => |a, b| if a == b { 1 } else { 0 },
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
            if let Value::Int(num) = arg.as_ref().unwrap() {
                num
            } else {
                &0
            }
        })
        .copied()
        .reduce(operation);

    if let Some(result) = res {
        match symbol {
            ">" | ">=" | "<" | "<=" | "!=" | "==" => Ok(Value::Bool(result != 0)),
            _ => Ok(Value::Int(result)),
        }
    } else {
        Err(ArithmeticError.to_string())
    }
}

fn eval_unary(list: &[Value], env: &mut EnvPtr) -> EvalResult {
    let Value::Symbol(operator) = &list[0];

    let operation = match operator {
        "abs" => |o: Value| -> EvalResult {
            if let Value::Int(num) = o {
                Ok(Value::Int(num.abs()))
            } else {
                Err(ArithmeticError.to_string())
            }
        },
        "neg" => |o: Value| -> EvalResult {
            if let Value::Int(num) = o {
                Ok(Value::Int(-num))
            } else {
                Err(ArithmeticError.to_string())
            }
        },
        "bit-flip" => |o: Value| -> EvalResult {
            if let Value::Int(num) = o {
                Ok(Value::Int(!num))
            } else {
                Err(ArithmeticError.to_string())
            }
        },
        "not" => |o: Value| {
            Ok(Value::Bool(! nil(&o)))
        },
        _ => return Err(format!("Unknown Unary Operator {operator}")),
    };

    let arg = match eval_obj(&list[1], env) {
        Ok(val) => val,
        x => return x,
    };

    operation(arg)
}

fn nil(x: &Value) -> bool {
    ! matches!(x, Value::Void | Value::Int(0) | Value::Bool(false))
}

fn eval_bool(list: &[Value], env: &mut EnvPtr) -> EvalResult {
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
        return Err("Insufficient Condiitional Args!".to_string());
    };

    match operator {
        "xor" => Ok(Value::Bool(lhs ^ rhs)),
        "or" => Ok(Value::Bool(*lhs | *rhs)),
        "eq" => Ok(Value::Bool(lhs == rhs)),
        "and" => Ok(Value::Bool(*lhs & *rhs)),
        _ => Err(format!("Unrecognized Boolean Operator {operator}!")),
    }
}

fn eval_def(list: &[Value], env: &mut EnvPtr) -> EvalResult {
    let deff_err = Err("Invalid Def!".to_string());

    let [name, value] = list else {
        return deff_err;
    };

    let name = match name {
        Value::Symbol(s) => s,
        _ => return deff_err,
    };

    let value = eval_obj(value, env).unwrap();
    env.borrow_mut().set(name, value);
    Ok(Value::Void)
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
                .cloned()
                .filter(|obj| *obj != Value::Void)
                .collect();
            for pair in rest.chunks(2) {
                let [cond, expr] = pair else {
                    return Err("Malformed Match Arm!".to_string());
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
            if let Value::Form{_quoted, l} = expr {
                eval_list(l, env)
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
        None => Err(s.to_string()),
    }
}

fn eval_fn_def(list: &[Value]) -> EvalResult {
    let params: Vec<String> = if let Value::Form{quoted: false, l} = &list[0] {
        l.iter()
            .map(|o| {
                match o {
                    Value::Symbol(s) => Ok(s.clone()),
                    _ => Err(format!("invalid Fun params")),
                }
                .unwrap()
            })
            .collect()
    } else {
        return Err("Invalid Fun Params".to_string());
    };

    let body = match &list[1] {
        Value::Form{_quoted, list} => list.clone(),
        _ => return Err("Invalid Fun!".to_string()),
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
    if func.is_none() {return Err(format!("{name} Is Not A Function!"))}

    match func.unwrap() {
        Value::Fun(params, body) => {
            let mut temp_env = Env::new_extended(env.clone());
            for (i, param) in params.iter().enumerate() {
                let val = eval_obj(&list[i], env);
                val.as_ref()?;
                temp_env.borrow_mut().set(param, val.unwrap())
            }
            eval_obj(&Value::Form(body), &mut temp_env)
        }
        _ => Err(format!("{name} Is Not A Function!"))
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
    let args: Vec<Result<i32, ()>> = list
        .iter()
        .map(|o| {
            let eval = eval_obj(o, env);

            if eval.is_err() {
                return Err(());
            }

            if let Value::Int(num) = eval.unwrap() {
                Ok(num)
            } else {
                Err(())
            }
        })
        .collect();

    if args.iter().any(|x| x.is_err()) {
        Err("Invalid Range Args In Rand".to_string())
    } else {
        let args: Vec<i32> = args.iter().map(|o| o.unwrap()).collect();
        let mut rng = rand::thread_rng();
        match args.len() {
            1 => Ok(Value::Int(rng.gen_range(0..args[0]))),
            2 => Ok(Value::Int(rng.gen_range(args[0]..args[1]))),
            _ => Err(format!("Rand Expected 1-2 Args, Received {}", args.len())),
        }
    }
}

fn run_file(list: &[Value]) -> EvalResult {
    let filename = if let Value::Symbol(s) = &list[0] {
        s
    } else {
        return Err("Invalid Filename".to_string())
    };

    let file = File::open(filename);
    if file.is_err() {return Err(format!("Cannot Read File {filename}"))}
    let mut buffer = String::new();
    file.unwrap().read_to_string(&mut buffer).unwrap();
    
    let env = &mut Env::new_ptr();

    let begin = std::time::Instant::now();
    let result = eval(&buffer, env);
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
    assert_eq!(result, Ok(Value::Form{quoted: false, tokens: vec!(Value::Number(1.))}))
}

#[test]
fn when_test() {
    let program = "(
                            (def a true)
                            (when a 1)
                        )";
    let result = eval(program, &mut Env::new_ptr());
    assert_eq!(result.unwrap(), Value::Form(vec!(Value::Int(1))))
}

#[test]
fn if_test() {
    let program = "(
                            (def a true)
                            (if a 1 0)
                        )";
    let result = eval(program, &mut Env::new_ptr());
    assert_eq!(result.unwrap(), Value::Form(vec!(Value::Int(1))))
}

#[test]
fn fn_test() {
    let program = "(
                            (defn inc (x) (+ x 1))
                            (inc 0)
                        )";
    let result = eval(program, &mut Env::new_ptr());
    assert_eq!(result.unwrap(), Value::Form(vec!(Value::Int(1))));

    let program = "(
        (defn inc (x) (+ x 1))
        (inc (- 2 2))
    )";
    let result = eval(program, &mut Env::new_ptr());
    assert_eq!(result.unwrap(), Value::Form(vec!(Value::Int(1))));
}
