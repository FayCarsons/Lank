use std::rc::Rc;

pub use crate::utils;
pub use utils::{
    env::{Env, EnvPtr},
    error::EvalResult,
    parser::parse,
    value::Value,
    BINARY_OPS, BOOL_OPS, UNARY_OPS,
};

mod coll;
mod conditional;
mod control;
mod fun;

use coll::*;
use control::*;

use self::{
    conditional::{eval_match, eval_ternary, eval_when},
    control::{defn, eval_def, eval_do, eval_fn_call, eval_fn_def, eval_nil},
    fun::{display, eval_binary_op, eval_bool, eval_lambda_call, eval_unary, gen_rand, run_file},
};

pub fn eval(program: &str, env: &mut EnvPtr) -> EvalResult {
    match parse(program) {
        Ok(list) => eval_value(&list, env),
        Err(e) => Err(e),
    }
}

pub fn eval_value(obj: &Value, env: &mut EnvPtr) -> EvalResult {
    match obj {
        Value::Symbol(s) => eval_symbol(s, env),
        Value::Form { quoted, tokens } => {
            if *quoted {
                Ok(obj.clone())
            } else {
                eval_form(tokens, env)
            }
        }
        //Value::Vec(tokens) => create_vec(tokens, env),
        Value::Fun(params, body) => Ok(Value::Fun(params.to_owned(), body.to_owned())),
        x => Ok(x.clone()),
    }
}

fn eval_form(list: &[Value], env: &mut EnvPtr) -> EvalResult {
    let head = match list.first() {
        Some(op) => op,
        None => return Err("Empty Parens".to_owned()),
    };

    match head {
        Value::Symbol(s) if BINARY_OPS.contains(&&**s) => eval_binary_op(list, env),
        Value::Symbol(s) if UNARY_OPS.contains(&&**s) => eval_unary(list, env),
        Value::Symbol(s) if BOOL_OPS.contains(&&**s) => eval_bool(list, env),
        Value::Symbol(s) => {
            let s = &**s;
            match s {
                "def" => eval_def(&list[1..], env),
                "defn" => defn(&list[1..], env),
                "let" => eval_let(&list[1..], env),
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
                "rand-nth" => rand_nth(&list[1..], env),
                "nil?" => eval_nil(&list[1..], env),
                "nth" => eval_nth(&list[1..], env),
                "list" | "vec" => make_coll(s, &list[1..], env),
                "first" => eval_first(&list[1..], env),
                "second" => eval_second(&list[1..], env),
                "last" => eval_last(&list[1..], env),
                "rest" => eval_rest(&list[1..], env),
                "split" => todo!(),
                "prepend" => eval_prepend(&list[1..], env),
                "append" => eval_append(&list[1..], env),
                "map" => eval_map(&list[1..], env),
                "reduce" => eval_reduce(&list[1..], env),
                "apply" => eval_apply(&list[1..], env),
                _ => eval_fn_call(s, &list[1..], env),
            }
        }

        Value::Fun(params, body) => {
            eval_lambda_call(params, body, &list[1..], env)
        }

        Value::Form { quoted, tokens } => {
            if *quoted {
                Ok(head.clone())
            } else {
                match eval_form(tokens, env) {
                    Ok(Value::Fun(ref params, ref body)) => {
                        eval_lambda_call(params, body, &list[1..], env)
                    }
                    x => x 
                }
            }
        }

        _ => {
            let xs = list
                .iter()
                .map(|v| eval_value(v, env))
                .collect::<Result<Vec<Value>, String>>()?;
            /* WAS THIS NECESSARY? ~INVESTIGATE~
            let xs = xs
                .into_iter()
                .filter(|x| *x != Value::Void)
                .collect(); */
            Ok(Value::Form {
                quoted: false,
                tokens: Rc::new(xs),
            })
        }
    }
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

