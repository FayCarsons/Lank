use crate::utils::{
    error::{IterResult, LankError},
    value::Form,
};

use super::{eval_form, eval_value, fun::*, Env, EnvPtr, EvalResult, Value};
use std::{iter, rc::Rc};

pub fn eval_def(list: &[Value], env: &mut EnvPtr) -> EvalResult {
    let def_err = Err(LankError::SyntaxError);

    let [name, value] = list else {
        return def_err;
    };

    let name = match name {
        Value::Symbol(s) => s,
        _ => return def_err,
    };

    let value = eval_value(value, env)?;
    env.borrow_mut().set(name, value);
    Ok(Value::Void)
}

pub fn eval_nil(list: &[Value], env: &mut EnvPtr) -> EvalResult {
    let val = list.first().ok_or(LankError::NoChildren)?;

    let val = eval_value(val, env)?;

    Ok(Value::Bool(nil(&val)))
}

pub fn eval_some(list: &[Value], env: &mut EnvPtr) -> EvalResult {
    let val = list.first().ok_or(LankError::NoChildren)?;

    let val = eval_value(val, env)?;

    Ok(Value::Bool(!nil(&val)))
}

pub fn eval_do(list: &[Value], env: &mut EnvPtr) -> EvalResult {
    let exprs = list
        .iter()
        .map(|expr| {
            if let Value::Form(Form::Unquoted(vals)) = expr {
                eval_form(vals, env)
            } else {
                eval_value(expr, env)
            }
        })
        .collect::<IterResult>()?;

    Ok(exprs.into_iter().last().unwrap())
}

pub fn eval_symbol(s: &str, env: &EnvPtr) -> EvalResult {
    let val = env.borrow_mut().get(s);
    match val {
        Some(v) => Ok(v.clone()),
        None => Err(LankError::Other(format!("Undefined variable {s}"))),
    }
}

pub fn eval_fn_def(list: &[Value]) -> EvalResult {
    let [params, body @ ..] = list else {
        return Err(LankError::FunctionFormat)
    };


    let params = if let Value::Vec(vals) = params {
        Rc::from(
            vals.iter()
                .map(|o| match o {
                    Value::Symbol(s) => Ok((&*s).to_string()),
                    _ => Err(format!("Invalid function params")),
                })
                .collect::<Result<Vec<String>, String>>()?,
        )
    } else {
        return Err(LankError::FunctionFormat);
    };

    Ok(Value::Fun(Rc::from(params), Form::from(body)))
}

pub fn defn(list: &[Value], env: &mut EnvPtr) -> EvalResult {
    let [name, fun @ ..] = list else {
        return Err(LankError::FunctionFormat)
    };

    let fun = eval_fn_def(fun)?;

    eval_def(&[name.clone(), fun], env)
}

pub fn eval_fn_call(name: &str, list: &[Value], env: &mut EnvPtr) -> EvalResult {
    let func = env
        .borrow_mut()
        .get(name)
        .ok_or(LankError::UnknownFunction(name.to_string()))?;

    match func {
        Value::Fun(params, body) => {
            let mut temp_env = Env::new_extended(env.clone());
            let vals = list
                .iter()
                .map(|v| eval_value(v, env))
                .collect::<IterResult>()?;
            params
                .iter()
                .zip(vals.iter())
                .for_each(|(param, val)| temp_env.borrow_mut().set(param, val.clone()));
            eval_form(&body, &mut temp_env)
        }
        _ => Err(LankError::UnknownFunction(name.to_string())),
    }
}

pub fn eval_let(list: &[Value], env: &mut EnvPtr) -> EvalResult {
    let [ref binding_form, ref body] = list[..] else {
        return Err(LankError::SyntaxError);
    };

    let mut temp_env = Env::new_extended(env.clone());

    let Value::Vec(bindings) = binding_form else {
        return Err(LankError::SyntaxError);
    };

    let bindings = bindings.iter().collect::<Vec<&Value>>();
    bindings
        .chunks(2)
        .try_for_each(|slice| -> Result<(), String> {
            let [var, val] = *slice else {
                return Err("Syntax error!".to_owned());
            };

            let res = eval_value(val, env)?;

            eval_def(&[var.clone(), res], &mut temp_env)?;
            Ok(())
        })?;

    eval_value(body, &mut temp_env)
}

pub fn eval_repeat(list: &[Value], env: &mut EnvPtr) -> EvalResult {
    let [num, expr] = list else {
        return Err(LankError::NumArguments("repeat".to_owned(), 2));
    };

    let Value::Number(num) = eval_value(num, env)? else {
        return Err(LankError::WrongType("repeat".to_owned()));
    };

    let coll = iter::repeat(expr)
        .take(num as usize)
        .map(|v| eval_value(v, env))
        .collect::<IterResult>()?;

    Ok(Value::from(coll))
}
