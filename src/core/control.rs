use crate::utils::{
    env::{get_env, set_env},
    error::{IterResult, LankError},
    value::Form,
};

use super::{
    args::{assert_fn, assert_num, assert_vec, get_args},
    eval_form, eval_value,
    fun::*,
    Env, EnvPtr, EvalResult, Value,
};
use std::{iter, rc::Rc};

pub fn eval_def(list: &[Value], env: &mut EnvPtr) -> EvalResult {
    let def_err = Err(LankError::SyntaxError);

    let [name, value] = get_args::<2>(list, LankError::NumArguments("Def".to_owned(), 2))?;

    let name = match name {
        Value::Symbol(s) => s,
        _ => return def_err,
    };

    let value = eval_value(&value, env)?;
    set_env(name.as_ref(), &value, env)?;
    Ok(Value::None)
}

pub fn eval_none(list: &[Value], env: &mut EnvPtr) -> EvalResult {
    let val = list.first().ok_or_else(|| LankError::NoChildren)?;
    let val = eval_value(val, env)?;

    Ok(Value::Bool(none(&val)))
}

pub fn eval_some(list: &[Value], env: &mut EnvPtr) -> EvalResult {
    let val = list.first().ok_or_else(|| LankError::NoChildren)?;
    let val = eval_value(val, env)?;

    Ok(Value::Bool(!none(&val)))
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
    let val = get_env(s, env).ok_or_else(|| LankError::Other(format!("Unbound variable {s}")))?;

    Ok(val)
}

pub fn eval_fn_def(list: &[Value]) -> EvalResult {
    let (params, body) = list
        .split_first()
        .ok_or_else(|| LankError::FunctionFormat)?;

    let params = if let Value::Vec(vals) = params {
        Rc::from(
            vals.iter()
                .map(|o| match o {
                    Value::Symbol(s) => Ok(s.to_string()),
                    _ => Err(format!("Invalid function params")),
                })
                .collect::<Result<Vec<String>, String>>()?,
        )
    } else {
        return Err(LankError::FunctionFormat);
    };

    Ok(Value::Fun(params, Form::from(body)))
}

pub fn defn(list: &[Value], env: &mut EnvPtr) -> EvalResult {
    let (name, fun) = list
        .split_first()
        .ok_or_else(|| LankError::FunctionFormat)?;
    let fun = eval_fn_def(fun)?;

    eval_def(&[name.clone(), fun], env)
}

pub fn eval_block(form: Form, env: &mut EnvPtr) -> EvalResult {
    let res = form
        .iter()
        .map(|v| eval_value(v, env))
        .collect::<IterResult>()?;

    Ok(res.last().ok_or_else(|| LankError::SyntaxError)?.clone())
}

pub fn eval_fn_call(name: &str, list: &[Value], env: &mut EnvPtr) -> EvalResult {
    let func = get_env(name, env).ok_or_else(|| LankError::UnknownFunction(name.to_string()))?;

    let (params, body) = assert_fn(&func, LankError::UnknownFunction(name.to_string()))?;

    let mut temp_env = Env::extend(env.clone());
    let vals = list
        .iter()
        .map(|v| eval_value(v, &mut temp_env))
        .collect::<IterResult>()?;
    params
        .iter()
        .zip(vals.iter())
        .try_for_each(|(param, val)| set_env(param, val, &mut temp_env))?;

    eval_block(body, &mut temp_env)
}

pub fn eval_lambda_call(lambda: &Value, args: &[Value], env: &mut EnvPtr) -> EvalResult {
    let args = args
        .iter()
        .map(|v| eval_value(v, env))
        .collect::<IterResult>()?;

    let mut temp_env = Env::extend(env.clone());

    let Value::Fun(params, body) = lambda else {
        return Err(LankError::SyntaxError);
    };

    params
        .iter()
        .zip(args.iter())
        .try_for_each(|(param, val)| set_env(param, val, &temp_env))?;

    eval_block(body.clone(), &mut temp_env)
}

pub fn eval_let(list: &[Value], env: &mut EnvPtr) -> EvalResult {
    let (binding_form, body) = list.split_first().ok_or_else(|| LankError::SyntaxError)?;
    let mut temp_env = Env::extend(env.clone());
    let bindings = assert_vec(binding_form, LankError::SyntaxError)?;
    let bindings = bindings.iter().cloned().collect::<Vec<Value>>();

    // FIX THIS
    bindings
        .chunks_exact(2)
        .try_for_each(|slice| -> Result<(), String> {
            let [var, val] = get_args::<2>(slice, LankError::SyntaxError)?;

            let res = eval_value(&val, &mut temp_env)?;

            eval_def(&[var.clone(), res], &mut temp_env)?;
            Ok(())
        })?;

    eval_block(Form::from(body), &mut temp_env)
}

pub fn eval_repeat(list: &[Value], env: &mut EnvPtr) -> EvalResult {
    let [num, expr] = get_args::<2>(list, LankError::NumArguments("Repeat".to_owned(), 2))?;

    let num = assert_num(
        &eval_value(&num, env)?,
        LankError::WrongType("repeat".to_owned()),
    )?;

    let coll = iter::repeat(expr)
        .take(num as usize)
        .map(|v| eval_value(&v, env))
        .collect::<IterResult>()?;

    Ok(Value::from(coll))
}
