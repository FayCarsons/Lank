use model::{
    env::{get_env, set_env},
    error::{IterResult, LankError},
    value::{Args, Form},
};

use super::{
    args::{assert_fn, assert_num, assert_symbol, assert_vec, eval_args},
    eval_value, Env, EnvPtr, EvalResult, Value,
};
use std::{iter, rc::Rc};

pub fn eval_def(list: Args, env: &mut EnvPtr) -> EvalResult {
    let [name, value] = list else {
        return Err(LankError::SyntaxError);
    };

    let name = &assert_symbol(name, LankError::SyntaxError)?;

    let value = &eval_value(value, env)?;
    set_env(name, value, env)?;
    Ok(Value::None)
}

pub fn eval_none(list: Args, env: &mut EnvPtr) -> EvalResult {
    let val = list.first().ok_or(LankError::NoChildren)?;
    let val = &eval_value(val, env)?;

    Ok(Value::Bool(val.is_none()))
}

pub fn eval_some(list: Args, env: &mut EnvPtr) -> EvalResult {
    let val = list.first().ok_or(LankError::NoChildren)?;
    let val = &eval_value(val, env)?;

    Ok(Value::Bool(val.is_some()))
}

pub fn eval_do(list: Args, env: &mut EnvPtr) -> EvalResult {
    let exprs = list
        .iter()
        .map(|expr| eval_value(expr, env))
        .collect::<IterResult>()?;

    Ok(exprs.into_iter().last().unwrap())
}

pub fn eval_symbol(s: &str, env: &EnvPtr) -> EvalResult {
    let val = get_env(s, env).ok_or_else(|| LankError::Other(format!("Unbound variable {s}")))?;

    Ok(val)
}

pub fn eval_fn_def(list: Args) -> EvalResult {
    let (params, body) = list.split_first().ok_or(LankError::FunctionFormat)?;

    let params = assert_vec(params, LankError::FunctionFormat)?;
    let params = Rc::from(
        params
            .iter()
            .map(|o| match o {
                Value::Symbol(s) => Ok(s.to_string()),
                _ => Err("Invalid function params".to_string()),
            })
            .collect::<Result<Vec<String>, String>>()?,
    );

    Ok(Value::Fun(params, Form::from(body)))
}

pub fn defn(list: Args, env: &mut EnvPtr) -> EvalResult {
    let (name, fun) = list.split_first().ok_or(LankError::FunctionFormat)?;
    let fun = eval_fn_def(fun)?;

    eval_def(&[name, &fun], env)
}

pub fn eval_block(form: Form, env: &mut EnvPtr) -> EvalResult {
    let res = form
        .iter()
        .map(|v| eval_value(v, env))
        .collect::<IterResult>()?;

    Ok(res.last().ok_or(LankError::SyntaxError)?.clone())
}

pub fn eval_fn_call(name: &str, list: Args, env: &mut EnvPtr) -> EvalResult {
    let func = get_env(name, env).ok_or_else(|| LankError::UnknownFunction(name.to_string()))?;

    let (params, body) = assert_fn(&func, LankError::UnknownFunction(name.to_string()))?;

    let mut temp_env = Env::extend(env.clone());
    let vals = eval_args(list, env)?;
    params
        .iter()
        .zip(vals.iter())
        .try_for_each(|(param, val)| set_env(param, val, &temp_env))?;

    eval_block(body, &mut temp_env)
}

pub fn eval_lambda_call(lambda: &Value, args: Args, env: &mut EnvPtr) -> EvalResult {
    let args = eval_args(args, env)?;
    let mut temp_env = Env::extend(env.clone());
    let (params, body) = assert_fn(lambda, LankError::SyntaxError)?;

    params
        .iter()
        .zip(args.iter())
        .try_for_each(|(param, val)| set_env(param, val, &temp_env))?;

    eval_block(body.clone(), &mut temp_env)
}

pub fn eval_let(list: Args, env: &EnvPtr) -> EvalResult {
    let (binding_form, body) = list.split_first().ok_or(LankError::SyntaxError)?;
    let mut temp_env = Env::extend(env.clone());
    let bindings = assert_vec(binding_form, LankError::SyntaxError)?;
    let bindings = bindings.iter().cloned().collect::<Vec<Value>>();

    // FIX THIS
    bindings
        .chunks_exact(2)
        .try_for_each(|slice| -> Result<(), LankError> {
            let [var, val] = slice else {
                return Err(LankError::SyntaxError);
            };

            let res = eval_value(val, &mut temp_env)?;

            eval_def(&[var, &res], &mut temp_env)?;
            Ok(())
        })?;

    eval_block(Form::from(body), &mut temp_env)
}

pub fn eval_repeat(list: Args, env: &mut EnvPtr) -> EvalResult {
    let [num, expr] = list else {
        return Err(LankError::NumArguments("Repeat".to_owned(), 2));
    };

    let num = eval_value(num, env)?;
    let num = assert_num(
        &eval_value(&num, env)?,
        LankError::WrongType("Repeat".to_owned()),
    )?;

    let coll = iter::repeat(expr)
        .take(num as usize)
        .map(|v| eval_value(v, env))
        .collect::<IterResult>()?;

    Ok(Value::from(coll))
}
