use crate::utils::{
    env::{get_env, set_env},
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
    set_env(name.as_ref(), &value, env)?;
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
    let val = get_env(s, env)?;

    if nil(&val) {
        Err(LankError::Other(format!("Unbound variable: {s}")))
    } else {
        Ok(val.clone())
    }
}

pub fn eval_fn_def(list: &[Value]) -> EvalResult {
    let [params, body @ ..] = list else {
        return Err(LankError::FunctionFormat);
    };

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
    let [name, fun @ ..] = list else {
        return Err(LankError::FunctionFormat);
    };

    let fun = eval_fn_def(fun)?;

    eval_def(&[name.clone(), fun], env)
}

pub fn eval_fn_body(form: Form, env: &mut EnvPtr) -> EvalResult {
    let res = form
        .iter()
        .map(|v| {
            if let Value::Symbol(s) = v {
                eval_symbol(s, env)
            } else {
                eval_value(v, env)
            }
        })
        .collect::<IterResult>()?;

    Ok(res.last().ok_or(LankError::SyntaxError)?.clone())
}

pub fn eval_fn_call(name: &str, list: &[Value], env: &mut EnvPtr) -> EvalResult {
    let func = get_env(name, env)?;

    match func {
        Value::Fun(params, body) => {
            let mut temp_env = Env::extend(env.clone());
            let vals = list
                .iter()
                .map(|v| eval_value(v, env))
                .collect::<IterResult>()?;
            params
                .iter()
                .zip(vals.iter())
                .try_for_each(|(param, val)| set_env(param, val, &mut temp_env))?;

            eval_fn_body(body, env)
        }
        _ => Err(LankError::UnknownFunction(name.to_string())),
    }
}

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

    let mut temp_env = Env::extend(env.clone());

    params
        .iter()
        .zip(args.iter())
        .try_for_each(|(param, val)| set_env(param, val, &temp_env))?;

    eval_fn_body(Form::from(&body[..]), &mut temp_env)
}

pub fn eval_let(list: &[Value], env: &mut EnvPtr) -> EvalResult {
    let [binding_form, body @ ..] = list else {
        return Err(LankError::SyntaxError);
    };

    let mut temp_env = Env::extend(env.clone());

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

    eval_fn_body(Form::from(body), &mut temp_env)
}

pub fn eval_repeat(list: &[Value], env: &mut EnvPtr) -> EvalResult {
    let [num, expr] = list else {
        return Err(LankError::NumArguments("Repeat".to_owned(), 2));
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
