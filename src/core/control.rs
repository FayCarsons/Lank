use crate::utils::{
    error::{IterResult, LankError},
    value::Form,
};

use super::{eval_form, eval_value, fun::*, Env, EnvPtr, EvalResult, Value};
use std::rc::Rc;

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
    let val = list.first();

    if val.is_none() {
        return Err(LankError::FunctionFormat);
    }

    let val = eval_value(val.unwrap(), env)?;

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
    let params = if let Value::Form(Form::Unquoted(vals)) = &list[0] {
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

    let body = match &list[1] {
        Value::Form(Form::Unquoted(vals)) => vals.clone(),
        _ => return Err(LankError::FunctionFormat),
    };

    Ok(Value::Fun(Rc::from(params), body))
}

pub fn defn(list: &[Value], env: &mut EnvPtr) -> EvalResult {
    let spl = list.split_first();

    if spl.is_none() {
        return Err(LankError::SyntaxError);
    }

    let (name, fun) = spl.unwrap();

    let fun = match eval_fn_def(fun) {
        Ok(f) => f,
        Err(e) => return Err(e),
    };

    eval_def([name.clone(), fun].as_ref(), env)
}

pub fn eval_fn_call(name: &str, list: &[Value], env: &mut EnvPtr) -> EvalResult {
    let func = env.borrow_mut().get(name);

    if func.is_none() {
        return Err(LankError::UnknownFunction(name.to_string()));
    }

    match func.unwrap() {
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
