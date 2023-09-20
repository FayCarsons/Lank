use super::{eval_form, eval_value, fun::nil, Env, EnvPtr, EvalResult, Value};
use std::rc::Rc;

pub fn eval_def(list: &[Value], env: &mut EnvPtr) -> EvalResult {
    let def_err = Err("Invalid Def!".to_owned());

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
        return Err("No arguments passed to 'Nil?'".to_owned());
    }

    let val = eval_value(val.unwrap(), env)?;

    Ok(Value::Bool(!nil(&val)))
}

pub fn eval_do(list: &[Value], env: &mut EnvPtr) -> EvalResult {
    let exprs = list
        .iter()
        .map(|expr| {
            if let Value::Form { tokens, .. } = expr {
                eval_form(tokens, env)
            } else {
                eval_value(expr, env)
            }
        })
        .collect::<Result<Vec<Value>, String>>()?;

    Ok(exprs.into_iter().last().unwrap())
}

pub fn eval_symbol(s: &str, env: &EnvPtr) -> EvalResult {
    let val = env.borrow_mut().get(s);
    match val {
        Some(v) => Ok(v.clone()),
        None => Err(s.to_owned()),
    }
}

pub fn eval_fn_def(list: &[Value]) -> EvalResult {
    let params = if let Value::Form { quoted: _, tokens } = &list[0] {
        Rc::new(
            tokens
                .iter()
                .map(|o| match o {
                    Value::Symbol(s) => Ok(String::from(&**s)),
                    _ => Err(format!("Invalid function params")),
                })
                .collect::<Result<Vec<String>, String>>()?,
        )
    } else {
        return Err("Invalid function params!".to_owned());
    };

    let body = match &list[1] {
        Value::Form { quoted: _, tokens } => tokens.clone(),
        _ => return Err("Invalid function body!".to_owned()),
    };
    Ok(Value::Fun(params, body))
}

pub fn defn(list: &[Value], env: &mut EnvPtr) -> EvalResult {
    let spl = list.split_first();
    if spl.is_none() {
        return Err("invalid defn!".to_owned());
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
        return Err(format!("{name} Is Not A Function!"));
    }

    match func.unwrap() {
        Value::Fun(params, body) => {
            let mut temp_env = Env::new_extended(env.clone());
            let vals = list
                .iter()
                .map(|v| eval_value(v, env))
                .collect::<Result<Vec<Value>, String>>()?;
            params
                .iter()
                .zip(vals.iter())
                .for_each(|(param, val)| temp_env.borrow_mut().set(param, val.clone()));
            eval_value(
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
