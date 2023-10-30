use model::{env::set_env, error::LankError, value::Args};

use super::{
    args::{assert_symbol, assert_vec, get_args},
    eval_value, Env, EnvPtr, EvalResult, Value,
};
use std::rc::Rc;

pub fn eval_ternary(list: Args, env: &mut EnvPtr) -> EvalResult {
    let [cond, true_body, false_body] = list else {
        return Err(LankError::SyntaxError);
    };

    let cond = eval_value(cond, env)?;

    if cond.is_none() {
        eval_value(false_body, env)
    } else {
        eval_value(true_body, env)
    }
}

pub fn eval_when(list: Args, env: &mut EnvPtr) -> EvalResult {
    let [cond, body] = list else {
        return Err(LankError::SyntaxError);
    };

    let cond = eval_value(cond, env)?;

    if cond.is_none() {
        Ok(Value::None)
    } else {
        eval_value(body, env)
    }
}

pub fn eval_match(list: Args, env: &mut EnvPtr) -> EvalResult {
    let val = list.first().ok_or(LankError::SyntaxError)?;
    let val = eval_value(val, env)?;

    let rest: Vec<&Value> = list[1..]
        .iter()
        .filter(|&obj| obj.is_some() && **obj != Value::Symbol(Rc::from("=>")))
        .cloned()
        .collect();

    for pair in rest.chunks(2) {
        let [cond, expr] = get_args(pair, env, LankError::SyntaxError)?;

        if val == cond {
            return Ok(expr);
        }
    }

    Ok(Value::None)
}

pub fn eval_if_let(list: Args, env: &mut EnvPtr) -> EvalResult {
    let [binding, true_body, false_body] = get_args::<3>(list, env, LankError::SyntaxError)?;
    let binding = assert_vec(&binding, LankError::SyntaxError)?;

    let [name, val] = binding.iter().take(2).collect::<Vec<&Value>>()[..] else {
        return Err(LankError::SyntaxError);
    };

    let val = eval_value(val, env)?;

    if val.is_none() {
        eval_value(&false_body, env)
    } else {
        let name = assert_symbol(name, LankError::SyntaxError)?;
        let mut temp_env = Env::extend(env.clone());
        set_env(name.as_ref(), &val, &temp_env)?;
        eval_value(&true_body, &mut temp_env)
    }
}

pub fn eval_when_let(list: Args, env: &mut EnvPtr) -> EvalResult {
    let [binding, body] = get_args::<2>(list, env, LankError::SyntaxError)?;
    let binding = assert_vec(&binding, LankError::SyntaxError)?;

    let [name, val] = binding.iter().take(2).collect::<Vec<&Value>>()[..] else {
        return Err(LankError::SyntaxError);
    };

    let val = eval_value(val, env)?;

    if val.is_none() {
        Ok(Value::None)
    } else {
        let name = assert_symbol(name, LankError::SyntaxError)?;
        let mut temp_env = Env::extend(env.clone());
        set_env(name.as_ref(), &val, &temp_env)?;
        eval_value(&body, &mut temp_env)
    }
}

pub fn eval_if_not(list: Args, env: &mut EnvPtr) -> EvalResult {
    let [cond, true_body, false_body] = get_args::<3>(list, env, LankError::SyntaxError)?;

    let cond = eval_value(&cond, env)?;

    if cond.is_none() {
        eval_value(&true_body, env)
    } else {
        eval_value(&false_body, env)
    }
}

pub fn eval_when_not(list: Args, env: &mut EnvPtr) -> EvalResult {
    let [cond, body] = get_args::<2>(list, env, LankError::SyntaxError)?;

    let cond = eval_value(&cond, env)?;

    if cond.is_none() {
        eval_value(&body, env)
    } else {
        Ok(Value::None)
    }
}

pub fn check_type(list: Args, env: &mut EnvPtr) -> EvalResult {
    let [op, arg] = get_args::<2>(list, env, LankError::NumArguments("Char?".to_owned(), 1))?;

    let Value::Symbol(op) = op else {
        unreachable!()
    };
    let arg = eval_value(&arg, env)?;

    let res = match &*op {
        "char?" => matches!(arg, Value::Char(_)),
        "number?" => matches!(arg, Value::Number(_)),
        "coll?" => matches!(arg, Value::Form(_) | Value::Vec(_) | Value::String(_)),
        "vec?" => matches!(arg, Value::Vec(_)),
        "list?" => matches!(arg, Value::Form(_)),
        "string?" => matches!(arg, Value::String(_)),
        "symbol?" => matches!(arg, Value::Symbol(_)),
        "fn?" => matches!(arg, Value::Fun(_, _)),
        "bool?" => matches!(arg, Value::Bool(_)),
        "map?" => matches!(arg, Value::Map(_)),
        _ => return Err(LankError::UnknownFunction(op.to_string())),
    };

    Ok(Value::from(res))
}
