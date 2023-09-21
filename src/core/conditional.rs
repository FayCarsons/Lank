use crate::utils::error::LankError;

use super::{eval_value, fun::nil, Env, EnvPtr, EvalResult, Value};
use std::rc::Rc;

pub fn eval_ternary(list: &[Value], env: &mut EnvPtr) -> EvalResult {
    let [cond, true_body, false_body] = &list[..] else {
        return Err(LankError::SyntaxError);
    };

    let cond = eval_value(cond, env)?;

    if nil(&cond) {
        eval_value(false_body, env)
    } else {
        eval_value(true_body, env)
    }
}

pub fn eval_when(list: &[Value], env: &mut EnvPtr) -> EvalResult {
    let [cond, body] = &list[..] else {
        return Err(LankError::SyntaxError);
    };

    let cond = eval_value(cond, env)?;

    if nil(&cond) {
        Ok(Value::Void)
    } else {
        eval_value(body, env)
    }
}

pub fn eval_match(list: &[Value], env: &mut EnvPtr) -> EvalResult {
    let var = eval_value(&list[0], env);

    match var {
        Ok(val) => {
            let rest: Vec<Value> = list[1..]
                .iter()
                .filter(|&obj| !nil(obj) && *obj != Value::Symbol(Rc::from("=>")))
                .cloned()
                .collect();

            for pair in rest.chunks(2) {
                let [cond, expr] = pair else {
                    return Err(LankError::SyntaxError);
                };

                let cond = match eval_value(cond, env) {
                    Ok(v) => v,
                    x => return x,
                };
                if val == cond {
                    return eval_value(expr, env);
                }
            }
        }
        Err(e) => return Err(e),
    }

    Ok(Value::Void)
}

pub fn eval_if_let(list: &[Value], env: &mut EnvPtr) -> EvalResult {
    let [binding, true_body, false_body] = &list[..] else {
        return Err(LankError::SyntaxError);
    };

    let Value::Vec(binding) = binding else {
        return Err(LankError::SyntaxError);
    };

    let [name, val] = binding.iter().collect::<Vec<&Value>>()[..] else {
        return Err(LankError::SyntaxError);
    };

    let val = eval_value(&val, env)?;

    if nil(&val) {
        eval_value(false_body, env)
    } else {
        let Value::Symbol(name) = name else {
            return Err(LankError::SyntaxError);
        };
        let mut temp_env = Env::new_extended(env.clone());
        temp_env.borrow_mut().set(&name, val);
        eval_value(true_body, &mut temp_env)
    }
}

pub fn eval_when_let(list: &[Value], env: &mut EnvPtr) -> EvalResult {
    let [binding, body] = &list[..] else {
        return Err(LankError::SyntaxError);
    };

    let Value::Vec(binding) = binding else {
        return Err(LankError::SyntaxError);
    };

    let [name, val] = binding.iter().collect::<Vec<&Value>>()[..] else {
        return Err(LankError::SyntaxError);
    };

    let val = eval_value(&val, env)?;

    if nil(&val) {
        Ok(Value::Void)
    } else {
        let Value::Symbol(name) = name else {
            return Err(LankError::SyntaxError);
        };
        let mut temp_env = Env::new_extended(env.clone());
        temp_env.borrow_mut().set(&name, val);
        eval_value(body, &mut temp_env)
    }
}

pub fn eval_if_not(list: &[Value], env: &mut EnvPtr) -> EvalResult {
    let [cond, true_body, false_body] = list else {
        return Err(LankError::SyntaxError);
    };

    let cond = eval_value(cond, env)?;

    if nil(&cond) {
        eval_value(true_body, env)
    } else {
        eval_value(false_body, env)
    }
}

pub fn eval_when_not(list: &[Value], env: &mut EnvPtr) -> EvalResult {
    let [cond, body] = list else {
        return Err(LankError::SyntaxError);
    };

    let cond = eval_value(cond, env)?;

    if nil(&cond) {
        eval_value(body, env)
    } else {
        Ok(Value::Void)
    }
}
