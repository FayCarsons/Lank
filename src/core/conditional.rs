use crate::utils::{error::LankError, value::Form, env::set_env};

use super::{eval_value, fun::nil, Env, EnvPtr, EvalResult, Value, control::eval_symbol};
use std::rc::Rc;

pub fn eval_ternary(list: &[Value], env: &mut EnvPtr) -> EvalResult {
    let [cond, true_body, false_body] = list else {
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
    let [cond, body] = list else {
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
    let [binding, true_body, false_body] = list else {
        return Err(LankError::SyntaxError);
    };

    let Value::Vec(binding) = binding else {
        return Err(LankError::SyntaxError);
    };

    let [name, val] = binding.iter().collect::<Vec<&Value>>()[..] else {
        return Err(LankError::SyntaxError);
    };

    let val = eval_value(val, env)?;

    if nil(&val) {
        eval_value(false_body, env)
    } else {
        let Value::Symbol(name) = name else {
            return Err(LankError::SyntaxError);
        };
        let mut temp_env = Env::extend(env.clone());
        set_env(&name.to_string(), &val, &mut temp_env)?;
        eval_value(true_body, &mut temp_env)
    }
}

pub fn eval_when_let(list: &[Value], env: &mut EnvPtr) -> EvalResult {
    let [binding, body] = list else {
        return Err(LankError::SyntaxError);
    };

    let Value::Vec(binding) = binding else {
        return Err(LankError::SyntaxError);
    };

    let [name, val] = binding.iter().collect::<Vec<&Value>>()[..] else {
        return Err(LankError::SyntaxError);
    };

    let val = eval_value(val, env)?;

    if nil(&val) {
        Ok(Value::Void)
    } else {
        let Value::Symbol(name) = name else {
            return Err(LankError::SyntaxError);
        };
        let mut temp_env = Env::extend(env.clone());
        set_env(&name.to_string(), &val, &mut temp_env)?;
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

pub fn eval_is_char(list: &[Value], env: &mut EnvPtr) -> EvalResult {
    let arg = list
        .first()
        .ok_or(LankError::NumArguments("Char?".to_owned(), 1))?;
    let arg = eval_value(arg, env)?;

    if let Value::Char(_) = arg {
        Ok(Value::from(true))
    } else {
        Ok(Value::from(false))
    }
}

pub fn eval_is_num(list: &[Value], env: &mut EnvPtr) -> EvalResult {
    let arg = list
        .first()
        .ok_or(LankError::NumArguments("Num?".to_owned(), 1))?;
    let arg = eval_value(arg, env)?;

    if let Value::Number(_) = arg {
        Ok(Value::from(true))
    } else {
        Ok(Value::from(false))
    }
}

pub fn eval_is_coll(list: &[Value], env: &mut EnvPtr) -> EvalResult {
    let arg = list
        .first()
        .ok_or(LankError::NumArguments("Coll?".to_owned(), 1))?;
    let arg = eval_value(arg, env)?;

    match arg {
        Value::Form(Form::Unquoted(_)) => Ok(Value::from(true)),
        Value::Vec(_) => Ok(Value::from(true)),
        _ => Ok(Value::from(false)),
    }
}

pub fn eval_is_vec(list: &[Value], env: &mut EnvPtr) -> EvalResult {
    let arg = list
        .first()
        .ok_or(LankError::NumArguments("Vec?".to_owned(), 1))?;
    let arg = eval_value(arg, env)?;

    if let Value::Vec(_) = arg {
        Ok(Value::from(true))
    } else {
        Ok(Value::from(false))
    }
}

pub fn eval_is_list(list: &[Value], env: &mut EnvPtr) -> EvalResult {
    let arg = list
        .first()
        .ok_or(LankError::NumArguments("List?".to_owned(), 1))?;
    let arg = eval_value(arg, env)?;

    if let Value::Form(_) = arg {
        Ok(Value::from(true))
    } else {
        Ok(Value::from(false))
    }
}

pub fn eval_is_string(list: &[Value], env: &mut EnvPtr) -> EvalResult {
    let arg = list
        .first()
        .ok_or(LankError::NumArguments("String?".to_owned(), 1))?;
    let arg = eval_value(arg, env)?;

    if let Value::String(_) = arg {
        Ok(Value::from(true))
    } else {
        Ok(Value::from(false))
    }
}

pub fn eval_is_symbol(list: &[Value], env: &mut EnvPtr) -> EvalResult {
    let arg = list
        .first()
        .ok_or(LankError::NumArguments("Symbol?".to_owned(), 1))?;
    let arg = eval_value(arg, env)?;

    if let Value::Symbol(_) = arg {
        Ok(Value::from(true))
    } else {
        Ok(Value::from(false))
    }
}

pub fn eval_is_bool(list: &[Value], env: &mut EnvPtr) -> EvalResult {
    let arg = list
        .first()
        .ok_or(LankError::NumArguments("Bool?".to_owned(), 1))?;
    let arg = eval_value(arg, env)?;

    if let Value::Bool(_) = arg {
        Ok(Value::from(true))
    } else {
        Ok(Value::from(false))
    }
}

// FIX!!! Does ot recognize native fns
pub fn eval_is_fun(list: &[Value], env: &mut EnvPtr) -> EvalResult {
    let arg = list
        .first()
        .ok_or(LankError::NumArguments("Fn?".to_owned(), 1))?;
    let arg = eval_value(arg, env)?;

    if let Value::Fun(_, _) = arg {
        Ok(Value::from(true))
    } else {
        Ok(Value::from(false))
    }
}

pub fn eval_is_map(list: &[Value], env: &mut EnvPtr) -> EvalResult {
    let arg = list.first().ok_or(LankError::NumArguments("Map?".to_owned(), 1))?;
    
    let arg = if let Value::Symbol(maybe) = arg {
        eval_symbol(maybe, env)?
    } else {
        arg.clone()
    };

    if let Value::Map(_) = arg {
        Ok(Value::from(true))
    } else {
        Ok(Value::from(false))
    }
}
