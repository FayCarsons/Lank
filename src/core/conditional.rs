use super::{eval_value, fun::nil, EnvPtr, EvalResult, Value};
use std::rc::Rc;

pub fn eval_ternary(list: &[Value], env: &mut EnvPtr) -> EvalResult {
    let cond = eval_value(&list[0], env).unwrap_or(Value::Void);

    let bool = !nil(&cond);

    if bool {
        eval_value(&list[1], env)
    } else {
        eval_value(&list[2], env)
    }
}

pub fn eval_when(list: &[Value], env: &mut EnvPtr) -> EvalResult {
    let cond = eval_value(&list[0], env).unwrap_or(Value::Void);

    let bool = !nil(&cond);

    if bool {
        eval_value(&list[1], env)
    } else {
        Ok(Value::Void)
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
                    return Err("Malformed Match Arm!".to_owned());
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
