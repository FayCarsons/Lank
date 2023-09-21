use std::{collections::VecDeque, rc::Rc};

use rand::{seq::SliceRandom, thread_rng};

use crate::utils::{
    error::{IterResult, LankError},
    value::Form,
};

use super::{control::eval_symbol, eval_form, eval_value, EnvPtr, EvalResult, Value};

pub fn make_coll(coll_type: &str, list: &[Value], env: &mut EnvPtr) -> EvalResult {
    let coll = match coll_type {
        "vec" => Value::from(
            list.iter()
                .map(|v| eval_value(v, env))
                .collect::<Result<VecDeque<Value>, LankError>>()?,
        ),
        "list" => Value::from(
            list.iter()
                .map(|v| eval_value(v, env))
                .collect::<IterResult>()?,
        ),
        "str" => {
            let res = list
                .iter()
                .cloned()
                .try_reduce(|a, b| eval_concat(&[a, b], env))?;
            if res.is_none() {
                return Err(LankError::WrongType("String".to_owned()));
            }
            res.unwrap()
        }
        _ => unreachable!(),
    };
    Ok(coll)
}

pub fn eval_nth(list: &[Value], env: &mut EnvPtr) -> EvalResult {
    let [coll, idx] = &list
        .iter()
        .map(|v| eval_value(v, env))
        .collect::<Result<Vec<Value>, LankError>>()?[..2]
    else {
        return Err(LankError::Other(
            "Nth requires args (nth col idx)".to_owned(),
        ));
    };

    let res = match coll {
        Value::Form(Form::Unquoted(vals)) => {
            if let Value::Number(num) = idx {
                let res = vals.get(*num as usize);
                res
            } else {
                return Err(LankError::NotANumber);
            }
        }
        Value::Vec(vector) => {
            if let Value::Number(num) = idx {
                vector.get(*num as usize)
            } else {
                return Err(LankError::NotANumber);
            }
        }
        _ => return Err(LankError::NotANumber),
    };

    Ok(if let Some(num) = res {
        num.clone()
    } else {
        Value::Void
    })
}

pub fn rand_nth(list: &[Value], env: &mut EnvPtr) -> EvalResult {
    let coll = eval_value(&list[0], env)?;

    let elem = match coll {
        Value::Form(Form::Unquoted(vals)) => {
            let res = vals.choose(&mut thread_rng());
            if res.is_none() {
                return Err(LankError::NoChildren);
            }
            res.unwrap().clone()
        }
        Value::Vec(vec) => {
            let vec = Vec::from_iter(vec.iter().cloned());
            let res = vec.choose(&mut thread_rng());
            if res.is_none() {
                return Err(LankError::NoChildren);
            }
            res.unwrap().clone()
        }
        Value::String(s) => {
            let res = s.as_bytes().choose(&mut thread_rng());
            if res.is_none() {
                return Err(LankError::NoChildren);
            }
            Value::Char(char::from(*res.unwrap()))
        }
        _ => return Err(LankError::WrongType("Nth".to_owned())),
    };

    Ok(elem.clone())
}

pub fn eval_first(list: &[Value], env: &mut EnvPtr) -> EvalResult {
    let head = &list[0];
    let coll = eval_value(head, env)?;

    let res = match coll {
        Value::Form(Form::Unquoted(vals)) => {
            if let Some(val) = vals.first() {
                val.clone()
            } else {
                Value::Void
            }
        }
        Value::Vec(vector) => {
            if let Some(val) = vector.front() {
                val.clone()
            } else {
                Value::Void
            }
        }
        _ => return Err(LankError::WrongType("First".to_owned())),
    };

    Ok(res)
}

pub fn eval_second(list: &[Value], env: &mut EnvPtr) -> EvalResult {
    let head = &list[0];
    let coll = eval_value(head, env)?;

    let res = match coll {
        Value::Form(Form::Unquoted(vals)) => {
            if let Some(val) = vals.get(1) {
                val.clone()
            } else {
                Value::Void
            }
        }
        Value::Vec(vector) => {
            if let Some(val) = vector.get(1) {
                val.clone()
            } else {
                Value::Void
            }
        }
        _ => return Err(LankError::WrongType("Second".to_owned())),
    };

    Ok(res)
}

pub fn eval_last(list: &[Value], env: &mut EnvPtr) -> EvalResult {
    let head = &list[0];
    let coll = eval_value(head, env)?;

    let res = match coll {
        Value::Form(Form::Unquoted(vals)) => {
            if let Some(val) = vals.last() {
                val.clone()
            } else {
                Value::Void
            }
        }
        Value::Vec(vector) => {
            if let Some(val) = vector.back() {
                val.clone()
            } else {
                Value::Void
            }
        }
        _ => return Err(LankError::WrongType("Last".to_owned())),
    };

    Ok(res)
}

pub fn eval_rest(list: &[Value], env: &mut EnvPtr) -> EvalResult {
    let not_coll = Err(LankError::WrongType("Rest".to_owned()));

    let head = &list[0];
    let coll = eval_value(head, env)?;

    let res = match coll {
        Value::Form(Form::Unquoted(vals)) => Value::from(vals[1..].to_vec()),
        Value::Vec(vector) => Value::Vec(Rc::new(
            vector.iter().skip(1).cloned().collect::<VecDeque<Value>>(),
        )),
        _ => return not_coll,
    };

    Ok(res)
}

pub fn eval_prepend(list: &[Value], env: &mut EnvPtr) -> EvalResult {
    let not_coll = Err(LankError::WrongType("Prepend".to_owned()));

    let [coll, val] = &list
        .iter()
        .map(|v| eval_value(v, env))
        .collect::<Result<Vec<Value>, LankError>>()?[..2]
    else {
        return not_coll;
    };

    let res: Value = match val {
        Value::Vec(other) => match coll {
            Value::Vec(this) => {
                Value::Vec(Rc::new(other.iter().chain(this.iter()).cloned().collect()))
            }

            Value::Form(Form::Unquoted(vals)) => Value::from(
                other
                    .iter()
                    .chain(vals.iter())
                    .cloned()
                    .collect::<Vec<Value>>(),
            ),
            _ => return not_coll,
        },

        x => match coll {
            Value::Vec(this) => Value::Vec(Rc::new(
                [x.clone()].iter().chain(this.iter()).cloned().collect(),
            )),

            Value::Form(Form::Unquoted(vals)) => Value::from(
                [x.clone()]
                    .iter()
                    .chain(vals.iter())
                    .cloned()
                    .collect::<Vec<Value>>(),
            ),
            _ => return not_coll,
        },
    };

    Ok(res)
}

pub fn eval_append(list: &[Value], env: &mut EnvPtr) -> EvalResult {
    let not_coll = Err(LankError::WrongType("Append".to_owned()));

    let [coll, val] = &list
        .iter()
        .map(|v| eval_value(v, env))
        .collect::<Result<Vec<Value>, LankError>>()?[..2]
    else {
        return not_coll;
    };

    let res: Value = match val {
        Value::Vec(other) => match coll {
            Value::Vec(this) => {
                Value::Vec(Rc::new(this.iter().chain(other.iter()).cloned().collect()))
            }

            Value::Form(Form::Unquoted(vals)) => Value::from(
                vals.iter()
                    .chain(other.iter())
                    .cloned()
                    .collect::<Vec<Value>>(),
            ),
            _ => return not_coll,
        },

        x => match coll {
            Value::Vec(this) => Value::Vec(Rc::new(
                this.iter().chain([x.clone()].iter()).cloned().collect(),
            )),

            Value::Form(Form::Unquoted(vals)) => Value::from(
                vals.iter()
                    .chain([x.clone()].iter())
                    .cloned()
                    .collect::<Vec<Value>>(),
            ),
            _ => return not_coll,
        },
    };

    Ok(res)
}

pub fn eval_map(list: &[Value], env: &mut EnvPtr) -> EvalResult {
    let not_coll = Err(LankError::WrongType("Map".to_owned()));

    let [fun, coll] = &list[..2] else {
        return not_coll;
    };

    let coll = if let Value::Symbol(s) = coll {
        eval_symbol(s, env)?
    } else if let Value::Form(Form::Unquoted(vals)) = coll {
        eval_form(vals, env)?
    } else {
        coll.clone()
    };

    let fun = match fun {
        Value::Fun(_, _) | Value::Symbol(_) => fun.clone(),

        Value::Form(Form::Unquoted(vals)) => eval_form(vals, env)?,

        _ => return not_coll,
    };

    match coll {
        Value::Vec(vector) => {
            let res = vector
                .iter()
                .map(|v| eval_form(&[fun.clone(), v.clone()], env))
                .collect::<Result<Vec<Value>, LankError>>()?;
            Ok(Value::Vec(Rc::new(res.into())))
        }
        Value::Form(Form::Unquoted(vals)) => {
            let res = vals
                .iter()
                .map(|v| eval_form(&[fun.clone(), v.clone()], env))
                .collect::<Result<Vec<Value>, LankError>>()?;
            Ok(Value::from(res))
        }
        _ => not_coll,
    }
}

pub fn eval_reduce(list: &[Value], env: &mut EnvPtr) -> EvalResult {
    let not_coll = Err(LankError::WrongType("Reduce".to_owned()));

    let [fun, coll] = &list[..2] else {
        return not_coll;
    };

    let coll = if let Value::Symbol(s) = coll {
        eval_symbol(s, env)?
    } else if let Value::Form(Form::Unquoted(vals)) = coll {
        eval_form(vals, env)?
    } else {
        coll.clone()
    };

    let fun = match fun {
        Value::Fun(_, _) | Value::Symbol(_) => fun.clone(),

        Value::Form(Form::Unquoted(vals)) => eval_form(vals, env)?,
        _ => return not_coll,
    };

    match coll {
        Value::Vec(vector) => {
            let mut it = vector.iter().cloned();
            try_fold_val(&mut it, &fun, env)
        }
        Value::Form(Form::Unquoted(vals)) => {
            let mut it = vals.iter().cloned();
            Ok(try_fold_val(&mut it, &fun, env)?)
        }
        _ => not_coll,
    }
}

pub fn try_fold_val(
    iter: &mut impl Iterator<Item = Value>,
    fun: &Value,
    env: &mut EnvPtr,
) -> EvalResult {
    let mut accum = match iter.next() {
        Some(v) => v,
        None => return Err(LankError::NumArguments("Reduce".to_owned(), 1)),
    };

    for x in iter {
        let res = eval_form(&[fun.clone(), x, accum], env);

        if res.is_ok() {
            accum = res.unwrap()
        } else {
            return Err(LankError::SyntaxError);
        }
    }

    Ok(accum)
}

pub fn eval_apply(list: &[Value], env: &mut EnvPtr) -> EvalResult {
    let not_coll = Err(LankError::WrongType("Apply".to_owned()));

    let [op, try_coll] = &list[..2] else {
        return Err(LankError::SyntaxError);
    };

    let mut coll = match try_coll {
        Value::Symbol(s) => {
            let res = eval_symbol(s, env)?;
            match res {
                Value::Vec(v) => v.iter().cloned().collect::<Vec<Value>>(),
                Value::Form(Form::Unquoted(vals)) => vals.to_vec(),
                _ => return not_coll,
            }
        }

        Value::Vec(v) => v.iter().cloned().collect::<Vec<Value>>(),

        Value::Form(Form::Unquoted(vals)) => match eval_form(vals, env)? {
            Value::Form(Form::Unquoted(vals)) => vals.to_vec(),
            Value::Vec(v) => v.iter().cloned().collect::<Vec<Value>>(),
            _ => return not_coll,
        },

        _ => return not_coll,
    };

    coll.insert(0, op.clone());

    eval_form(&coll, env)
}

pub fn eval_concat(list: &[Value], env: &mut EnvPtr) -> EvalResult {
    let args = list
        .iter()
        .map(|v| eval_value(v, env))
        .collect::<IterResult>()?;

    let [lhs, rhs] = &args[..] else {
        return Err(LankError::NumArguments("Concat".to_owned(), 2));
    };

    match (lhs, rhs) {
        (Value::String(s), Value::Char(c)) => {
            //let s = s.replace("\"", "");
            let s = format!("{s}{c}");
            Ok(Value::from(s))
        }
        (Value::Char(c), Value::String(s)) => {
            let s = format!("{c}{s}");
            Ok(Value::from(s))
        }
        (Value::Char(a), Value::Char(b)) => Ok(Value::from(format!("{a}{b}"))),
        (Value::String(a), Value::String(b)) => Ok(Value::from(a.to_string() + &b)),
        _ => Err(LankError::WrongType("concat".to_owned())),
    }
}
