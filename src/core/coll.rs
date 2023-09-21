use std::{collections::VecDeque, rc::Rc};


use rand::{thread_rng, seq::SliceRandom};

use super::{control::eval_symbol, eval_form, eval_value, EnvPtr, EvalResult, Value};

pub fn make_coll(coll_type: &str, list: &[Value], env: &mut EnvPtr) -> EvalResult {
    let coll = match coll_type {
        "vec" => Value::from(list.iter().map(|v| eval_value(v, env)).collect::<Result<VecDeque<Value>, String>>()?),
        "list" => Value::from(list.iter().map(|v| eval_value(v, env)).collect::<Result<Vec<Value>, String>>()?),
        _ => unreachable!()
    };
    Ok(coll)
}

pub fn eval_nth(list: &[Value], env: &mut EnvPtr) -> EvalResult {
    let [coll, idx] = &list
        .iter()
        .map(|v| eval_value(v, env))
        .collect::<Result<Vec<Value>, String>>()?[..2]
    else {
        return Err("Nth requires args (nth col idx)".to_owned());
    };

    let res = match coll {
        Value::Form { quoted, tokens } => {
            if *quoted {
                return Err("Cannot index quoted form!".to_owned());
            } else if let Value::Number(num) = idx {
                let res = tokens.get(*num as usize);
                res
            } else {
                return Err(format!("Expected number index, got {idx}"));
            }
        }
        Value::Vec(vector) => {
            if let Value::Number(num) = idx {
                vector.get(*num as usize)
            } else {
                return Err(format!("Expected number index, got {idx}"));
            }
        }
        _ => return Err(format!("Nth expected coll got {coll}")),
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
        Value::Form{tokens, ..} => {
            let res = tokens.choose(&mut thread_rng());
            if res.is_none() {
                return Err("Empty coll!".to_owned())
            }
            res.unwrap().clone()
        },
        Value::Vec(vec) => {
            let vec = Vec::from_iter(vec.iter().cloned());
            let res =  vec.choose(&mut thread_rng());
            if res.is_none() {
                return Err("Empty coll!".to_owned())
            }
            res.unwrap().clone()
        }
        Value::String(s) => {
            let res = s.as_bytes().choose(&mut thread_rng());
            if res.is_none() {
                return Err("Empty coll!".to_owned())
            }
            Value::Char(char::from(*res.unwrap()))
        }
        _ => return Err(format!("Expected coll got {coll}"))
    };

    Ok(elem.clone())
}

pub fn eval_first(list: &[Value], env: &mut EnvPtr) -> EvalResult {
    let head = &list[0];
    let coll = eval_value(head, env)?;

    let res = match coll {
        Value::Form { quoted, tokens } => {
            if quoted {
                return Err("Cannot index quoted form!".to_owned());
            } else if let Some(val) = tokens.first() {
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
        _ => return Err(format!("Expected coll got {coll}")),
    };

    Ok(res)
}

pub fn eval_second(list: &[Value], env: &mut EnvPtr) -> EvalResult {
    let head = &list[0];
    let coll = eval_value(head, env)?;

    let res = match coll {
        Value::Form { quoted, tokens } => {
            if quoted {
                return Err("Cannot index quoted form!".to_owned());
            } else if let Some(val) = tokens.get(1) {
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
        _ => return Err(format!("Expected coll got {coll}")),
    };

    Ok(res)
}

pub fn eval_last(list: &[Value], env: &mut EnvPtr) -> EvalResult {
    let head = &list[0];
    let coll = eval_value(head, env)?;

    let res = match coll {
        Value::Form { quoted, tokens } => {
            if quoted {
                return Err("Cannot index quoted form!".to_owned());
            } else if let Some(val) = tokens.last() {
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
        _ => return Err(format!("Expected coll got {coll}")),
    };

    Ok(res)
}

pub fn eval_rest(list: &[Value], env: &mut EnvPtr) -> EvalResult {
    let head = &list[0];
    let coll = eval_value(head, env)?;

    let res = match coll {
        Value::Form { quoted, tokens } => {
            if quoted {
                return Err("Cannot index quoted form!".to_owned());
            } else {
                Value::Form {
                    quoted: false,
                    tokens: Rc::from(tokens[1..].to_vec()),
                }
            }
        }
        Value::Vec(vector) => Value::Vec(Rc::new(
            vector.iter().skip(1).cloned().collect::<VecDeque<Value>>(),
        )),
        _ => return Err(format!("Expected coll got {coll}")),
    };

    Ok(res)
}

pub fn eval_split(list: &[Value], env: &mut EnvPtr) -> Result<(Value, Value), String> {
    let head = &list[0];
    let coll = if let Value::Symbol(_s) = head {
        eval_value(head, env)?
    } else {
        head.clone()
    };

    let res = match coll {
        Value::Form { quoted, tokens } => {
            if quoted {
                return Err("Cannot index quoted form!".to_owned());
            } else if let Some((first, rest)) = tokens.split_first() {
                (
                    first.clone(),
                    Value::Form {
                        quoted: false,
                        tokens: Rc::new(rest.to_vec()),
                    },
                )
            } else {
                (Value::Void, Value::Void)
            }
        }
        Value::Vec(vector) => {
            let res = (
                vector.front(),
                vector.range(1..).cloned().collect::<VecDeque<_>>(),
            );
            if res.0.is_none() {
                return Err("Cannot split empty vec!".to_owned());
            }

            (res.0.unwrap().clone(), Value::Vec(Rc::new(res.1)))
        }
        _ => return Err(format!("Expected coll got {coll}")),
    };

    Ok(res)
}

pub fn eval_prepend(list: &[Value], env: &mut EnvPtr) -> EvalResult {
    let [coll, val] = &list
        .iter()
        .map(|v| eval_value(v, env))
        .collect::<Result<Vec<Value>, String>>()?[..2]
    else {
        return Err("Prepend requires args (prepend col value)".to_owned());
    };

    let res: Value = match val {
        Value::Vec(other) => {
            match coll {
                Value::Vec(this) => {
                    Value::Vec(Rc::new(other.iter().chain(this.iter()).cloned().collect()))
                }

                Value::Form { quoted, tokens } => {
                    if *quoted {
                        return Err("Cannot prepend quoted form!".to_owned())
                    } else {
                        Value::Form{quoted: false, tokens: Rc::new(other.iter().chain(tokens.iter()).cloned().collect::<Vec<Value>>())}
                    }
                }
                _ => return Err(format!("Prepend expected coll as 1st arg, got {coll}"))
            }
        }

        x => {
            match coll {
                Value::Vec(this) => {
                    Value::Vec(Rc::new([x.clone()].iter().chain(this.iter()).cloned().collect()))
                }

                Value::Form { quoted, tokens } => {
                    if *quoted {
                        return Err("Cannot prepend quoted form!".to_owned())
                    } else {
                        Value::Form{quoted: false, tokens: Rc::new([x.clone()].iter().chain(tokens.iter()).cloned().collect())}
                    }
                }
                _ => return Err(format!("Prepend expected coll as 1st arg, got {coll}"))
            }
        }
    };

    Ok(res)
}

pub fn eval_append(list: &[Value], env: &mut EnvPtr) -> EvalResult {
    let [coll, val] = &list
        .iter()
        .map(|v| eval_value(v, env))
        .collect::<Result<Vec<Value>, String>>()?[..2]
    else {
        return Err("Append requires args (cons col value)".to_owned());
    };

    let res: Value = match val {
        Value::Vec(other) => {
            match coll {
                Value::Vec(this) => {
                    Value::Vec(Rc::new(this.iter().chain(other.iter()).cloned().collect()))
                }

                Value::Form { quoted, tokens } => {
                    if *quoted {
                        return Err("Cannot append quoted form!".to_owned())
                    } else {
                        Value::Form{quoted: false, tokens: Rc::new(tokens.iter().chain(other.iter()).cloned().collect::<Vec<Value>>())}
                    }
                }
                _ => return Err(format!("Append expected coll as 1st arg, got {coll}"))
            }
        }

        x => {
            match coll {
                Value::Vec(this) => {
                    Value::Vec(Rc::new(this.iter().chain([x.clone()].iter()).cloned().collect()))
                }

                Value::Form { quoted, tokens } => {
                    if *quoted {
                        return Err("Cannot append quoted form!".to_owned())
                    } else {
                        Value::Form{quoted: false, tokens: Rc::new(tokens.iter().chain([x.clone()].iter()).cloned().collect())}
                    }
                }
                _ => return Err(format!("Append expected coll as 1st arg, got {coll}"))
            }
        }
    };

    Ok(res)
}

pub fn eval_map(list: &[Value], env: &mut EnvPtr) -> EvalResult {
    let [fun, coll] = &list[..2] else {
        return Err("Map requires args (map col value)".to_owned());
    };

    let coll = if let Value::Symbol(s) = coll {
        eval_symbol(s, env)?
    } else if let Value::Form{tokens, ..} = coll {
        eval_form(tokens, env)?
    } else {
        coll.clone()
    };

    let fun = match fun {
        Value::Fun(_, _) | Value::Symbol(_) => fun.clone(),

        Value::Form { tokens, .. } => eval_form(tokens, env)?,
        
        _ => return Err("Map requires fn as second arg!".to_owned()),
    };

    match coll {
        Value::Vec(vector) => {
            let res = vector
                .iter()
                .map(|v| eval_form(&[fun.clone(), v.clone()], env))
                .collect::<Result<Vec<Value>, String>>()?;
            Ok(Value::Vec(Rc::new(res.into())))
        }
        Value::Form { quoted, tokens } => {
            if quoted {
                Err("Cannot evaluate quoted form!".to_owned())
            } else {
                let res = tokens
                    .iter()
                    .map(|v| eval_form(&[fun.clone(), v.clone()], env))
                    .collect::<Result<Vec<Value>, String>>()?;
                Ok(Value::Form {
                    quoted: false,
                    tokens: Rc::new(res),
                })
            }
        }
        _ => Err("Map expected coll!".to_owned()),
    }
}

pub fn eval_reduce(list: &[Value], env: &mut EnvPtr) -> EvalResult {
    let [fun, coll] = &list[..2] else {
        return Err("Map requires args (map col value)".to_owned());
    };

    let coll = if let Value::Symbol(s) = coll {
        eval_symbol(s, env)?
    } else if let Value::Form{tokens, ..} = coll {
        eval_form(tokens, env)?
    } else {
        coll.clone()
    };

    let fun = match fun {
        Value::Fun(_, _) | Value::Symbol(_) => fun.clone(),

        Value::Form { tokens, .. } => eval_form(tokens, env)?,
        _ => return Err("Map requires fn as second arg!".to_owned()),
    };

    match coll {
        Value::Vec(vector) => {
            let mut it = vector.iter().cloned();
            try_fold_val(&mut it, &fun, env)
        }
        Value::Form { quoted, tokens } => {
            if quoted {
                Err("Cannot evaluate quoted form!".to_owned())
            } else {
                let mut it = tokens.iter().cloned();
                Ok(try_fold_val(&mut it, &fun, env)?)
            }
        }
        _ => Err("Map expected coll!".to_owned()),
    }
}

pub fn try_fold_val(
    iter: &mut impl Iterator<Item = Value>,
    fun: &Value,
    env: &mut EnvPtr,
) -> EvalResult {
    let mut accum = match iter.next() {
        Some(v) => v,
        None => return Err("Not enough args for reduce!".to_owned())
    };

    for x in iter {
        let res = eval_form(&[fun.clone(), x, accum], env);

        if res.is_ok() {
            accum = res.unwrap()
        } else {
            return Err("Reduce error!".to_owned());
        }
    }

    Ok(accum)
}

pub fn eval_apply(list: &[Value], env: &mut EnvPtr) -> EvalResult {
    let [op, try_coll] = &list[..2] else {
        return Err(format!("Apply expected operator && args, got {list:?}"))
    };

    let mut coll = match try_coll {
        Value::Symbol(s) => {
            let res = eval_symbol(s, env)?;
            match res {
                Value::Vec(v) => v.iter().cloned().collect::<Vec<Value>>(),
                 Value::Form{tokens, ..} => tokens.to_vec(),
                _ => return Err(format!("Apply expected coll got {res}"))
            }
        }
        
        Value::Vec(v) => {
            v.iter().cloned().collect::<Vec<Value>>()
        }

        Value::Form{tokens, ..} => {
            match eval_form(tokens, env)? {
                Value::Form{tokens,..} => tokens.to_vec(),
                Value::Vec(v) => v.iter().cloned().collect::<Vec<Value>>(),
                _ => return Err(format!("Apply expected coll got {try_coll}"))
            }
        } 

        _ => return Err(format!("Apply expected coll got {try_coll}"))
    };

    coll.insert(0, op.clone());

    eval_form(&coll, env)
}
