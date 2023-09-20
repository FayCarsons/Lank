use std::{collections::VecDeque, rc::Rc};

use crate::utils::value::Vector;

use super::{eval_value, EnvPtr, EvalResult, Value};

pub type OptionalEval = Result<Option<Value>, String>;

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
            } else {
                if let Value::Number(num) = idx {
                    let res = tokens.get(*num as usize);
                    res
                } else {
                    return Err(format!("Expected number index, got {idx}"));
                }
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

pub fn eval_first(list: &[Value], env: &mut EnvPtr) -> EvalResult {
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
            } else {
                if let Some(val) = tokens.first() {
                    val.clone()
                } else {
                    Value::Void
                }
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
    let coll = if let Value::Symbol(_s) = head {
        eval_value(head, env)?
    } else {
        head.clone()
    };

    let res = match coll {
        Value::Form { quoted, tokens } => {
            if quoted {
                return Err("Cannot index quoted form!".to_owned());
            } else {
                if let Some(val) = tokens.get(1) {
                    val.clone()
                } else {
                    Value::Void
                }
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
    let coll = if let Value::Symbol(_s) = head {
        eval_value(head, env)?
    } else {
        head.clone()
    };

    let res = match coll {
        Value::Form { quoted, tokens } => {
            if quoted {
                return Err("Cannot index quoted form!".to_owned());
            } else {
                if let Some(val) = tokens.last() {
                    val.clone()
                } else {
                    Value::Void
                }
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
    let coll = if let Value::Symbol(_s) = head {
        eval_value(head, env)?
    } else {
        head.clone()
    };

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
            } else {
                if let Some((first, rest)) = tokens.split_first() {
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

pub fn cons(list: &[Value], env: &mut EnvPtr) -> EvalResult {
    let [coll, val] = &list
        .iter()
        .map(|v| eval_value(v, env))
        .collect::<Result<Vec<Value>, String>>()?[..2]
    else {
        return Err("Cons requires args (cons col value)".to_owned());
    };

    let res: Value = match val {
        Value::Vec(other) => {
            if let Value::Vec(this) = coll {
                Value::Vec(Rc::new(other.iter().chain(this.iter()).cloned().collect()))
            } else {
                return Err(format!("Cons expected coll as 1st arg, got {coll}"))
            }
        }
        _ => {
            if let Value::Vec(this) = coll {
                Value::Vec(Rc::new([val.clone()].iter().chain(this.iter()).cloned().collect()))
            } else {
                return Err(format!("Cons expected coll as 1st arg, got {coll}"))
            }
        }
    };

    Ok(res)
}
