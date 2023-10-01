use std::{collections::VecDeque, rc::Rc};

use rand::{seq::SliceRandom, thread_rng};

use crate::utils::{
    error::{IterResult, LankError},
    value::Form,
};

use super::{
    args::{assert_num, assert_string, get_args},
    control::eval_symbol,
    eval_form, eval_value,
    fun::none,
    EnvPtr, EvalResult, Value,
};

// COLLS (MANY CAN TAKE STRINGS AS WELL)
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
        "str" => eval_concat(list, env)?,
        "bit-seq" => {
            let eval = eval_value(&list[0], env)?;
            let Value::Number(n) = eval else {
                return Err(LankError::WrongType("Bit-seq".to_owned()));
            };
            Value::BitSeq(n as u16)
        }
        _ => unreachable!(),
    };
    Ok(coll)
}

pub fn eval_range(list: &[Value], env: &mut EnvPtr) -> EvalResult {
    let list = list
        .iter()
        .map(|v| eval_value(v, env))
        .collect::<IterResult>()?;

    let nums: Vec<Value> = match &list[..] {
        [Value::Number(end)] => (0..*end as u64).map(|n| Value::Number(n as f64)).collect(),
        [Value::Number(start), Value::Number(end)] => (*start as u64..*end as u64)
            .map(|n| Value::Number(n as f64))
            .collect(),
        [Value::Number(start), Value::Number(end), Value::Number(step)] => (*start as u64
            ..*end as u64)
            .step_by(*step as usize)
            .map(|n| Value::Number(n as f64))
            .collect(),
        _ => return Err(LankError::NumArguments("range".to_owned(), 1)),
    };

    Ok(Value::from(nums))
}

pub fn eval_count(list: &[Value], env: &mut EnvPtr) -> EvalResult {
    let coll = eval_value(&list[0], env)?;

    let len = match coll {
        Value::Form(Form::Unquoted(f)) => f.len(),
        Value::Vec(vec) => vec.len(),
        Value::String(s) => s.len(),
        _ => return Err(LankError::WrongType("Count".to_owned())),
    };

    Ok(Value::Number(len as f64))
}

pub fn eval_nth(list: &[Value], env: &mut EnvPtr) -> EvalResult {
    let args = &list
        .iter()
        .map(|v| eval_value(v, env))
        .collect::<IterResult>()?;

    let [coll, idx] = get_args::<2>(args, LankError::NumArguments("nth".to_owned(), 2))?;

    let Value::Number(idx) = idx else {
        return Err(LankError::NotANumber);
    };

    let res = match coll {
        Value::Form(Form::Unquoted(vals)) => {
            if let Some(elem) = vals.get(idx as usize) {
                elem.clone()
            } else {
                Value::None
            }
        }
        Value::Vec(vals) => {
            if let Some(elem) = vals.get(idx as usize) {
                elem.clone()
            } else {
                Value::None
            }
        }
        Value::String(s) => {
            if let Some(char) = s.chars().nth(idx as usize) {
                Value::Char(char)
            } else {
                Value::None
            }
        }
        _ => return Err(LankError::NotANumber),
    };

    Ok(res)
}

pub fn rand_nth(list: &[Value], env: &mut EnvPtr) -> EvalResult {
    let coll = eval_value(&list[0], env)?;

    let elem = match coll {
        Value::Form(Form::Unquoted(vals)) => vals
            .choose(&mut thread_rng())
            .ok_or_else(|| LankError::NoChildren)?
            .clone(),
        Value::Vec(vec) => {
            let vec = Vec::from_iter(vec.iter().cloned());
            vec.choose(&mut thread_rng())
                .ok_or_else(|| LankError::NoChildren)?
                .clone()
        }
        Value::String(s) => {
            let res = s
                .as_bytes()
                .choose(&mut thread_rng())
                .ok_or_else(|| LankError::NoChildren)?;
            Value::Char(char::from(*res))
        }
        _ => return Err(LankError::WrongType("Nth".to_owned())),
    };

    Ok(elem.clone())
}

pub fn eval_shuffle(list: &[Value], env: &mut EnvPtr) -> EvalResult {
    let arg = eval_value(&list[0], env)?;
    let mut rng = thread_rng();
    let coll = match arg {
        Value::Vec(vals) => {
            let mut shuffled = Vec::from_iter(vals.iter().cloned());
            shuffled.shuffle(&mut rng);
            Value::from(VecDeque::from(shuffled))
        }
        Value::Form(Form::Unquoted(vals)) => {
            let mut shuffled = vals.iter().cloned().collect::<Vec<Value>>();
            shuffled.shuffle(&mut rng);
            Value::from(shuffled)
        }
        Value::String(s) => {
            let mut shuffled = s.chars().collect::<Vec<char>>();
            shuffled.shuffle(&mut rng);
            Value::from(shuffled.iter().collect::<String>())
        }
        _ => return Err(LankError::WrongType("Shuffle".to_owned())),
    };

    Ok(coll)
}

pub fn eval_sort(list: &[Value], env: &mut EnvPtr) -> EvalResult {
    let arg = eval_value(&list[0], env)?;

    match arg {
        Value::Vec(vals) => {
            let mut copy = Vec::from_iter(vals.iter().cloned());
            copy.sort();

            Ok(Value::from(VecDeque::from(copy)))
        }
        Value::Form(Form::Unquoted(vals)) => {
            let mut copy = vals.iter().cloned().collect::<Vec<Value>>();
            copy.sort();
            Ok(Value::from(copy))
        }
        Value::String(chars) => {
            let mut copy = chars.as_bytes().to_vec();
            copy.sort();

            Ok(Value::from(
                String::from_utf8(copy.to_vec()).map_err(|err| err.to_string())?,
            ))
        }
        _ => Err(LankError::WrongType("Sort".to_owned())),
    }
}

pub fn eval_first(list: &[Value], env: &mut EnvPtr) -> EvalResult {
    let coll = eval_value(&list[0], env)?;

    let res = match coll {
        Value::Form(Form::Unquoted(vals)) => {
            if let Some(val) = vals.first() {
                val.clone()
            } else {
                Value::None
            }
        }
        Value::Vec(vector) => {
            if let Some(val) = vector.front() {
                val.clone()
            } else {
                Value::None
            }
        }
        Value::String(s) => {
            if let Some(c) = s.chars().next() {
                Value::Char(c)
            } else {
                Value::None
            }
        }
        _ => return Err(LankError::WrongType("First".to_owned())),
    };

    Ok(res)
}

pub fn eval_second(list: &[Value], env: &mut EnvPtr) -> EvalResult {
    let coll = eval_value(&list[0], env)?;

    let res = match coll {
        Value::Form(Form::Unquoted(vals)) => {
            if let Some(val) = vals.get(1) {
                val.clone()
            } else {
                Value::None
            }
        }
        Value::Vec(vector) => {
            if let Some(val) = vector.get(1) {
                val.clone()
            } else {
                Value::None
            }
        }
        Value::String(s) => {
            if let Some(c) = s.chars().nth(1) {
                Value::Char(c)
            } else {
                Value::None
            }
        }
        _ => return Err(LankError::WrongType("Second".to_owned())),
    };

    Ok(res)
}

pub fn eval_last(list: &[Value], env: &mut EnvPtr) -> EvalResult {
    let coll = eval_value(&list[0], env)?;

    let res = match coll {
        Value::Form(Form::Unquoted(vals)) => {
            if let Some(val) = vals.last() {
                val.clone()
            } else {
                Value::None
            }
        }
        Value::Vec(vector) => {
            if let Some(val) = vector.back() {
                val.clone()
            } else {
                Value::None
            }
        }
        Value::String(s) => {
            if let Some(c) = s.chars().last() {
                Value::Char(c)
            } else {
                Value::None
            }
        }
        _ => return Err(LankError::WrongType("Last".to_owned())),
    };

    Ok(res)
}

pub fn eval_rest(list: &[Value], env: &mut EnvPtr) -> EvalResult {
    let not_coll = Err(LankError::WrongType("Rest".to_owned()));

    let coll = eval_value(&list[0], env)?;

    let res = match coll {
        Value::Form(Form::Unquoted(vals)) => Value::from(vals[1..].to_vec()),
        Value::Vec(vector) => Value::Vec(Rc::new(
            vector.iter().skip(1).cloned().collect::<VecDeque<Value>>(),
        )),
        Value::String(s) => Value::from(s.chars().skip(1).collect::<String>()),
        _ => return not_coll,
    };

    Ok(res)
}

pub fn eval_prepend(list: &[Value], env: &mut EnvPtr) -> EvalResult {
    let not_coll = Err(LankError::WrongType("Prepend".to_owned()));

    let args = list
        .iter()
        .map(|v| eval_value(v, env))
        .collect::<Result<Vec<Value>, LankError>>()?;

    let [coll, val] = get_args::<2>(&args, LankError::NumArguments("prepend".to_owned(), 2))?;

    let res: Value = match val {
        Value::Vec(other) => {
            if let Value::Vec(this) = coll {
                Value::Vec(Rc::new(other.iter().chain(this.iter()).cloned().collect()))
            } else {
                return not_coll;
            }
        }

        Value::Form(Form::Unquoted(other)) => {
            if let Value::Form(Form::Unquoted(this)) = coll {
                Value::from(
                    other
                        .iter()
                        .chain(this.iter())
                        .cloned()
                        .collect::<Vec<Value>>(),
                )
            } else {
                return not_coll;
            }
        }

        Value::String(other) => {
            if let Value::String(this) = coll {
                Value::from(other.to_string() + &this)
            } else {
                return not_coll;
            }
        }

        Value::Char(c) => {
            if let Value::String(this) = coll {
                Value::from(c.to_string() + &this)
            } else {
                return not_coll;
            }
        }

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

    let args = list
        .iter()
        .map(|v| eval_value(v, env))
        .collect::<Result<Vec<Value>, LankError>>()?;

    let [coll, val] = get_args::<2>(&args, LankError::NumArguments("prepend".to_owned(), 2))?;

    let res: Value = match val {
        Value::Vec(other) => {
            if let Value::Vec(this) = coll {
                Value::Vec(Rc::new(this.iter().chain(other.iter()).cloned().collect()))
            } else {
                return not_coll;
            }
        }

        Value::Form(Form::Unquoted(other)) => {
            if let Value::Form(Form::Unquoted(this)) = coll {
                Value::from(
                    this.iter()
                        .chain(other.iter())
                        .cloned()
                        .collect::<Vec<Value>>(),
                )
            } else {
                return not_coll;
            }
        }

        Value::String(other) => {
            if let Value::String(this) = coll {
                Value::from(this.to_string() + &other)
            } else if let Value::Char(this) = coll {
                Value::from(this.to_string() + &other)
            } else {
                return not_coll;
            }
        }

        Value::Char(other) => {
            if let Value::String(this) = coll {
                Value::from(format!("{this}{other}"))
            } else if let Value::Char(this) = coll {
                Value::from(format!("{this}{other}"))
            } else {
                return not_coll;
            }
        }

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

pub fn eval_reverse(list: &[Value], env: &mut EnvPtr) -> EvalResult {
    let coll = eval_value(&list[0], env)?;

    let res = match coll {
        Value::Form(Form::Unquoted(f)) => {
            Value::from(f.iter().cloned().rev().collect::<Vec<Value>>())
        }
        Value::Vec(v) => Value::from(v.iter().cloned().rev().collect::<VecDeque<Value>>()),
        Value::String(s) => Value::from(s.chars().rev().collect::<String>()),
        _ => return Err(LankError::WrongType("Reverse".to_owned())),
    };

    Ok(res)
}

pub fn eval_map(list: &[Value], env: &mut EnvPtr) -> EvalResult {
    let not_coll = Err(LankError::WrongType("Map".to_owned()));

    let [fun, coll] = get_args::<2>(list, LankError::NumArguments("map".to_owned(), 2))?;

    let coll = eval_value(&coll, env)?;

    let fun = match fun {
        Value::Fun(_, _) | Value::Symbol(_) => fun.clone(),
        Value::Form(Form::Unquoted(vals)) => eval_form(&vals, env)?,

        _ => return not_coll,
    };

    match coll {
        Value::Vec(vector) => {
            let res = vector
                .iter()
                .map(|v| eval_form(&[fun.clone(), v.clone()], env))
                .collect::<IterResult>()?;
            Ok(Value::Vec(Rc::new(res.into())))
        }
        Value::Form(Form::Unquoted(vals)) => {
            let res = vals
                .iter()
                .map(|v| eval_form(&[fun.clone(), v.clone()], env))
                .collect::<IterResult>()?;
            Ok(Value::from(res))
        }

        Value::Map(map) => {
            let res = map
                .iter()
                .map(|(k, v)| eval_form(&[fun.clone(), k.clone(), v.clone()], env))
                .collect::<IterResult>()?;
            Ok(Value::from(VecDeque::from(res)))
        }

        Value::String(s) => {
            let res = s
                .chars()
                .map(|v| eval_form(&[fun.clone(), Value::Char(v)], env))
                .collect::<IterResult>()?;
            Ok(Value::from(res))
        }
        _ => not_coll,
    }
}

pub fn eval_map_indexed(list: &[Value], env: &mut EnvPtr) -> EvalResult {
    let not_coll = Err(LankError::WrongType("Map".to_owned()));

    let [fun, coll] = get_args::<2>(list, LankError::NumArguments("map-indexed".to_string(), 2))?;

    let coll = eval_value(&coll, env)?;

    let fun = match fun {
        Value::Fun(_, _) | Value::Symbol(_) => fun.clone(),
        Value::Form(Form::Unquoted(vals)) => eval_form(&vals, env)?,

        _ => return not_coll,
    };

    match coll {
        Value::Vec(vector) => {
            let res = vector
                .iter()
                .enumerate()
                .map(|(idx, item)| eval_form(&[fun.clone(), Value::from(idx), item.clone()], env))
                .collect::<IterResult>()?;
            Ok(Value::Vec(Rc::new(res.into())))
        }

        Value::Form(Form::Unquoted(vals)) => {
            let res = vals
                .iter()
                .enumerate()
                .map(|(idx, item)| eval_form(&[fun.clone(), Value::from(idx), item.clone()], env))
                .collect::<IterResult>()?;
            Ok(Value::from(res))
        }
        Value::Map(map) => {
            let res = map
                .iter()
                .enumerate()
                .map(|(idx, (k, v))| {
                    eval_form(&[fun.clone(), Value::from(idx), k.clone(), v.clone()], env)
                })
                .collect::<IterResult>()?;
            Ok(Value::from(VecDeque::from(res)))
        }
        Value::String(s) => {
            let res = s
                .chars()
                .enumerate()
                .map(|(idx, item)| {
                    eval_form(&[fun.clone(), Value::from(idx), Value::from(item)], env)
                })
                .collect::<IterResult>()?;
            Ok(Value::from(res))
        }
        _ => not_coll,
    }
}

pub fn eval_reduce(list: &[Value], env: &mut EnvPtr) -> EvalResult {
    let not_coll = Err(LankError::WrongType("Reduce".to_owned()));

    let [fun, coll] = get_args::<2>(list, LankError::NumArguments("reduce".to_owned(), 2))?;

    let coll = eval_value(&coll, env)?;

    let fun = match fun {
        Value::Fun(_, _) | Value::Symbol(_) => fun.clone(),
        Value::Form(Form::Unquoted(vals)) => eval_form(&vals, env)?,
        _ => return not_coll,
    };

    match coll {
        Value::Vec(vector) => {
            let mut it = vector.iter().cloned();
            try_fold_val(&mut it, &fun, env)
        }
        Value::Form(Form::Unquoted(vals)) => {
            let mut it = vals.iter().cloned();
            try_fold_val(&mut it, &fun, env)
        }
        Value::String(s) => {
            let mut it = s.chars().map(Value::Char);
            try_fold_val(&mut it, &fun, env)
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

        accum = res?;
    }

    Ok(accum)
}

pub fn eval_apply(list: &[Value], env: &mut EnvPtr) -> EvalResult {
    let not_coll = Err(LankError::WrongType("Apply".to_owned()));

    let [op, try_coll] = get_args::<2>(list, LankError::NumArguments("apply".to_owned(), 2))?;

    let mut coll = match try_coll {
        Value::Symbol(s) => {
            let res = eval_symbol(&s, env)?;
            match res {
                Value::Vec(v) => v.iter().cloned().collect::<Vec<Value>>(),
                Value::Form(Form::Unquoted(vals)) => vals.to_vec(),
                Value::String(s) => s.chars().map(Value::Char).collect::<Vec<Value>>(),
                _ => return not_coll,
            }
        }

        Value::Vec(v) => v.iter().cloned().collect::<Vec<Value>>(),
        Value::String(s) => s.chars().map(Value::Char).collect::<Vec<Value>>(),

        Value::Form(Form::Unquoted(vals)) => match eval_form(&vals, env)? {
            Value::Form(Form::Unquoted(vals)) => vals.to_vec(),
            Value::Vec(v) => v.iter().cloned().collect::<Vec<Value>>(),
            Value::String(s) => s.chars().map(Value::Char).collect::<Vec<Value>>(),
            _ => return not_coll,
        },

        _ => return not_coll,
    };

    coll.insert(0, op.clone());
    eval_form(&coll, env)
}

pub fn eval_filter(list: &[Value], env: &mut EnvPtr) -> EvalResult {
    let not_coll = Err(LankError::WrongType("Map".to_owned()));

    let [fun, coll] = get_args::<2>(list, LankError::NumArguments("filter".to_owned(), 2))?;

    let coll = eval_value(&coll, env)?;

    let fun = match fun {
        Value::Fun(_, _) | Value::Symbol(_) => fun.clone(),
        Value::Form(Form::Unquoted(vals)) => eval_form(&vals, env)?,
        _ => return not_coll,
    };

    try_filter_coll(fun, coll, env)
}

fn try_filter_coll(fun: Value, coll: Value, env: &mut EnvPtr) -> EvalResult {
    match coll {
        Value::Vec(vec) => {
            let mut new_vec = VecDeque::new();
            for item in vec.iter() {
                if !none(&eval_form(&[fun.clone(), item.clone()], env)?) {
                    println!("Item {item} is Not none");
                    new_vec.push_back(item.clone())
                }
            }
            Ok(Value::from(new_vec))
        }
        Value::Form(Form::Unquoted(form)) => {
            let mut new_form = Vec::new();
            for item in form.iter() {
                if !none(&eval_form(&[fun.clone(), item.clone()], env)?) {
                    new_form.push(item.clone());
                }
            }
            Ok(Value::from(new_form))
        }
        Value::String(s) => {
            let mut new_str = String::new();
            for char in s.chars() {
                if !none(&eval_form(&[fun.clone(), Value::Char(char)], env)?) {
                    new_str.push(char);
                }
            }
            Ok(Value::from(new_str))
        }
        _ => Err(LankError::WrongType("Filter".to_owned())),
    }
}

pub fn eval_take(list: &[Value], env: &mut EnvPtr) -> EvalResult {
    let args = list
        .iter()
        .map(|v| eval_value(v, env))
        .collect::<IterResult>()?;

    let [num, coll] = get_args::<2>(&args, LankError::NumArguments("take".to_owned(), 2))?;
    let num = assert_num(&num, LankError::WrongType("take".to_owned()))?;

    let coll = match coll {
        Value::Vec(vals) => Value::from(
            vals.iter()
                .cloned()
                .take(num as usize)
                .collect::<VecDeque<Value>>(),
        ),
        Value::Form(Form::Unquoted(vals)) => Value::from(
            vals.iter()
                .cloned()
                .take(num as usize)
                .collect::<Vec<Value>>(),
        ),
        Value::String(s) => Value::from(s.chars().take(num as usize).collect::<String>()),
        _ => return Err(LankError::WrongType("Take".to_owned())),
    };

    Ok(coll)
}

pub fn eval_drop(list: &[Value], env: &mut EnvPtr) -> EvalResult {
    let args = list
        .iter()
        .map(|v| eval_value(v, env))
        .collect::<IterResult>()?;

    let [num, coll] = get_args::<2>(&args, LankError::NumArguments("take".to_owned(), 2))?;
    let num = assert_num(&num, LankError::WrongType("take".to_owned()))?;

    let coll = match coll {
        Value::Vec(vals) => Value::from(
            vals.iter()
                .cloned()
                .skip(num as usize)
                .collect::<VecDeque<Value>>(),
        ),
        Value::Form(Form::Unquoted(vals)) => Value::from(
            vals.iter()
                .cloned()
                .skip(num as usize)
                .collect::<Vec<Value>>(),
        ),
        Value::String(s) => Value::from(s.chars().skip(num as usize).collect::<String>()),
        _ => return Err(LankError::WrongType("Take".to_owned())),
    };

    Ok(coll)
}

// STRINGS

pub fn eval_concat(list: &[Value], env: &mut EnvPtr) -> EvalResult {
    let args = list
        .iter()
        .map(|v| eval_value(v, env))
        .collect::<IterResult>()?;

    let string = args
        .iter()
        .cloned()
        .map(String::from)
        .reduce(|a, b| a + &b)
        .ok_or_else(|| LankError::SyntaxError);

    Ok(Value::from(string?))
}

pub fn eval_format(list: &[Value], env: &mut EnvPtr) -> EvalResult {
    let args = list
        .iter()
        .map(|v| eval_value(v, env))
        .collect::<IterResult>()?;

    let string = args
        .first()
        .ok_or_else(|| LankError::WrongType("Format".to_owned()))?;
    let string = assert_string(string, LankError::WrongType("Format".to_owned()))?;

    let res = String::from_iter(
        string
            .split("{}")
            .zip(args[1..].iter())
            .map(|(str, val)| str.to_owned() + &String::from(val.clone())),
    );

    Ok(Value::from(res))
}

pub fn eval_bytes(list: &[Value], env: &mut EnvPtr) -> EvalResult {
    let str = list.get(0).ok_or_else(|| LankError::NoChildren)?;

    let chars = match str {
        Value::Symbol(_) => match eval_value(str, env)? {
            Value::String(s) => s,
            Value::Char(c) => Rc::from(c.to_string()),
            _ => return Err(LankError::WrongType("Bytes".to_owned())),
        },
        Value::String(s) => s.clone(),
        Value::Char(c) => Rc::from(c.to_string()),
        _ => return Err(LankError::WrongType("Bytes".to_owned())),
    };

    if chars.len() == 1 {
        Ok(Value::Number(chars.as_bytes()[0] as f64))
    } else {
        Ok(Value::from(
            chars
                .as_bytes()
                .iter()
                .map(|b| Value::Number(*b as f64))
                .collect::<VecDeque<Value>>(),
        ))
    }
}
