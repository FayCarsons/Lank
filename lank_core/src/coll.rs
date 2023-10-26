use std::{collections::VecDeque, rc::Rc};

use rand::{seq::SliceRandom, thread_rng};

use model::{
    error::{IterResult, LankError},
    value::{Args, Form},
};

use super::{
    args::{assert_num, assert_string, eval_args, get_args, assert_bitseq, assert_symbol},
    eval_form, eval_value,
    EnvPtr, EvalResult, Value,
};

// Bit-seq
pub fn eval_bit_op(list: Args, env: &mut EnvPtr) -> EvalResult {
    let op = list.first().ok_or_else(|| LankError::SyntaxError)?;
    let op = assert_symbol(op, LankError::SyntaxError)?;
    let [bitseq, idx] = get_args::<2>(
        &list[1..],
        env,
        LankError::NumArguments("Bit operation".to_owned(), 2),
    )?;

    let bitseq = assert_bitseq(&bitseq, LankError::WrongType("Bit-set".to_owned()))?;
    let idx = assert_num(&idx, LankError::WrongType("Bit-set".to_owned()))?;

    match &*op {
        "bit-set" => Ok(Value::BitSeq(bitseq | (1 << (15 - (idx as usize))))),
        "bit-get" => {
            let res = (bitseq >> (15 - (idx as usize))) & 1 == 1;
            Ok(Value::Number(if res { 1. } else { 0. }))
        }
        "bit-tog" => Ok(Value::BitSeq(bitseq ^ (1 << (15 - (idx as usize))))),
        "bit-clear" => Ok(Value::BitSeq(bitseq & !(1 << (15 - (idx as usize))))),
        "count-ones" => Ok(Value::Number(bitseq.count_ones() as f64)),
        _ => Err(LankError::UnknownFunction(op.to_string())),
    }
}

// COLLS (MANY CAN TAKE STRINGS AS WELL)
pub fn make_coll(coll_type: &str, list: Args, env: &mut EnvPtr) -> EvalResult {
    let coll = match coll_type {
        "vec" => Value::from(
            list.iter()
                .map(|v| eval_value(v, env))
                .collect::<Result<VecDeque<Value>, LankError>>()?,
        ),
        "list" => Value::from(eval_args(list, env)?),
        "str" => eval_concat(list, env)?,
        "bit-seq" => {
            let arg = list
                .first()
                .ok_or_else(|| LankError::NumArguments("Bit-seq".to_owned(), 1))?;
            let eval = eval_value(arg, env)?;
            let Value::Number(n) = eval else {
                return Err(LankError::WrongType("Bit-seq".to_owned()));
            };
            Value::BitSeq(n as u16)
        }
        _ => unreachable!(),
    };
    Ok(coll)
}

pub fn eval_range(list: Args, env: &mut EnvPtr) -> EvalResult {
    let args = eval_args(list, env)?;

    let nums: Vec<Value> = match &args[..] {
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

pub fn eval_count(list: Args, env: &mut EnvPtr) -> EvalResult {
    let arg = list
        .first()
        .ok_or_else(|| LankError::NumArguments("Count".to_owned(), 1))?;
    let coll = eval_value(arg, env)?;

    let len = match coll {
        Value::Form(Form::Unquoted(f)) => f.len(),
        Value::Vec(vec) => vec.len(),
        Value::String(s) => s.len(),
        _ => return Err(LankError::WrongType("Count".to_owned())),
    };

    Ok(Value::Number(len as f64))
}

pub fn eval_nth(list: Args, env: &mut EnvPtr) -> EvalResult {
    let [coll, idx] = get_args::<2>(list, env, LankError::NumArguments("nth".to_owned(), 2))?;

    let idx = assert_num(&idx, LankError::WrongType("Nth".to_owned()))?;

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

pub fn rand_nth(list: Args, env: &mut EnvPtr) -> EvalResult {
    let arg = list
        .first()
        .ok_or_else(|| LankError::NumArguments("Rand-nth".to_owned(), 1))?;
    let coll = eval_value(arg, env)?;

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

pub fn eval_shuffle(list: Args, env: &mut EnvPtr) -> EvalResult {
    let arg = list
        .first()
        .ok_or_else(|| LankError::NumArguments("Shuffle".to_owned(), 1))?;
    let coll = eval_value(arg, env)?;

    let mut rng = thread_rng();
    let coll = match coll {
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

pub fn eval_sort(list: Args, env: &mut EnvPtr) -> EvalResult {
    let arg = list
        .first()
        .ok_or_else(|| LankError::NumArguments("Sort".to_owned(), 1))?;
    let coll = eval_value(arg, env)?;

    match coll {
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

pub fn eval_first(list: Args, env: &mut EnvPtr) -> EvalResult {
    let arg = list
        .first()
        .ok_or_else(|| LankError::NumArguments("First".to_owned(), 1))?;
    let coll = eval_value(arg, env)?;

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

pub fn eval_second(list: Args, env: &mut EnvPtr) -> EvalResult {
    let arg = list
        .first()
        .ok_or_else(|| LankError::NumArguments("Second".to_owned(), 1))?;
    let coll = eval_value(arg, env)?;

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

pub fn eval_last(list: Args, env: &mut EnvPtr) -> EvalResult {
    let arg = list
        .first()
        .ok_or_else(|| LankError::NumArguments("Last".to_owned(), 1))?;
    let coll = eval_value(arg, env)?;

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

pub fn eval_rest(list: Args, env: &mut EnvPtr) -> EvalResult {
    let not_coll = Err(LankError::WrongType("Rest".to_owned()));

    let arg = list
        .first()
        .ok_or_else(|| LankError::NumArguments("Rest".to_owned(), 1))?;
    let coll = eval_value(arg, env)?;

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

pub fn eval_prepend(list: Args, env: &mut EnvPtr) -> EvalResult {
    let not_coll = Err(LankError::WrongType("Prepend".to_owned()));

    let [coll, val] = get_args::<2>(list, env, LankError::NumArguments("prepend".to_owned(), 2))?;

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

pub fn eval_append(list: Args, env: &mut EnvPtr) -> EvalResult {
    let not_coll = Err(LankError::WrongType("Append".to_owned()));

    let [coll, val] = get_args::<2>(list, env, LankError::NumArguments("prepend".to_owned(), 2))?;

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

pub fn eval_reverse(list: Args, env: &mut EnvPtr) -> EvalResult {
    let arg = list
        .first()
        .ok_or_else(|| LankError::NumArguments("Reverse".to_owned(), 1))?;
    let coll = eval_value(arg, env)?;

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

pub fn eval_map(list: Args, env: &mut EnvPtr) -> EvalResult {
    let not_coll = Err(LankError::WrongType("Map".to_owned()));

    let [fun, coll] = get_args::<2>(list, env, LankError::NumArguments("map".to_owned(), 2))?;

    match coll {
        Value::Vec(vector) => {
            let res = vector
                .iter()
                .map(|v| eval_form(&[&fun, v], env))
                .collect::<IterResult>()?;
            Ok(Value::Vec(Rc::new(res.into())))
        }
        Value::Form(Form::Unquoted(vals)) => {
            let res = vals
                .iter()
                .map(|v| eval_form(&[&fun, v], env))
                .collect::<IterResult>()?;
            Ok(Value::from(res))
        }

        Value::Map(map) => {
            let res = map
                .iter()
                .map(|(k, v)| eval_form(&[&fun, k, v], env))
                .collect::<IterResult>()?;
            Ok(Value::from(VecDeque::from(res)))
        }

        Value::String(s) => {
            let res = s
                .chars()
                .map(|v| eval_form(&[&fun, &Value::Char(v)], env))
                .collect::<IterResult>()?;
            Ok(Value::from(res))
        }
        _ => not_coll,
    }
}

pub fn eval_map_indexed(list: Args, env: &mut EnvPtr) -> EvalResult {
    let not_coll = Err(LankError::WrongType("Map".to_owned()));

    let [fun, coll] = get_args::<2>(
        list,
        env,
        LankError::NumArguments("map-indexed".to_string(), 2),
    )?;

    match coll {
        Value::Vec(vector) => {
            let res = vector
                .iter()
                .enumerate()
                .map(|(idx, item)| eval_form(&[&fun, &Value::from(idx), item], env))
                .collect::<IterResult>()?;
            Ok(Value::Vec(Rc::new(res.into())))
        }

        Value::Form(Form::Unquoted(vals)) => {
            let res = vals
                .iter()
                .enumerate()
                .map(|(idx, item)| eval_form(&[&fun, &Value::from(idx), item], env))
                .collect::<IterResult>()?;
            Ok(Value::from(res))
        }
        Value::Map(map) => {
            let res = map
                .iter()
                .enumerate()
                .map(|(idx, (k, v))| eval_form(&[&fun, &Value::from(idx), k, v], env))
                .collect::<IterResult>()?;
            Ok(Value::from(VecDeque::from(res)))
        }
        Value::String(s) => {
            let res = s
                .chars()
                .enumerate()
                .map(|(idx, item)| eval_form(&[&fun, &Value::from(idx), &Value::from(item)], env))
                .collect::<IterResult>()?;
            Ok(Value::from(res))
        }
        _ => not_coll,
    }
}

pub fn eval_reduce(list: Args, env: &mut EnvPtr) -> EvalResult {
    let not_coll = Err(LankError::WrongType("Reduce".to_owned()));

    let [fun, coll] = get_args::<2>(list, env, LankError::NumArguments("reduce".to_owned(), 2))?;

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
        let res = eval_form(&[&fun, &x, &accum], env);

        accum = res?;
    }

    Ok(accum)
}

pub fn eval_apply(list: Args, env: &mut EnvPtr) -> EvalResult {
    let not_coll = Err(LankError::WrongType("Apply".to_owned()));

    let [op, coll] = get_args(list, env, LankError::NumArguments("apply".to_owned(), 2))?;

    let mut coll = match coll {
        Value::Vec(v) => v.iter().cloned().collect::<Vec<Value>>(),
        Value::String(s) => s.chars().map(Value::Char).collect::<Vec<Value>>(),

        Value::Form(Form::Unquoted(vals)) => {
            match eval_form(&vals.iter().collect::<Vec<&Value>>(), env)? {
                Value::Form(Form::Unquoted(vals)) => vals.to_vec(),
                Value::Vec(v) => v.iter().cloned().collect::<Vec<Value>>(),
                Value::String(s) => s.chars().map(Value::Char).collect::<Vec<Value>>(),
                _ => return not_coll,
            }
        }

        _ => return not_coll,
    };

    coll.insert(0, op.clone());
    eval_form(&coll.iter().collect::<Vec<&Value>>(), env)
}

pub fn eval_filter(list: Args, env: &mut EnvPtr) -> EvalResult {
    let [fun, coll] = get_args::<2>(list, env, LankError::NumArguments("filter".to_owned(), 2))?;

    match coll {
        Value::Vec(vec) => {
            let mut new_vec = VecDeque::new();
            for item in vec.iter() {
                if eval_form(&[&fun, item], env)?.is_some() {
                    println!("Item {item} is Not none");
                    new_vec.push_back(item.clone())
                }
            }
            Ok(Value::from(new_vec))
        }
        Value::Form(Form::Unquoted(form)) => {
            let mut new_form = Vec::new();
            for item in form.iter() {
                if eval_form(&[&fun, item], env)?.is_some() {
                    new_form.push(item.clone());
                }
            }
            Ok(Value::from(new_form))
        }
        Value::String(s) => {
            let mut new_str = String::new();
            for char in s.chars() {
                if eval_form(&[&fun, &Value::Char(char)], env)?.is_some() {
                    new_str.push(char);
                }
            }
            Ok(Value::from(new_str))
        }
        _ => Err(LankError::WrongType("Filter".to_owned())),
    }
}

pub fn eval_take(list: Args, env: &mut EnvPtr) -> EvalResult {
    let [num, coll] = get_args::<2>(list, env, LankError::NumArguments("take".to_owned(), 2))?;

    let num = assert_num(&num, LankError::WrongType("take".to_owned()))?;

    let coll = match coll {
        Value::Vec(vals) => Value::from(
            vals.iter()
                .take(num as usize)
                .cloned()
                .collect::<VecDeque<Value>>(),
        ),
        Value::Form(Form::Unquoted(vals)) => Value::from(
            vals.iter()
                .take(num as usize)
                .cloned()
                .collect::<Vec<Value>>(),
        ),
        Value::String(s) => Value::from(s.chars().take(num as usize).collect::<String>()),
        _ => return Err(LankError::WrongType("Take".to_owned())),
    };

    Ok(coll)
}

pub fn eval_drop(list: Args, env: &mut EnvPtr) -> EvalResult {
    let [num, coll] = get_args::<2>(list, env, LankError::NumArguments("take".to_owned(), 2))?;
    let num = assert_num(&num, LankError::WrongType("take".to_owned()))?;

    let coll = match coll {
        Value::Vec(vals) => Value::from(
            vals.iter()
                .skip(num as usize)
                .cloned()
                .collect::<VecDeque<Value>>(),
        ),
        Value::Form(Form::Unquoted(vals)) => Value::from(
            vals.iter()
                .skip(num as usize)
                .cloned()
                .collect::<Vec<Value>>(),
        ),
        Value::String(s) => Value::from(s.chars().skip(num as usize).collect::<String>()),
        _ => return Err(LankError::WrongType("Take".to_owned())),
    };

    Ok(coll)
}

// STRINGS

pub fn eval_concat(list: Args, env: &mut EnvPtr) -> EvalResult {
    let args = eval_args(list, env)?;

    let string = args
        .iter()
        .map(String::from)
        .reduce(|a, b| a + &b)
        .ok_or_else(|| LankError::SyntaxError);

    Ok(Value::from(string?))
}

pub fn eval_format(list: Args, env: &mut EnvPtr) -> EvalResult {
    let args = eval_args(list, env)?;

    let string = args
        .first()
        .ok_or_else(|| LankError::WrongType("Format".to_owned()))?;
    let string = assert_string(string, LankError::WrongType("Format".to_owned()))?;

    let res = String::from_iter(
        string
            .split("{}")
            .zip(args[1..].iter())
            .map(|(str, val)| format!("{str}{}", val)),
    );

    Ok(Value::from(res))
}

pub fn eval_bytes(list: Args, env: &mut EnvPtr) -> EvalResult {
    let str = list.first().ok_or_else(|| LankError::NoChildren)?;

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
