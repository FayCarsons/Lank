use std::collections::{HashMap, VecDeque};

use crate::utils::error::{IterResult, LankError};

use super::{
    args::{assert_map, get_args},
    eval_form, eval_value, EnvPtr, EvalResult, Value,
};

pub fn make_map(list: &[Value], env: &mut EnvPtr) -> EvalResult {
    let arg = eval_value(&list[0], env)?;

    let Value::Vec(pairs) = arg else {
        return Err(LankError::WrongType("Hashmap".to_owned()));
    };

    let pairs = pairs
        .iter()
        .map(|vec| {
            let Value::Vec(pair) = vec else {
                return Err(LankError::WrongType("Hashmap".to_owned()));
            };

            let pair = Vec::from_iter(pair.iter().cloned());

            let (Some(k), Some(v)) = (pair.get(0), pair.get(1)) else {
                return Err(LankError::SyntaxError);
            };

            Ok((k.clone(), v.clone()))
        })
        .collect::<Result<HashMap<Value, Value>, LankError>>()?;

    Ok(Value::from(pairs))
}

pub fn eval_get(list: &[Value], env: &mut EnvPtr) -> EvalResult {
    let args = list
        .iter()
        .map(|v| eval_value(v, env))
        .collect::<IterResult>()?;
    let [map, key] = get_args::<2>(&args, LankError::NumArguments("Get".to_owned(), 2))?;

    let map = assert_map(&map, &LankError::WrongType("Get".to_owned()))?;
    let res = map.get(&key).unwrap_or(&Value::None);

    Ok(res.clone())
}

pub fn eval_update(list: &[Value], env: &mut EnvPtr) -> EvalResult {
    let args = list
        .iter()
        .map(|v| eval_value(v, env))
        .collect::<IterResult>()?;
    let [map, key, fun] = get_args::<3>(&args, LankError::NumArguments("Update".to_owned(), 3))?;

    let map = assert_map(&map, &LankError::WrongType("Update".to_owned()))?;
    let val = map.get(&key).unwrap_or(&Value::None);

    let expr_res = eval_form(&[fun.clone(), val.clone()], env)?;

    let new_map = map
        .into_iter()
        .chain([(key.clone(), expr_res)])
        .collect::<HashMap<Value, Value>>();
    Ok(Value::from(new_map))
}

pub fn eval_keys(list: &[Value], env: &mut EnvPtr) -> EvalResult {
    let arg = eval_value(&list[0], env)?;

    let map = assert_map(&arg, &LankError::WrongType("Keys".to_owned()))?;

    Ok(Value::from(
        map.keys().cloned().collect::<VecDeque<Value>>(),
    ))
}

pub fn eval_vals(list: &[Value], env: &mut EnvPtr) -> EvalResult {
    let arg = eval_value(&list[0], env)?;

    let map = assert_map(&arg, &LankError::WrongType("Keys".to_owned()))?;

    Ok(Value::from(
        map.values().cloned().collect::<VecDeque<Value>>(),
    ))
}

pub fn eval_merge(list: &[Value], env: &mut EnvPtr) -> EvalResult {
    let args = list[..2]
        .iter()
        .map(|v| eval_value(v, env))
        .collect::<IterResult>()?;
    let [map_one, map_two] = get_args::<2>(&args, LankError::NumArguments("Merge".to_owned(), 2))?;

    let error = LankError::WrongType("Merge".to_owned());
    let (map_one, map_two) = (assert_map(&map_one, &error)?, assert_map(&map_two, &error)?);
    let new_map = map_one
        .clone()
        .into_iter()
        .chain(map_two.clone().into_iter())
        .collect::<HashMap<Value, Value>>();

    Ok(Value::from(new_map))
}
