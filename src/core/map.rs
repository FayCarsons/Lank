use std::collections::{HashMap, VecDeque};

use crate::utils::{error::LankError, value::Args};

use super::{
    args::{assert_map, assert_vec, get_args},
    eval_form, eval_value, EnvPtr, EvalResult, Value,
};

pub fn make_map(list: Args, env: &mut EnvPtr) -> EvalResult {
    let arg = list
        .first()
        .ok_or_else(|| LankError::NumArguments("Map".to_owned(), 1))?;
    let arg = eval_value(arg, env)?;

    let pairs = assert_vec(&arg, LankError::WrongType("Hashmap".to_owned()))?;

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

pub fn eval_get(list: Args, env: &mut EnvPtr) -> EvalResult {
    let [map, key] = get_args::<2>(list, env, LankError::NumArguments("Get".to_owned(), 2))?;

    let map = assert_map(&map, &LankError::WrongType("Get".to_owned()))?;
    let res = map.get(&key).unwrap_or(&Value::None);

    Ok(res.clone())
}

pub fn eval_update(list: Args, env: &mut EnvPtr) -> EvalResult {
    let [map, key, fun] =
        get_args::<3>(list, env, LankError::NumArguments("Update".to_owned(), 3))?;

    let map = assert_map(&map, &LankError::WrongType("Update".to_owned()))?;
    let val = map.get(&key).unwrap_or(&Value::None);

    let expr_res = eval_form(&[&fun, val], env)?;

    let new_map = map
        .into_iter()
        .chain([(key.clone(), expr_res)])
        .collect::<HashMap<Value, Value>>();
    Ok(Value::from(new_map))
}

pub fn eval_keys(list: Args, env: &mut EnvPtr) -> EvalResult {
    let arg = list
        .first()
        .ok_or_else(|| LankError::NumArguments("Keys".to_owned(), 1))?;
    let arg = eval_value(arg, env)?;

    let map = assert_map(&arg, &LankError::WrongType("Keys".to_owned()))?;

    Ok(Value::from(
        map.keys().cloned().collect::<VecDeque<Value>>(),
    ))
}

pub fn eval_vals(list: Args, env: &mut EnvPtr) -> EvalResult {
    let arg = list
        .first()
        .ok_or_else(|| LankError::NumArguments("Keys".to_owned(), 1))?;
    let arg = eval_value(arg, env)?;

    let map = assert_map(&arg, &LankError::WrongType("Keys".to_owned()))?;

    Ok(Value::from(
        map.values().cloned().collect::<VecDeque<Value>>(),
    ))
}

pub fn eval_merge(list: Args, env: &mut EnvPtr) -> EvalResult {
    let [map_one, map_two] =
        get_args::<2>(list, env, LankError::NumArguments("Get".to_owned(), 2))?;

    let error = LankError::WrongType("Merge".to_owned());

    let (map_one, map_two) = (assert_map(&map_one, &error)?, assert_map(&map_two, &error)?);
    let new_map = map_one
        .clone()
        .into_iter()
        .chain(*map_two)
        .collect::<HashMap<Value, Value>>();

    Ok(Value::from(new_map))
}

pub fn eval_zipmap(list: Args, env: &mut EnvPtr) -> EvalResult {
    let [keys, vals] = get_args(list, env, LankError::NumArguments("Zipmap".to_owned(), 2))?;
    let [keys, vals] = [
        assert_vec(&keys, LankError::SyntaxError)?,
        assert_vec(&vals, LankError::SyntaxError)?,
    ];

    let map = keys
        .iter()
        .cloned()
        .zip(vals.iter().cloned())
        .collect::<HashMap<Value, Value>>();
    Ok(Value::from(map))
}
