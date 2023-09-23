use crate::utils::error::{IterResult, LankError};

use super::{eval_value, EnvPtr, EvalResult, Value};

pub fn set_bit(list: &[Value], env: &mut EnvPtr) -> EvalResult {
    let [bitseq, idx] = &list
        .iter()
        .map(|v| eval_value(v, env))
        .collect::<IterResult>()?[..]
    else {
        return Err(LankError::NumArguments("Bit-set".to_owned(), 2));
    };

    let Value::BitSeq(bitseq) = bitseq else {
        return Err(LankError::WrongType("Bit-set".to_owned()));
    };

    let Value::Number(idx) = idx else {
        return Err(LankError::WrongType("Bit-set".to_owned()));
    };

    Ok(Value::BitSeq(bitseq | (1 << (15 - (*idx as usize)))))
}

pub fn get_bit(list: &[Value], env: &mut EnvPtr) -> EvalResult {
    let [bitseq, idx] = &list
        .iter()
        .map(|v| eval_value(v, env))
        .collect::<IterResult>()?[..]
    else {
        return Err(LankError::NumArguments("Bit-set".to_owned(), 2));
    };

    let Value::BitSeq(bitseq) = bitseq else {
        return Err(LankError::WrongType("Bit-set".to_owned()));
    };

    let Value::Number(idx) = idx else {
        return Err(LankError::WrongType("Bit-set".to_owned()));
    };

    let res = (bitseq >> (15 - (*idx as usize))) & 1 == 1;
    Ok(Value::Number(if res { 1. } else { 0. }))
}

pub fn toggle_bit(list: &[Value], env: &mut EnvPtr) -> EvalResult {
    let [bitseq, idx] = &list
        .iter()
        .map(|v| eval_value(v, env))
        .collect::<IterResult>()?[..]
    else {
        return Err(LankError::NumArguments("Bit-set".to_owned(), 2));
    };

    let Value::BitSeq(bitseq) = bitseq else {
        return Err(LankError::WrongType("Bit-set".to_owned()));
    };

    let Value::Number(idx) = idx else {
        return Err(LankError::WrongType("Bit-set".to_owned()));
    };

    Ok(Value::BitSeq(bitseq ^ (1 << (15 - (*idx as usize)))))
}

pub fn clear_bit(list: &[Value], env: &mut EnvPtr) -> EvalResult {
    let [bitseq, idx] = &list
        .iter()
        .map(|v| eval_value(v, env))
        .collect::<IterResult>()?[..]
    else {
        return Err(LankError::NumArguments("Bit-set".to_owned(), 2));
    };

    let Value::BitSeq(bitseq) = bitseq else {
        return Err(LankError::WrongType("Bit-set".to_owned()));
    };

    let Value::Number(idx) = idx else {
        return Err(LankError::WrongType("Bit-set".to_owned()));
    };

    Ok(Value::BitSeq(bitseq & !(1 << (15 - (*idx as usize)))))
}

pub fn count_set(list: &[Value], env: &mut EnvPtr) -> EvalResult {
    let Value::BitSeq(bitseq) = eval_value(&list[0], env)? else {
        return Err(LankError::WrongType("Count-set".to_owned()));
    };

    Ok(Value::Number(bitseq.count_ones() as f64))
}
