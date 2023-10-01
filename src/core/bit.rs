use crate::utils::error::{IterResult, LankError};

use super::{
    args::{assert_bitseq, assert_num, get_args},
    eval_value, EnvPtr, EvalResult, Value,
};

pub fn eval_bit_op(list: &[Value], env: &mut EnvPtr) -> EvalResult {
    let [op, bitseq, idx] = get_args::<3>(
        &list
            .iter()
            .map(|v| eval_value(v, env))
            .collect::<IterResult>()?,
        LankError::NumArguments("Bit operation".to_owned(), 2),
    )?;

    let Value::Symbol(op) = op else {
        unreachable!()
    };

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
