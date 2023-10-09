use crate::utils::{error::LankError, value::Args};

use super::{
    args::{assert_bitseq, assert_num, assert_symbol, get_args},
    EnvPtr, EvalResult, Value,
};

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
