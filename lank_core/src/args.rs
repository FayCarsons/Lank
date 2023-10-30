use std::rc::Rc;

use model::{
    error::{IterResult, LankError},
    value::{Args, Form, Map, Seq, Vector},
};

use super::{eval_value, EnvPtr, Value};

pub fn get_args<const N: usize>(
    list: Args,
    env: &mut EnvPtr,
    error: LankError,
) -> Result<[Value; N], LankError> {
    if list.len() != N {
        Err(error)
    } else {
        Ok(std::array::try_from_fn(|i| eval_value(list[i], env))?)
    }
}

pub fn eval_args(args: Args, env: &mut EnvPtr) -> IterResult {
    args.iter()
        .map(|v| eval_value(v, env))
        .collect::<IterResult>()
}

pub fn assert_vec(maybe: &Value, error: LankError) -> Result<Vector, LankError> {
    if let Value::Vec(res) = maybe {
        Ok(res.clone())
    } else {
        Err(error)
    }
}

#[allow(unused)]
pub fn assert_form(maybe: &Value, error: LankError) -> Result<Seq, LankError> {
    if let Value::Form(Form::Unquoted(res)) = maybe {
        Ok(res.clone())
    } else {
        Err(error)
    }
}

pub fn assert_num(maybe: &Value, error: LankError) -> Result<f64, LankError> {
    if let Value::Number(res) = maybe {
        Ok(*res)
    } else {
        Err(error)
    }
}

#[allow(unused)]
pub fn assert_bool(maybe: &Value, error: LankError) -> Result<bool, LankError> {
    if let Value::Bool(res) = maybe {
        Ok(*res)
    } else {
        Err(error)
    }
}

pub fn assert_bitseq(maybe: &Value, error: LankError) -> Result<u16, LankError> {
    if let Value::BitSeq(res) = maybe {
        Ok(*res)
    } else {
        Err(error)
    }
}

pub fn assert_string(maybe: &Value, error: LankError) -> Result<Rc<str>, LankError> {
    if let Value::String(s) = maybe {
        Ok(s.clone())
    } else {
        Err(error)
    }
}

pub fn assert_symbol(maybe: &Value, error: LankError) -> Result<Rc<str>, LankError> {
    if let Value::Symbol(s) = maybe {
        Ok(s.clone())
    } else {
        Err(error)
    }
}

pub fn assert_map(maybe: &Value, error: &LankError) -> Result<Map, LankError> {
    if let Value::Map(m) = maybe {
        Ok(m.clone())
    } else {
        Err(error.clone())
    }
}

pub fn assert_fn(maybe: &Value, error: LankError) -> Result<(Rc<Vec<String>>, Form), LankError> {
    if let Value::Fun(params, body) = maybe {
        Ok((params.clone(), body.clone()))
    } else {
        Err(error)
    }
}

pub fn higher_order_fn_args(
    list: &[&Value],
    env: &mut EnvPtr,
    err: LankError,
) -> Result<(Value, Value), LankError> {
    match list {
        [fun, coll] => {
            let c = eval_value(coll, env)?;
            Ok(((*fun).clone(), c))
        }
        _ => Err(err),
    }
}
