use std::rc::Rc;

use crate::utils::{
    error::LankError,
    value::{Form, Args, Map, Seq, Vector},
};

use super::Value;

pub fn get_args<const N: usize>(list: Args, error: LankError) -> Result<[Value; N], LankError> {
    if list.len() != N {
        Err(error)
    } else {
        Ok(std::array::from_fn(|i| list[i].clone()))
    }
}

pub fn assert_vec(maybe: &Value, error: LankError) -> Result<Vector, LankError> {
    if let Value::Vec(res) = maybe {
        Ok(res.clone())
    } else {
        Err(error)
    }
}

pub fn assert_form(maybe: Value, error: LankError) -> Result<Seq, LankError> {
    if let Value::Form(Form::Unquoted(res)) = maybe {
        Ok(res)
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

pub fn assert_bool(maybe: Value, error: LankError) -> Result<bool, LankError> {
    if let Value::Bool(res) = maybe {
        Ok(res)
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
        return Err(error);
    }
}
