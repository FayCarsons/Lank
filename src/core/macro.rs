use std::rc::Rc;

use crate::utils::{
    error::LankError,
    value::{Args, Form},
};

use super::{EnvPtr, EvalResult, Value};

// Fucked!! rewrite!
pub fn eval_replace(list: Args, _env: &mut EnvPtr) -> EvalResult {
    let [coll, symbol, replacement] = list else {
        return Err(LankError::NumArguments("Replace".to_owned(), 3));
    };

    println!("coll: {coll} symbol: {symbol} replacement: {replacement}");

    match coll {
        Value::Form(Form::Quoted(form)) => {
            let mut new_form = form.iter().cloned().collect::<Vec<Value>>();
            if let Some(idx) = new_form.iter().position(|token| token == *symbol) {
                let _ = std::mem::replace(&mut new_form[idx], (*replacement).clone());
                Ok(Value::Form(Form::Unquoted(Rc::new(new_form))))
            } else {
                Ok(Value::None)
            }
        }
        _ => Ok(Value::None),
    }
}
