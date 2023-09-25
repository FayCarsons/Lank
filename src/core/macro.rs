use std::{rc::Rc, collections::{VecDeque, HashSet}};

use crate::utils::{error::LankError, value::Form};

use super::{EnvPtr, Value, EvalResult, IterResult};

pub fn eval_replace(list: &[Value], env: &mut EnvPtr) -> EvalResult {
    let [coll, symbol, replacement] = list else {
        return Err(LankError::NumArguments("Replace".to_owned(), 3))
    };

    println!("coll: {coll} symbol: {symbol} replacement: {replacement}");

    match coll {
        Value::Form(Form::Quoted(form)) => {
            let mut new_form = form.iter().cloned().collect::<Vec<Value>>();
            if let Some(idx) = new_form.iter().position(|token| if token == symbol {true} else {false}) {
                let _ = std::mem::replace(&mut new_form[idx], replacement.clone());
                Ok(Value::Form(Form::Unquoted(Rc::new(new_form))))
            } else {
                Ok(Value::Void)
            }
        }
        _ => Ok(Value::Void)
    }
}