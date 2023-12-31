#![feature(array_try_from_fn)]
pub use model::{
    env::{Env, EnvPtr},
    error::{EvalResult, IterResult, LankError},
    value::{Args, Form, Value},
};

pub mod args;
mod coll;
mod conditional;
mod control;
mod fun;
mod r#macro;
mod map;

use self::{args::*, coll::*, conditional::*, control::*, fun::*, map::*, r#macro::*};

use phf::{phf_set, Set};

pub static BINARY_OPS: Set<&str> = phf_set!(
    "+", "-", "*", "/", "mod", ">", ">=", "<", "<=", "!=", "==", "<<", ">>", "exp", "&", ",", "^",
);

pub const UNARY_OPS: Set<&str> = phf_set!("abs", "neg", "bit-flip", "not");
pub const BOOL_OPS: Set<&str> = phf_set!("xor", "eq", "or", "and");
pub const TYPE_CHECKS: Set<&str> = phf_set!(
    "char?", "number?", "coll?", "vec?", "list?", "string?", "symbol?", "bool?", "fn?", "map?"
);
pub const BIT_OPS: Set<&str> = phf_set!("bit-set", "bit-get", "bit-clear", "bit-tog", "count-ones");

pub fn is_builtin(value: &Value) -> Result<bool, LankError> {
    let value = &assert_symbol(
        value,
        LankError::WrongType("Expected symbol on lhs of form!".to_owned()),
    )?;
    Ok(BINARY_OPS.contains(value)
        || UNARY_OPS.contains(value)
        || BOOL_OPS.contains(value)
        || TYPE_CHECKS.contains(value)
        || BIT_OPS.contains(value))
}

pub fn eval_value(obj: &Value, env: &mut EnvPtr) -> EvalResult {
    match obj {
        Value::Symbol(s) => eval_symbol(s, env),
        Value::Form(Form::Unquoted(vals)) => eval_form(&vals.iter().collect::<Vec<&Value>>(), env),
        x => Ok(x.clone()),
    }
}

fn eval_form(list: Args, env: &mut EnvPtr) -> EvalResult {
    let head = list.first().ok_or(LankError::EmptyList)?;

    match head {
        Value::Symbol(s) if BINARY_OPS.contains(&**s) => eval_binary_op(list, env),
        Value::Symbol(s) if UNARY_OPS.contains(&**s) => eval_unary(list, env),
        Value::Symbol(s) if BOOL_OPS.contains(&**s) => eval_bool(list, env),
        Value::Symbol(s) if BIT_OPS.contains(&**s) => eval_bit_op(list, env),
        Value::Symbol(s) if TYPE_CHECKS.contains(&**s) => check_type(list, env),
        Value::Symbol(s) => {
            let s = &**s;
            let list = &list[1..];
            match s {
                "def" => eval_def(list, env),
                "defn" => defn(list, env),
                "type-of" => eval_type_of(list, env),
                "let" => eval_let(list, env),
                "do" => eval_do(list, env),
                "fn" => eval_fn_def(list),
                "display" => display(list, env),
                "rand" => gen_rand(list, env),

                // conditionals
                "if" | "?" => eval_ternary(list, env),
                "if-let" => eval_if_let(list, env),
                "when-let" => eval_when_let(list, env),
                "if-not" => eval_if_not(list, env),
                "when-not" => eval_when_not(list, env),
                "when" => eval_when(list, env),
                "match" => eval_match(list, env),

                // nil testing
                "none?" => eval_none(list, env),
                "some?" => eval_some(list, env),

                // type coercion
                "char" => eval_char(list, env),
                "long" => eval_long(list, env),

                // collections ( strings are colls here >:) )
                "nth" => eval_nth(list, env),
                "rand-nth" => rand_nth(list, env),
                "sort" => eval_sort(list, env),
                "shuffle" => eval_shuffle(list, env),
                "list" | "vec" | "str" | "bit-seq" => make_coll(s, list, env),
                "range" => eval_range(list, env),
                "count" => eval_count(list, env),
                "first" => eval_first(list, env),
                "second" => eval_second(list, env),
                "last" => eval_last(list, env),
                "rest" => eval_rest(list, env),
                "prepend" => eval_prepend(list, env),
                "append" => eval_append(list, env),
                "take" => eval_take(list, env),
                "drop" => eval_drop(list, env),
                "reverse" => eval_reverse(list, env),
                "map" => eval_map(list, env),
                "map-indexed" => eval_map_indexed(list, env),
                "reduce" => eval_reduce(list, env),
                "filter" => eval_filter(list, env),
                "repeat" => eval_repeat(list, env),
                "apply" => eval_apply(list, env),
                "concat" => eval_concat(list, env),
                "format" => eval_format(list, env),
                "bytes" => eval_bytes(list, env),

                // Maps
                "hashmap" => make_map(list, env),
                "get" => eval_get(list, env),
                "update" => eval_update(list, env),
                "keys" => eval_keys(list, env),
                "vals" => eval_vals(list, env),
                "merge" => eval_merge(list, env),
                "zipmap" => eval_zipmap(list, env),

                // Macros
                "replace" => eval_replace(list, env),

                _ => eval_fn_call(s, list, env),
            }
        }

        Value::Fun(_, _) => eval_lambda_call(head, &list[1..], env),

        Value::Form(form) => match form {
            Form::Quoted(_) => Ok((*head).clone()),
            Form::Unquoted(vals) => {
                let res = eval_form(&vals.iter().collect::<Vec<&Value>>(), env)?;
                if let Value::Fun(_, _) = res {
                    eval_lambda_call(&res, &list[1..], env)
                } else {
                    Ok(res)
                }
            }
        },

        _ => {
            let xs = list
                .iter()
                .filter(|x| ***x != Value::None)
                .map(|v| eval_value(v, env))
                .collect::<IterResult>()?;
            Ok(Value::from(xs))
        }
    }
}
