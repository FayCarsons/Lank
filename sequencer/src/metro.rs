use lank_core::args::{assert_bool, assert_num};
use model::{
    error::LankError,
    value::{Form, Map, Value},
};
use std::rc::Rc;
#[derive(Clone, Debug)]
pub struct Metro {
    /// Takes the function to be executed each (interval) ms
    /// an optional indexing operation, if None defaults to (defn inc [x] (+ x 1))
    /// and an environment
    operation: Form,
    indexing_operation: Option<Form>,

    interval: usize,

    idx: usize,
    start: usize,
    end: usize,
    one_shot: bool,
}

impl Default for Metro {
    fn default() -> Self {
        Metro {
            operation: Form::from(
                &[
                    Value::Symbol(Rc::from("display")),
                    Value::Symbol(Rc::from("idx")),
                ][..],
            ),
            indexing_operation: None,

            interval: Self::bpm2ms(&120.),

            idx: 0,
            start: 0,
            end: 15,
            one_shot: false,
        }
    }
}

impl Metro {
    pub fn new(operation: &Form, opts: Option<Map>) -> Result<Self, LankError> {
        Ok(if opts.is_some() {
            let opts = opts.unwrap();
            let (idx_op, interval, start, end, one_shot) = Self::parse_opts(opts)?;
            Metro {
                operation: operation.clone(),
                indexing_operation: idx_op,

                interval: interval.unwrap_or_else(|| Self::bpm2ms(&120.)),

                idx: 0,
                start: start.unwrap_or(0),
                end: end.unwrap_or(15),
                one_shot: one_shot.unwrap_or(false),
            }
        } else {
            Metro {
                operation: operation.clone(),
                ..Default::default()
            }
        })
    }

    fn parse_opts(
        opts: Map,
    ) -> Result<
        (
            Option<Form>,
            Option<usize>,
            Option<usize>,
            Option<usize>,
            Option<bool>,
        ),
        LankError,
    > {
        let idx_op = opts.get(&Value::from("idx-op")).and_then(|opt| {
            let Value::Form(ret) = opt else { return None };
            Some(ret)
        });

        let interval = if let Some(bpm) = opts.get(&Value::from("bpm")) {
            let inner = assert_num(bpm, LankError::WrongType("set-bpm(internal)".to_owned()))?;
            Some(Self::bpm2ms(&inner))
        } else {
            opts.get(&Value::from("ms")).and_then(|val| {
                let Value::Number(num) = val else { return None };
                Some(*num as usize)
            })
        };

        let start = opts
            .get(&Value::from("start"))
            .and_then(|opt| assert_num(opt, LankError::WrongType("Defmetro".to_owned())).ok());
        let end = opts
            .get(&Value::from("end"))
            .and_then(|opt| assert_num(opt, LankError::WrongType("Defmetro".to_owned())).ok());
        let one_shot = opts
            .get(&Value::from("oneshot"))
            .and_then(|opt| assert_bool(opt, LankError::WrongType("Defmetro".to_owned())).ok());
        Ok((
            idx_op.cloned(),
            interval,
            start.map(|f| f as usize),
            end.map(|f| f as usize),
            one_shot,
        ))
    }

    pub fn bpm2ms(bpm: &f64) -> usize {
        (60_000. / bpm) as usize / 16
    }
}
