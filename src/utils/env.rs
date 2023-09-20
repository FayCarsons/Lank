use super::value::Value;
use std::{cell::RefCell, collections::HashMap, rc::Rc};

pub type EnvPtr = Rc<RefCell<Env>>;

#[derive(Clone, Debug, PartialEq)]
pub struct Env {
    parent: Option<Rc<RefCell<Env>>>,
    vars: HashMap<String, Value>,
}

impl Env {
    pub fn new() -> Self {
        Env {
            vars: HashMap::new(),
            parent: None,
        }
    }

    pub fn new_ptr() -> EnvPtr {
        Rc::new(RefCell::new(Env::new()))
    }

    pub fn extend(parent: Rc<RefCell<Self>>) -> Env {
        Env {
            vars: HashMap::new(),
            parent: Some(parent),
        }
    }

    pub fn new_extended(parent: Rc<RefCell<Self>>) -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(Self::extend(parent)))
    }

    pub fn get(&self, name: &str) -> Option<Value> {
        match self.vars.get(name) {
            Some(value) => Some(value.clone()),
            None => self
                .parent
                .as_ref()
                .and_then(|o| o.borrow().get(name).clone()),
        }
    }

    pub fn set(&mut self, name: &str, val: Value) {
        self.vars.insert(name.to_string(), val);
    }

    pub fn merge(&self, other: Env) -> Self {
        Env {
            vars: self
                .vars
                .iter()
                .chain(other.vars.iter())
                .map(|(k, v)| (k.clone(), v.clone()))
                .collect(),
            parent: if let Some(p) = &self.parent {
                Some(p.clone())
            } else if let Some(p) = other.parent {
                Some(p)
            } else {
                None
            },
        }
    }
}
