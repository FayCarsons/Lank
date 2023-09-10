use super::parser::Object;
use std::{cell::RefCell, collections::HashMap, rc::Rc};

pub type EnvPtr = Rc<RefCell<Env>>;

#[derive(Clone, Debug, PartialEq)]
pub struct Env {
    parent: Option<Rc<RefCell<Env>>>,
    vars: HashMap<String, Object>,
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

    pub fn get(&self, name: &str) -> Option<Object> {
        match self.vars.get(name) {
            Some(value) => Some(value.clone()),
            None => self
                .parent
                .as_ref()
                .and_then(|o| o.borrow().get(name).clone()),
        }
    }

    pub fn set(&mut self, name: &str, val: Object) {
        self.vars.insert(name.to_string(), val);
    }
}
