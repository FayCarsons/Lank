use crate::core::EvalResult;

use super::{value::Value, error::LankError};
use std::{cell::RefCell, collections::HashMap, rc::Rc, sync::RwLock};

pub type EnvPtr = Rc<RwLock<Env>>;

#[derive(Clone, Debug)]
pub struct Env {
    vars: HashMap<String, Value>,
    parent: Option<EnvPtr>
}

impl Env {
    fn new() -> Self {
        let vars = HashMap::new();
        let parent = None;
        Env {
            vars, parent
        }
    }

    pub fn new_ptr() -> EnvPtr {
        let new = Env::new();
        Rc::new(RwLock::new(new))
    }

    pub fn to_ptr(&self) -> EnvPtr {
        Rc::new(RwLock::new(self.clone()))
    }

    pub fn extend(parent: EnvPtr) -> EnvPtr {
        let vars = HashMap::new();
        let parent = Some(parent);
        Env{vars, parent}.to_ptr()
    }

    pub fn set(&mut self, name: &str, val: Value) {
        self.vars.insert(name.to_string(), val);
    }

    pub fn get(&self, name: &str) -> Result<Option<Value>, LankError> {
        match self.vars.get(name) {
            Some(v) => Ok(Some(v.clone())),
            None => {
                let mut parent = self.parent.clone();

                while let Some(current) = parent {
                    let current_read = current.read().map_err(|err| err.to_string())?;
                    if let Some(ret) = current_read.vars.get(name) {
                        return Ok(Some(ret.clone()))
                    } else {
                        parent = current_read.parent.clone();
                    }
                }

                Ok(Some(Value::Void))
            }
        }
    }

    pub fn merge(&self, other: EnvPtr) -> Result<Self, LankError> {
        let other = other.read()?.clone();
        Ok(Env {
            vars: self
                .vars
                .clone()
                .into_iter()
                .chain(other.vars)
                .collect(),
            parent: if let Some(p) = &self.parent {
                Some(p.clone())
            } else {
                other.parent
            },
        })
    }
}

pub fn set_env(param: &String, val: &Value, env: &mut EnvPtr) -> Result<(), LankError> {
    let mut lock = env.write().map_err(|_| LankError::Other("RWLOCK POINSED ON WRITE".to_owned()))?;
    lock.set(param, val.clone());
    Ok(())
}

pub fn get_env(name: &str, env: &EnvPtr) -> Result<Option<Value>, LankError> {
    let lock = env.read().map_err(|err| err.to_string())?;
    lock.get(name)
}

/* #[derive(Clone, Debug, PartialEq)]
pub struct Env {
    parent: Option<Rc<RefCell<Env>>>,
    vars: HashMap<String, Value>,
}

impl Default for Env {
    fn default() -> Self {
        Self::new()
    }
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
                .clone()
                .into_iter()
                .chain(other.vars)
                .collect(),
            parent: if let Some(p) = &self.parent {
                Some(p.clone())
            } else {
                other.parent
            },
        }
    }
} */
