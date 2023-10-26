use super::{error::LankError, value::Value};
use std::{collections::HashMap, rc::Rc, sync::RwLock};

pub type EnvPtr = Rc<RwLock<Env>>;

#[derive(Clone, Debug)]
pub struct Env {
    vars: HashMap<String, Value>,
    parent: Option<EnvPtr>,
}

impl Default for Env {
    fn default() -> Self {
        Env {
            vars: HashMap::new(),
            parent: None,
        }
    }
}

impl Env {
    fn new() -> Self {
        Env::default()
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
        Env { vars, parent }.to_ptr()
    }

    pub fn set(&mut self, name: &str, val: Value) {
        self.vars.insert(name.to_string(), val);
    }

    pub fn get(&self, name: &str) -> Option<Value> {
        match self.vars.get(name) {
            Some(v) => Some(v.clone()),
            None => {
                let mut parent = self.parent.clone();

                while let Some(current) = parent {
                    let current_read = current.read().ok()?;

                    if let Some(ret) = current_read.vars.get(name) {
                        return Some(ret.clone());
                    } else {
                        parent = current_read.parent.clone();
                    }
                }

                None
            }
        }
    }

    pub fn merge(&self, other: EnvPtr) -> Result<Self, LankError> {
        let other = other.read()?.clone();
        Ok(Env {
            vars: self.vars.clone().into_iter().chain(other.vars).collect(),
            parent: if let Some(p) = &self.parent {
                Some(p.clone())
            } else {
                other.parent
            },
        })
    }
}

pub fn set_env(param: &str, val: &Value, env: &EnvPtr) -> Result<(), LankError> {
    if env.read()?.vars.contains_key(param) {
        return Err(LankError::Redefinition(param.to_string()));
    }

    let mut lock = env.write().map_err(|_| LankError::EnvPoison)?;
    lock.set(param, val.clone());
    Ok(())
}

pub fn get_env(name: &str, env: &EnvPtr) -> Option<Value> {
    let lock = env.read().ok()?;
    lock.get(name)
}
