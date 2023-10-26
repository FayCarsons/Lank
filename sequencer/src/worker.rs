use super::metro::Metro;
use model::{env::Env, error::LankError};
use std::{thread::{self, JoinHandle}, sync::Arc};


pub struct Worker {
    env: Arc<Env>,
    handle: JoinHandle<Result<(), LankError>>
    
}

impl Worker {
}