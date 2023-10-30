use super::metro::Metro;
use model::{env::Env, error::LankError};
use std::{
    sync::Arc,
    thread::{self, JoinHandle},
};

pub struct Worker {
    env: Arc<Env>,
    handle: JoinHandle<Result<(), LankError>>,
}

impl Worker {}
