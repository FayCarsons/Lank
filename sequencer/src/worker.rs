use model::{env::Env, error::LankError};
use std::{sync::Arc, thread::JoinHandle};

pub struct Worker {
    env: Arc<Env>,
    handle: JoinHandle<Result<(), LankError>>,
}

impl Worker {}
