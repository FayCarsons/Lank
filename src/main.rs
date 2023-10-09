#![feature(array_try_from_fn)]
#![allow(clippy::non_canonical_partial_ord_impl)]
#![allow(clippy::unnecessary_lazy_evaluations)]
pub mod utils;
use utils::env::Env;

mod core;
use core::eval;

mod sequencer;

use linefeed::{Interface, ReadResult};
use std::{env, fs::File, io::Write, path::Path, sync::OnceLock, collections::HashSet};

use crate::core::Value;

const PROMPT: &str = "Lank> ";
pub struct Config {
    print_tokens: bool,
    clear_history: bool,
    history_path: Option<String>,
}

static CONFIG: OnceLock<Config> = OnceLock::new();

fn main() -> std::io::Result<()> {
    let config = CONFIG.get_or_init(|| {
        let env_args: HashSet<String> = HashSet::from_iter(env::args().skip(1));
        println!("Env args: {env_args:?}");
        let print_tokens = env_args.contains("-p") || env_args.contains("--print-ast");
        let clear_history = env_args.contains("-c") || env_args.contains("--clear-hisory");
        let history_path = if env_args.contains("-p") || env_args.contains("--history-path") {
            env_args.into_iter().find(|s| !s.starts_with('-'))
        } else {
            None
        };

        Config {
            print_tokens,
            clear_history,
            history_path,
        }
    });

    let stdout = std::io::stdout();
    let mut lock = stdout.lock();

    writeln!(lock, "Lank Version 0.0.1")?;
    writeln!(lock, "Enter 'exit' to exit")?;

    let reader = Interface::new("Lank").unwrap();
    if let Some(path) = &config.history_path {
        if Path::new(path).exists() ^ config.clear_history {
            reader.load_history(path)?
        } else if config.clear_history {
            reader.clear_history()
        } else {
            File::create("../history.txt")?;
        }
    }

    reader.set_prompt(PROMPT).unwrap();

    let mut env = Env::new_ptr();

    writeln!(lock, "Size of value enum: {}", std::mem::size_of::<Value>())?;

    while let ReadResult::Input(input) = reader.read_line().unwrap() {
        if input.eq("exit") {
            break;
        } else if input.eq("(display env)") {
            writeln!(lock, "Lank> env: {:#?}", *env.clone())?;
        } else if input.eq("") {
            continue;
        } else {
            let val = eval(input.as_ref(), &mut env);

            match val {
                Err(e) => writeln!(lock, "{e}")?,
                Ok(v) => writeln!(lock, "{v}")?,
            }
            lock.flush()?
        }
        reader.add_history(input);
    }
    reader.save_history("../history.txt")?;
    writeln!(lock, "Ok bye :3")?;
    Ok(())
}
