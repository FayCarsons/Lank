#![feature(array_try_from_fn)]
#![allow(clippy::non_canonical_partial_ord_impl)]
#![allow(clippy::unnecessary_lazy_evaluations)]
pub mod utils;
use utils::env::Env;

mod core;
use core::eval;

use linefeed::{Interface, ReadResult};
use std::{env, fs::File, io::Write, path::Path, sync::OnceLock};

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
        let mut env_args = env::args();
        let mut print_tokens = false;
        let mut clear_history = false;
        let mut history_path = None;

        while let Some(ref arg) = env_args.next() {
            match arg.as_ref() {
                "-c" | "--clear-history" => clear_history = true,
                "-t" | "--print-ast" => print_tokens = true,
                "-p" | "--history-path" => history_path = env_args.next(),
                _ => continue,
            }
        }

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

    println!("Size of value enum: {}", std::mem::size_of::<Value>());

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
        }
        reader.add_history(input);
    }
    reader.save_history("../history.txt")?;
    writeln!(lock, "Ok bye :3")?;
    Ok(())
}
