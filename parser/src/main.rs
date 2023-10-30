#![feature(array_try_from_fn)]
#![allow(clippy::non_canonical_partial_ord_impl)]
#![allow(clippy::unnecessary_lazy_evaluations)]
pub mod parser;
use parser::eval;

pub mod cli_config;
use cli_config::Config;

use linefeed::{Interface, ReadResult};
use std::{fs::File, io::Write, path::Path, sync::OnceLock};

use model::env::Env;

const PROMPT: &str = "Lank> ";

static CONFIG: OnceLock<Config> = OnceLock::new();

fn main() -> std::io::Result<()> {
    let config = Config::new();
    CONFIG.get_or_init(|| config.clone());

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
