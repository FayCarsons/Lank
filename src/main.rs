#![feature(iterator_try_reduce)]
#![feature(try_trait_v2)]
pub mod utils;
use utils::{env::Env, value::Value};

mod core;
use core::eval;

use linefeed::{Interface, ReadResult};
use std::{env, fs::File, path::Path};

const PROMPT: &str = "Lank> ";

fn main() -> std::io::Result<()> {
    {
        println!("Size of Val: {}", std::mem::size_of::<Value>());
    }

    println!("Lank Version 0.0.1");
    println!("Enter 'exit' to exit");

    let reader = Interface::new("Lank").unwrap();

    let history_path = Path::new("../history.txt");

    let env_args = env::args().collect::<Vec<String>>();
    if history_path.exists() && ! env_args.contains(&"-c".to_string()) | env_args.contains(&"--clear-history".to_string()) {
        reader.load_history(history_path).unwrap();
    } else {
        File::create("../history.txt")?;
    }

    reader.set_prompt(PROMPT).unwrap();

    let mut env = Env::new_ptr();

    while let ReadResult::Input(input) = reader.read_line().unwrap() {
        if input.eq("exit") {
            break;
        } else if input.eq("(display env)") {
            println!("Lank> env: {:#?}", *env.clone());
        } else if input.eq("") {
            continue;
        } else {
            let val = eval(input.as_ref(), &mut env);
            print!("Lank> ");
            match val {
                Err(e) => println!("{e}"),
                Ok(v) => println!("{v}")
            }
        }
        reader.add_history(input);
    }
    reader.save_history("../history.txt")?;
    println!("Bye :3");
    Ok(())
}
