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
    println!("Press CTRL + c to Exit");

    let reader = Interface::new("Lank").unwrap();

    let history_path = Path::new("../history.txt");

    let history_arg = env::args().next().unwrap_or("".to_string());
    if history_path.exists() && !matches!(history_arg.as_str(), "-c" | "--clear") {
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
                Err(e) => println!("{e} "),
                Ok(Value::Void) => {}
                Ok(Value::Number(n)) => println!("{n}"),
                Ok(Value::Bool(b)) => println!("{b}"),
                Ok(Value::Symbol(s)) => println!("{s}"),
                Ok(Value::Fun(params, body)) => {
                    println!("Fun (");
                    params.iter().for_each(|p| println!("{p} "));
                    println!(") ");
                    body.iter().for_each(|exp| println!("({exp}) "));
                }
                Ok(x) => println!("{x}"),
            }
        }
        reader.add_history(input);
    }
    reader.save_history("../history.txt")?;
    println!("Bye :3");
    Ok(())
}
