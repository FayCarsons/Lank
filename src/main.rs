mod utils;
use linefeed::{Interface, ReadResult};
use utils::{env::Env, eval::eval, parser::Object};

const PROMPT: &str = "Lank> ";

fn log<T>(x: T) -> T
where
    T: std::fmt::Display,
{
    println!("{x}");
    x
}

fn main() -> std::io::Result<()> {
    println!("Lank Version 0.0.1");
    println!("Press CTRL + c to Exit");

    let reader = Interface::new("Lank").unwrap();
    reader.load_history("../history.txt").unwrap_or_default();
    reader.set_prompt(PROMPT).unwrap();

    let mut env = Env::new_ptr();

    while let ReadResult::Input(input) = reader.read_line().unwrap() {
        if input.eq("exit") {
            break;
        } else if input.eq("(display env)") {
            println!("Lank> env: {:#?}", *env.clone());
        } else if input.eq("") {
            continue
        } else {
            let val = eval(input.as_ref(), &mut env);
            print!("Lank> ");
            match val {
                Err(e) => println!("{e} "),
                Ok(Object::Void) => {}
                Ok(Object::Int(n)) => println!("{n}"),
                Ok(Object::Bool(b)) => println!("{b}"),
                Ok(Object::Symbol(s)) => println!("{s}"),
                Ok(Object::Func(params, body)) => {
                    println!("Func (");
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
