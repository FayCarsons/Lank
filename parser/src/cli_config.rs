use std::{collections::HashSet, env};

#[derive(Debug, Clone)]
pub struct Config {
    pub print_tokens: bool,
    pub clear_history: bool,
    pub history_path: Option<String>,
}

impl Default for Config {
    fn default() -> Self {
        Self::new()
    }
}

impl Config {
    pub fn new() -> Self {
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
    }
}
