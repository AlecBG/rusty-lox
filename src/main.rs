use std::{env, path::Path};

use lox::lox::{run_file, run_prompt};

fn main() {
    let args: Vec<String> = env::args().collect();
    if &args.len() > &2 {
        println!("Received {} arguments, that's too many!", &args.len());
        std::process::exit(64);
    } else if &args.len() == &2 {
        run_file(&Path::new(&args[1]));
    } else {
        run_prompt();
    }
}
