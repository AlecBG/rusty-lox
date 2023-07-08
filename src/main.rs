use std::{env, path::Path};

use lox::lox::{run_file, run_prompt};

fn main() {
    let args: Vec<String> = env::args().collect();
    match args.len() {
        1 => run_prompt(), //run_file(Path::new("examples/functions.lox")),
        2 => run_file(Path::new(&args[1])),
        _ => {
            println!("Received {} arguments, that's too many!", &args.len());
            std::process::exit(64);
        }
    }
}
