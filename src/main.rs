use std::{env, path::Path};

use lox::lox::Lox;

fn main() {
    let mut lox = Lox {};
    let args: Vec<String> = env::args().collect();
    if &args.len() > &2 {
        println!("Received {} arguments, that's too many!", &args.len());
        std::process::exit(64);
    } else if &args.len() == &2 {
        lox.run_file(&Path::new(&args[1]));
    } else {
        lox.run_prompt();
    }
}
