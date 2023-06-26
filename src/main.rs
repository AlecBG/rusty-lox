use std::{env, fs::read, io::stdin, path::Path};

use lox::parser::parser::Parser;
use lox::scanner::scanner::{Error, Scanner};

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

fn run_file(path: &Path) {
    let file_contents = String::from_utf8(read(path).unwrap()).unwrap();
    let result = run(file_contents);
    match result {
        Ok(()) => {}
        Err(_) => std::process::exit(65),
    }
}

fn run_prompt() {
    loop {
        let mut buffer = String::new();
        stdin().read_line(&mut buffer).unwrap();
        if &buffer == "" {
            return;
        }
        let result = run(buffer);
        match result {
            Ok(()) => {}
            Err(_) => println!("oh no"),
        }
    }
}

fn run(source: String) -> Result<(), Error> {
    println!("input: {}", &source);
    let mut scanner = Scanner::new(source);
    let tokens = scanner.scan_tokens();
    let mut parser = Parser::new(tokens);
    let expression = parser.parse_expression();
    match expression {
        Ok(expr) => println!("Got the expression:\n{:?}", expr),
        Err(err) => {
            println!("Error: {:?}", err);
            return Err(Error {});
        }
    };
    Ok(())
}
