use std::{
    fs::read,
    io::stdin,
    io::{self, Write},
    path::Path,
};

use crate::interpreter::{interpret, interpret_with_environment, Environment, RuntimeError};
use crate::parser::{parse, ParserErrors};
use crate::scanner::{scan_tokens, SyntaxError};

#[derive(Debug)]
pub enum LoxError {
    ParserErrors(ParserErrors),
    SyntaxError(SyntaxError),
    RuntimeError(RuntimeError),
}

pub fn run_file(path: &Path) {
    let file_contents = String::from_utf8(read(path).unwrap()).unwrap();
    let result = run(file_contents);
    match result {
        Ok(()) => {}
        Err(LoxError::ParserErrors(_)) | Err(LoxError::SyntaxError(_)) => std::process::exit(65),
        Err(LoxError::RuntimeError(_)) => std::process::exit(70),
    }
}

pub fn run_prompt() {
    let mut prompt_environment = Environment::new();
    loop {
        let mut buffer = String::new();
        print!("> ");
        io::stdout().flush().unwrap();
        stdin().read_line(&mut buffer).unwrap();
        if &buffer == "" {
            return;
        }
        let tokens = match scan_tokens(buffer) {
            Ok(x) => x,
            Err(err) => {
                eprintln!("Error: {:?}", err);
                continue;
            }
        };
        let statements_result = parse(tokens);
        let statements = match statements_result {
            Ok(statements) => {
                if statements.len() > 1 {
                    eprintln!("Only one statement per line allowed");
                    continue;
                }
                if statements.len() == 0 {
                    continue;
                }
                statements
            }
            Err(err) => {
                eprintln!("Error: {:?}", err);
                continue;
            }
        };
        let result = interpret_with_environment(statements, prompt_environment);
        prompt_environment = match result {
            Ok(environment) => environment,
            Err((env, err)) => {
                eprintln!("Error: {:?}", err);
                env
            }
        };
    }
}

fn run(source: String) -> Result<(), LoxError> {
    let tokens = match scan_tokens(source) {
        Ok(x) => x,
        Err(err) => {
            eprintln!("Error: {:?}", err);
            return Err(LoxError::SyntaxError(err));
        }
    };

    let statements = match parse(tokens) {
        Ok(s) => s,
        Err(err) => {
            eprintln!("Error: {:?}", err);
            return Err(LoxError::ParserErrors(err));
        }
    };

    let interpret_result = interpret(statements);
    match interpret_result {
        Ok(_) => {}
        Err(err) => {
            eprintln!("Error: {:?}", err);
            return Err(LoxError::RuntimeError(err));
        }
    };

    Ok(())
}
