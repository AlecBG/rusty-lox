use std::{
    error::Error,
    fmt::Display,
    fs::read,
    io::stdin,
    io::{self, Write},
    path::Path,
};

use crate::interpreting::{Interpreter, RefCellEnvironment, RuntimeError};
use crate::parsing::{parse, ParserErrors};
use crate::resolving::{resolve, ResolverError};
use crate::scanning::{scan_tokens, SyntaxError};

#[derive(Debug)]
pub enum LoxError {
    ParserErrors(ParserErrors),
    SyntaxError(SyntaxError),
    ResolverError(ResolverError),
    RuntimeError(RuntimeError),
}

impl Display for LoxError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::ParserErrors(e) => f.write_str(&format!("{e}")),
            Self::SyntaxError(e) => f.write_str(&format!("{e}")),
            Self::ResolverError(e) => f.write_str(&format!("{e}")),
            Self::RuntimeError(e) => f.write_str(&format!("{e}")),
        }
    }
}

impl Error for LoxError {}

pub fn run_file(path: &Path) {
    let file_bytes = match read(path) {
        Ok(x) => x,
        Err(e) => {
            eprintln!("Error reading file: {e}");
            std::process::exit(1);
        }
    };
    let file_contents = match String::from_utf8(file_bytes) {
        Ok(x) => x,
        Err(e) => {
            eprintln!("Error parsing file as utf8: {e}");
            std::process::exit(1);
        }
    };
    let result = run(&file_contents);
    match result {
        Ok(()) => {}
        Err(LoxError::ParserErrors(_) | LoxError::SyntaxError(_)) => std::process::exit(65),
        Err(LoxError::ResolverError(_)) => std::process::exit(65),
        Err(LoxError::RuntimeError(_)) => std::process::exit(70),
    }
}

pub fn run_prompt() {
    let mut interpreter = Interpreter::new(Box::new(RefCellEnvironment::new()));
    loop {
        let mut buffer = String::new();
        print!("> ");
        if let Err(e) = io::stdout().flush() {
            eprintln!("Error reading std input: {e}");
            std::process::exit(1);
        };
        if let Err(e) = stdin().read_line(&mut buffer) {
            eprintln!("Error reading std input: {e}");
            std::process::exit(1);
        };
        if buffer.is_empty() {
            return;
        }
        let tokens = match scan_tokens(&buffer) {
            Ok(x) => x,
            Err(err) => {
                eprintln!("Error: {err:?}");
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
                if statements.is_empty() {
                    continue;
                }
                statements
            }
            Err(err) => {
                eprintln!("Error: {err:?}");
                continue;
            }
        };
        let result = interpreter.execute_statements(statements);
        match result {
            Ok(_) => {}
            Err(err) => {
                eprintln!("Error: {err:?}");
            }
        };
    }
}

fn run(source: &str) -> Result<(), LoxError> {
    let tokens = match scan_tokens(source) {
        Ok(x) => x,
        Err(err) => {
            eprintln!("Error: {err}");
            return Err(LoxError::SyntaxError(err));
        }
    };

    let statements = match parse(tokens) {
        Ok(s) => s,
        Err(err) => {
            eprintln!("Error: {err}");
            return Err(LoxError::ParserErrors(err));
        }
    };

    let mut interpreter = Interpreter::new(Box::new(RefCellEnvironment::new()));
    resolve(&mut interpreter, statements.clone()).map_err(LoxError::ResolverError)?;
    let interpret_result = interpreter.execute_statements(statements);
    match interpret_result {
        Ok(_) => {}
        Err(err) => {
            eprintln!("Error: {err}");
            return Err(LoxError::RuntimeError(err));
        }
    };

    Ok(())
}
