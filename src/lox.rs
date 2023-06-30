use std::{
    fs::read,
    io::stdin,
    io::{self, Write},
    path::Path,
};

use crate::interpreter::{Interpreter, RuntimeError};
use crate::parser::{Parser, ParserErrors};
use crate::scanner::{Scanner, SyntaxError};

#[derive(Debug)]
pub enum LoxError {
    ParserErrors(ParserErrors),
    SyntaxError(SyntaxError),
    RuntimeError(RuntimeError),
}

pub struct Lox {}

impl Lox {
    pub fn run_file(&mut self, path: &Path) {
        let file_contents = String::from_utf8(read(path).unwrap()).unwrap();
        let result = self.run(file_contents);
        match result {
            Ok(()) => {}
            Err(LoxError::ParserErrors(_)) | Err(LoxError::SyntaxError(_)) => {
                std::process::exit(65)
            }
            Err(LoxError::RuntimeError(_)) => std::process::exit(70),
        }
    }

    pub fn run_prompt(&mut self) {
        loop {
            let mut buffer = String::new();
            print!("> ");
            io::stdout().flush().unwrap();
            stdin().read_line(&mut buffer).unwrap();
            if &buffer == "" {
                return;
            }
            let result = self.run(buffer);
            match result {
                Ok(()) => {}
                Err(_) => {}
            }
        }
    }

    fn run(&mut self, source: String) -> Result<(), LoxError> {
        let mut scanner = Scanner::new(source);
        let tokens_result = scanner.scan_tokens();
        let tokens = match tokens_result {
            Ok(x) => x,
            Err(err) => {
                eprintln!("Error: {:?}", err);
                return Err(LoxError::SyntaxError(err));
            }
        };

        let mut parser = Parser::new(tokens);
        let statements_results = parser.parse();
        let statements = match statements_results {
            Ok(s) => s,
            Err(err) => {
                eprintln!("Error: {:?}", err);
                return Err(LoxError::ParserErrors(err));
            }
        };

        let mut interpreter = Interpreter::new();
        let interpret_result = interpreter.interpret(statements);
        match interpret_result {
            Ok(()) => {}
            Err(err) => {
                eprintln!("Error: {:?}", err);
                return Err(LoxError::RuntimeError(err));
            }
        };

        Ok(())
    }
}
