use std::{fs::read, io::stdin, path::Path};

use crate::interpreter::{Interpreter, RuntimeError};
use crate::parser::{Parser, ParserError};
use crate::scanner::{Scanner, SyntaxError};

#[derive(Debug)]
pub enum LoxError {
    ParserError(ParserError),
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
            Err(LoxError::ParserError(_)) | Err(LoxError::SyntaxError(_)) => std::process::exit(65),
            Err(LoxError::RuntimeError(_)) => std::process::exit(70),
        }
    }

    pub fn run_prompt(&mut self) {
        loop {
            let mut buffer = String::new();
            stdin().read_line(&mut buffer).unwrap();
            if &buffer == "" {
                return;
            }
            let result = self.run(buffer);
            match result {
                Ok(()) => {}
                Err(e) => eprintln!("Error: {:?}", e),
            }
        }
    }

    fn run(&mut self, source: String) -> Result<(), LoxError> {
        println!("input: {}", &source);

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
        let expression_result = parser.parse_expression();
        let expression = match expression_result {
            Ok(expression) => expression,
            Err(err) => {
                eprintln!("Error: {:?}", err);
                return Err(LoxError::ParserError(err));
            }
        };

        let mut interpreter = Interpreter::new();
        let interpret_result = interpreter.interpret(expression);
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
