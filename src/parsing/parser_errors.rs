use std::{error::Error, fmt::Display};

use crate::scanning::Token;

#[derive(Debug)]
pub struct ParserError {
    pub token: Token,
    pub message: String,
}

impl Display for ParserError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&format!(
            "Parser error: {} at token {} on line {}",
            self.message, self.token, self.token.line
        ))
    }
}

impl Error for ParserError {}

#[derive(Debug)]
pub struct ParserErrors {
    pub errors: Vec<ParserError>,
}

impl Display for ParserErrors {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("Parser errors:")?;
        for error in &self.errors {
            f.write_str(&format!("\n    {error}"))?;
        }
        Ok(())
    }
}

impl Error for ParserErrors {}
