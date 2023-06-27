use std::string::ToString;

use crate::scanner::token_type::TokenType;

#[derive(Debug, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub lexeme: Option<String>,
    pub line: usize,
}

impl ToString for Token {
    fn to_string(&self) -> String {
        let lexeme = match &self.lexeme {
            Some(lex) => lex,
            None => "NONE",
        };
        format!("{:?} {}", self.token_type, lexeme)
    }
}
