use std::string::ToString;

use crate::scanner::token_type::TokenType;

#[derive(Debug, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub line: usize,
}

impl ToString for Token {
    fn to_string(&self) -> String {
        format!("{:?}", self.token_type)
    }
}
