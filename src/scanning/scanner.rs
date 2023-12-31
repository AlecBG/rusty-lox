use std::collections::HashMap;
use std::error::Error;
use std::fmt::Display;

use super::token::Token;

use super::token_type::TokenType;

#[derive(Debug)]
pub struct SyntaxError {
    pub message: String,
    pub line: usize,
}

impl Display for SyntaxError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&format!("Syntax error: {} at {}", self.message, self.line))
    }
}

impl Error for SyntaxError {}

pub fn scan_tokens(source: &str) -> Result<Vec<Token>, SyntaxError> {
    let mut scanner = Scanner::new(source);
    scanner.scan_tokens()
}

struct Scanner {
    source_chars: Vec<char>,
    tokens: Vec<Token>,
    start: usize,
    current: usize,
    line: usize,
    keywords: HashMap<String, TokenType>,
}

impl Scanner {
    fn new(source: &str) -> Scanner {
        let source_chars: Vec<char> = source.chars().collect();
        let keywords: HashMap<String, TokenType> = HashMap::from([
            ("and".to_string(), TokenType::And),
            ("class".to_string(), TokenType::Class),
            ("else".to_string(), TokenType::Else),
            ("false".to_string(), TokenType::False),
            ("for".to_string(), TokenType::For),
            ("fun".to_string(), TokenType::Fun),
            ("if".to_string(), TokenType::If),
            ("nil".to_string(), TokenType::Nil),
            ("or".to_string(), TokenType::Or),
            ("print".to_string(), TokenType::Print),
            ("return".to_string(), TokenType::Return),
            ("super".to_string(), TokenType::Super),
            ("this".to_string(), TokenType::This),
            ("true".to_string(), TokenType::True),
            ("var".to_string(), TokenType::Var),
            ("while".to_string(), TokenType::While),
        ]);
        Scanner {
            source_chars,
            tokens: vec![],
            start: 0,
            current: 0,
            line: 1,
            keywords,
        }
    }

    fn scan_tokens(&mut self) -> Result<Vec<Token>, SyntaxError> {
        while !self.is_at_end() {
            self.start = self.current;
            match self.scan_token() {
                Ok(_) => {}
                Err(err) => return Err(err),
            }
        }
        self.add_token(TokenType::EOF);
        Ok(self.tokens.clone())
    }

    fn scan_token(&mut self) -> Result<(), SyntaxError> {
        let c = self.advance();
        match c {
            '(' => self.add_token(TokenType::LeftParen),
            ')' => self.add_token(TokenType::RightParen),
            '{' => self.add_token(TokenType::LeftBrace),
            '}' => self.add_token(TokenType::RightBrace),
            ',' => self.add_token(TokenType::Comma),
            '.' => self.add_token(TokenType::Dot),
            '-' => self.add_token(TokenType::Minus),
            '+' => self.add_token(TokenType::Plus),
            ';' => self.add_token(TokenType::Semicolon),
            '*' => self.add_token(TokenType::Star),

            '!' => {
                if self.is_current_char('=') {
                    self.add_token(TokenType::BangEqual);
                } else {
                    self.add_token(TokenType::Bang);
                }
            }
            '=' => {
                if self.is_current_char('=') {
                    self.add_token(TokenType::EqualEqual);
                } else {
                    self.add_token(TokenType::Equal);
                }
            }
            '<' => {
                if self.is_current_char('=') {
                    self.add_token(TokenType::LessEqual);
                } else {
                    self.add_token(TokenType::Less);
                }
            }
            '>' => {
                if self.is_current_char('=') {
                    self.add_token(TokenType::GreaterEqual);
                } else {
                    self.add_token(TokenType::Greater);
                }
            }

            '/' => {
                if self.is_current_char('/') {
                    while self.peek() != '\n' && !self.is_at_end() {
                        self.advance();
                    }
                } else {
                    self.add_token(TokenType::Slash);
                }
            }

            // Ignore whitespace
            ' ' | '\r' | '\t' => {}

            '\n' => {
                self.line += 1;
            }

            '"' => match self.add_string_token() {
                Ok(_) => {}
                Err(err) => return Err(err),
            },
            '0'..='9' => self.add_number_token(),
            'a'..='z' | 'A'..='Z' | '_' => self.add_identifier_token(),

            _ => {
                return Err(SyntaxError {
                    message: format!("{} Unexpected character: '{}'", self.line, c),
                    line: self.line,
                });
            }
        };
        Ok(())
    }

    fn advance(&mut self) -> char {
        let c = self.source_chars[self.current];
        self.current += 1;
        c
    }

    fn add_token(&mut self, token_type: TokenType) {
        self.tokens.push(Token {
            token_type,
            line: self.line,
        });
    }

    fn add_string_token(&mut self) -> Result<(), SyntaxError> {
        while self.peek() != '"' && !self.is_at_end() {
            if self.peek() == '\n' {
                self.line += 1;
            }
            self.advance();
        }
        if self.is_at_end() {
            error(self.line, "Unterminated string.");
            return Err(SyntaxError {
                message: "Unterminated string.".to_string(),
                line: self.line,
            });
        }
        self.advance(); // The closing '"'
        let string_value: String = self.source_chars[self.start + 1..self.current - 1]
            .iter()
            .collect();
        self.add_token(TokenType::String(string_value));
        Ok(())
    }

    fn add_number_token(&mut self) {
        while is_digit(self.peek()) {
            self.advance();
        }
        // look for a decimal point
        if self.peek() == '.' && is_digit(self.peek_next()) {
            // Consume the '.'
            self.advance();

            while is_digit(self.peek()) {
                self.advance();
            }
        }

        let number_string: String = self.source_chars[self.start..self.current].iter().collect();
        let number: f64 = number_string.parse::<f64>().unwrap();
        self.add_token(TokenType::Number(number));
    }

    fn add_identifier_token(&mut self) {
        while is_alphanumeric(self.peek()) {
            self.advance();
        }
        let text: String = self.source_chars[self.start..self.current].iter().collect();
        let possible_keyword_token = self.keywords.get(&text);
        match possible_keyword_token {
            Some(keyword) => self.add_token(keyword.clone()),
            None => {
                self.add_token(TokenType::Identifier(text));
            }
        }
    }

    fn is_current_char(&mut self, expected_char: char) -> bool {
        if self.is_at_end() {
            return false;
        }
        if self.source_chars[self.current] != expected_char {
            return false;
        }
        self.current += 1;
        true
    }

    fn peek(&self) -> char {
        if self.is_at_end() {
            return '\0';
        }
        self.source_chars[self.current]
    }

    fn peek_next(&self) -> char {
        if self.current + 1 >= self.source_chars.len() {
            return '\0';
        }
        self.source_chars[self.current + 1]
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.source_chars.len()
    }
}

fn is_digit(c: char) -> bool {
    c.is_ascii_digit()
}

fn is_alpha(c: char) -> bool {
    c.is_ascii_lowercase() || c.is_ascii_uppercase() || c == '_'
}

fn is_alphanumeric(c: char) -> bool {
    is_digit(c) || is_alpha(c)
}

fn error(line: usize, message: &str) {
    report(line, "", message);
}

fn report(line: usize, where_: &str, message: &str) {
    eprintln!("[line {line}] Error{where_}: {message}");
}
