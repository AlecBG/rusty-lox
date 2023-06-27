use std::convert::TryFrom;

use crate::scanner::Token;
use crate::scanner::TokenType;

use super::expression::{BinaryOperator, Expr, UnaryOperator};

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

#[derive(Debug)]
pub struct ParserError {
    pub token: Token,
    pub message: String,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Parser {
        Parser { tokens, current: 0 }
    }

    pub fn parse_expression(&mut self) -> Result<Expr, ParserError> {
        self.equality()
    }

    fn equality(&mut self) -> Result<Expr, ParserError> {
        let result_expr = self.comparison();
        let mut expression = match result_expr {
            Ok(expr) => expr,
            e => return e,
        };
        while self.matches(&[TokenType::BangEqual, TokenType::EqualEqual]) {
            let previous_token = self.previous(); // We know this is now != or ==
            let operator = BinaryOperator::try_from(previous_token.token_type.clone()).unwrap();
            let right_expr_result = self.comparison();
            let right = match right_expr_result {
                Ok(expr) => expr,
                e => return e,
            };
            expression = Expr::Binary {
                left: Box::new(expression),
                operator,
                right: Box::new(right),
            };
        }
        Ok(expression)
    }

    fn comparison(&mut self) -> Result<Expr, ParserError> {
        let result_expr = self.term();
        let mut expression = match result_expr {
            Ok(expr) => expr,
            e => return e,
        };
        while self.matches(&[
            TokenType::Greater,
            TokenType::GreaterEqual,
            TokenType::Less,
            TokenType::LessEqual,
        ]) {
            let previous_token = self.previous(); // We know this is now >, >=, < or <=
            let operator = BinaryOperator::try_from(previous_token.token_type.clone()).unwrap();
            let right_expr_result = self.term();
            let right = match right_expr_result {
                Ok(expr) => expr,
                e => return e,
            };
            expression = Expr::Binary {
                left: Box::new(expression),
                operator,
                right: Box::new(right),
            };
        }
        Ok(expression)
    }

    fn term(&mut self) -> Result<Expr, ParserError> {
        let result_expr = self.factor();
        let mut expression = match result_expr {
            Ok(expr) => expr,
            e => return e,
        };
        while self.matches(&[TokenType::Minus, TokenType::Plus]) {
            let previous_token = self.previous(); // We know this is now - or +
            let operator = BinaryOperator::try_from(previous_token.token_type.clone()).unwrap();
            let right_expr_result = self.factor();
            let right = match right_expr_result {
                Ok(expr) => expr,
                e => return e,
            };
            expression = Expr::Binary {
                left: Box::new(expression),
                operator,
                right: Box::new(right),
            };
        }
        Ok(expression)
    }

    fn factor(&mut self) -> Result<Expr, ParserError> {
        let result_expr = self.unary();
        let mut expression = match result_expr {
            Ok(expr) => expr,
            e => return e,
        };
        while self.matches(&[TokenType::Slash, TokenType::Star]) {
            let previous_token = self.previous(); // We know this is now / or *
            let operator = BinaryOperator::try_from(previous_token.token_type.clone()).unwrap();
            let right_expr_result = self.unary();
            let right = match right_expr_result {
                Ok(expr) => expr,
                e => return e,
            };
            expression = Expr::Binary {
                left: Box::new(expression),
                operator,
                right: Box::new(right),
            };
        }
        Ok(expression)
    }

    fn unary(&mut self) -> Result<Expr, ParserError> {
        if self.matches(&[TokenType::Bang, TokenType::Minus]) {
            let previous_token = self.previous(); // We know this is now ! or -
            let unary_operator =
                UnaryOperator::try_from(previous_token.token_type.clone()).unwrap();
            let result_expression = self.unary();
            match result_expression {
                Ok(expression) => {
                    Expr::Unary {
                        operator: unary_operator,
                        expression: Box::new(expression),
                    };
                }
                e => return e,
            }
        }
        self.primary()
    }

    fn primary(&mut self) -> Result<Expr, ParserError> {
        let expr = match self.peek().token_type.clone() {
            TokenType::False => Expr::Boolean(false),
            TokenType::True => Expr::Boolean(true),
            TokenType::Nil => Expr::Nil,
            TokenType::Number(x) => Expr::Number(x),
            TokenType::String(x) => Expr::String(x),
            TokenType::LeftParen => {
                let result_expr = self.parse_expression();
                match result_expr {
                    Ok(expr) => {
                        match self.consume(TokenType::RightParen, "Expect ')' after expression.") {
                            Ok(_) => {}
                            Err(err) => return Err(err),
                        };
                        Expr::Grouping(Box::new(expr))
                    }
                    e => return e,
                }
            }
            unexpected_type => {
                return Err(Parser::error(
                    self.peek(),
                    &format!("Expect expression, found type {:?}.", unexpected_type),
                ));
            }
        };
        self.advance();
        Ok(expr)
    }

    fn synchronize(&mut self) {
        self.advance();
        while self.is_at_end() {
            if self.previous().token_type == TokenType::Semicolon {
                return;
            }

            match self.peek().token_type.clone() {
                TokenType::Class
                | TokenType::For
                | TokenType::Fun
                | TokenType::If
                | TokenType::Print
                | TokenType::Return
                | TokenType::Var
                | TokenType::While => {
                    return;
                }
                _ => {
                    self.advance();
                }
            }
        }
    }

    fn consume(&mut self, token_type: TokenType, message: &str) -> Result<&Token, ParserError> {
        if self.check(&token_type) {
            return Ok(self.advance());
        }
        Err(Parser::error(self.peek(), message))
    }

    fn error(token: &Token, message: &str) -> ParserError {
        let line_number = token.clone().line;
        let lexeme = token.clone().lexeme;
        let return_message = match token.token_type {
            TokenType::EOF => format!("{}, at end {}", line_number, message),
            _ => format!("{}, at '{:?}' {}", line_number, lexeme, message),
        };
        ParserError {
            token: token.clone(),
            message: return_message.to_string(),
        }
    }

    fn matches(&mut self, token_types: &[TokenType]) -> bool {
        for token_type in token_types {
            if self.check(token_type) {
                self.advance();
                return true;
            }
        }
        false
    }

    fn check(&self, token_type: &TokenType) -> bool {
        if self.is_at_end() {
            return false;
        }
        self.peek().token_type == token_type.clone()
    }

    fn advance(&mut self) -> &Token {
        if !self.is_at_end() {
            self.current += 1;
        }
        self.previous()
    }

    fn peek(&self) -> &Token {
        self.tokens.get(self.current).unwrap()
    }

    fn is_at_end(&self) -> bool {
        self.peek().token_type == TokenType::EOF
    }

    fn previous(&self) -> &Token {
        self.tokens.get(self.current - 1).unwrap()
    }
}
