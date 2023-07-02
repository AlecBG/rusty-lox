use std::convert::TryFrom;

use crate::scanner::Token;
use crate::scanner::TokenType;

use super::expression::{BinaryOperator, Expr, UnaryOperator};
use super::statement::Stmt;

#[derive(Debug)]
pub struct ParserError {
    pub token: Token,
    pub message: String,
}

#[derive(Debug)]
pub struct ParserErrorLine {
    pub line: usize,
    pub error: ParserError,
}

#[derive(Debug)]
pub struct ParserErrors {
    pub errors: Vec<ParserErrorLine>,
}

pub fn parse(tokens: Vec<Token>) -> Result<Vec<Stmt>, ParserErrors> {
    let mut parser = Parser::new(tokens);
    parser.parse()
}

struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    fn new(tokens: Vec<Token>) -> Parser {
        Parser { tokens, current: 0 }
    }

    fn parse(&mut self) -> Result<Vec<Stmt>, ParserErrors> {
        let mut statements: Vec<Stmt> = vec![];
        let mut parser_errors_line: Vec<ParserErrorLine> = vec![];
        let mut line = 1;
        while !self.is_at_end() {
            match self.parse_declaration() {
                Ok(stmt) => statements.push(stmt),
                Err(error) => parser_errors_line.push(ParserErrorLine { line, error }),
            }
            line += 1;
        }
        if parser_errors_line.is_empty() {
            Ok(statements)
        } else {
            Err(ParserErrors {
                errors: parser_errors_line,
            })
        }
    }

    fn parse_declaration(&mut self) -> Result<Stmt, ParserError> {
        let result = if self.matches(&[TokenType::Var]) {
            self.parse_variable_declaration()
        } else {
            self.parse_statement()
        };
        match result {
            Ok(stmt) => Ok(stmt),
            Err(err) => {
                self.synchronize();
                Err(err)
            }
        }
    }

    fn parse_variable_declaration(&mut self) -> Result<Stmt, ParserError> {
        let token = self.peek().clone().token_type;
        match token {
            TokenType::Identifier(name) => {
                self.advance();
                let initializer = if self.matches(&[TokenType::Equal]) {
                    match self.parse_expression() {
                        Ok(e) => e,
                        Err(err) => return Err(err),
                    }
                } else {
                    Expr::Nil
                };
                match self.consume(
                    TokenType::Semicolon,
                    "Expect ';' after variable declaration",
                ) {
                    Ok(_) => {}
                    Err(err) => return Err(err),
                };
                Ok(Stmt::Var {
                    name: name.to_string(),
                    initializer,
                })
            }
            _ => Err(Parser::error(self.peek(), "Expect variable name.")),
        }
    }

    fn parse_statement(&mut self) -> Result<Stmt, ParserError> {
        if self.matches(&[TokenType::If]) {
            self.parse_if_statement()
        } else if self.matches(&[TokenType::Print]) {
            self.parse_print_statement()
        } else if self.matches(&[TokenType::LeftBrace]) {
            self.parse_block_statement()
        } else {
            self.parse_expression_statement()
        }
    }

    fn parse_if_statement(&mut self) -> Result<Stmt, ParserError> {
        match self.consume(TokenType::LeftParen, "Expect '(' after if.") {
            Ok(_) => {}
            Err(err) => return Err(err),
        };
        let condition = match self.parse_expression() {
            Ok(expr) => expr,
            Err(err) => return Err(err),
        };
        match self.consume(TokenType::RightParen, "Expect ')' after if condition.") {
            Ok(_) => {}
            Err(err) => return Err(err),
        };
        let then_stmt = match self.parse_statement() {
            Ok(stmt) => Box::new(stmt),
            Err(err) => return Err(err),
        };
        let else_stmt = match self.matches(&[TokenType::Else]) {
            true => match self.parse_statement() {
                Ok(stmt) => Some(Box::new(stmt)),
                Err(err) => return Err(err),
            },
            false => None,
        };
        Ok(Stmt::If {
            condition,
            then_stmt,
            else_stmt,
        })
    }

    fn parse_print_statement(&mut self) -> Result<Stmt, ParserError> {
        let value_result = self.parse_expression();
        let value = match value_result {
            Ok(expr) => expr,
            Err(err) => return Err(err),
        };
        match self.consume(TokenType::Semicolon, "Expect ';' after value.") {
            Ok(_) => {}
            Err(err) => return Err(err),
        };

        Ok(Stmt::Print(value))
    }

    fn parse_block_statement(&mut self) -> Result<Stmt, ParserError> {
        let mut statements: Vec<Stmt> = vec![];
        while !self.matches(&[TokenType::RightBrace]) && !self.is_at_end() {
            match self.parse_declaration() {
                Ok(s) => statements.push(s),
                Err(e) => return Err(e),
            }
        }
        // Have stepped past right brace (matches moves pointer forward by one), so we step one back
        //  to get to it.
        self.current -= 1;
        match self.consume(TokenType::RightBrace, "Expect '}' after block.") {
            Ok(_) => {}
            Err(e) => return Err(e),
        };
        Ok(Stmt::Block(statements))
    }

    fn parse_expression_statement(&mut self) -> Result<Stmt, ParserError> {
        let value_result = self.parse_expression();
        let value = match value_result {
            Ok(expr) => expr,
            Err(err) => return Err(err),
        };
        match self.consume(TokenType::Semicolon, "Expect ';' after expression.") {
            Ok(_) => {}
            Err(err) => return Err(err),
        };
        Ok(Stmt::Expression(value))
    }

    fn parse_expression(&mut self) -> Result<Expr, ParserError> {
        self.parse_assignment()
    }

    fn parse_assignment(&mut self) -> Result<Expr, ParserError> {
        let expression = match self.parse_equality() {
            Ok(e) => e,
            Err(err) => return Err(err),
        };
        if self.matches(&[TokenType::Equal]) {
            let value = match self.parse_assignment() {
                Ok(v) => v,
                Err(err) => return Err(err),
            };
            match expression {
                Expr::Variable(name) => {
                    return Ok(Expr::Assign {
                        name,
                        expression: Box::new(value),
                    })
                }
                _ => {
                    return Err(ParserError {
                        token: self.peek().clone(),
                        message: "Invalid assignment target.".to_string(),
                    })
                }
            };
        }
        Ok(expression)
    }

    fn parse_equality(&mut self) -> Result<Expr, ParserError> {
        let result_expr = self.parse_comparison();
        let mut expression = match result_expr {
            Ok(expr) => expr,
            e => return e,
        };
        while self.matches(&[TokenType::BangEqual, TokenType::EqualEqual]) {
            let previous_token = self.previous(); // We know this is now != or ==
            let operator = BinaryOperator::try_from(previous_token.token_type.clone()).unwrap();
            let right_expr_result = self.parse_comparison();
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

    fn parse_comparison(&mut self) -> Result<Expr, ParserError> {
        let result_expr = self.parse_term();
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
            let right_expr_result = self.parse_term();
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

    fn parse_term(&mut self) -> Result<Expr, ParserError> {
        let result_expr = self.parse_factor();
        let mut expression = match result_expr {
            Ok(expr) => expr,
            e => return e,
        };
        while self.matches(&[TokenType::Minus, TokenType::Plus]) {
            let previous_token = self.previous(); // We know this is now - or +
            let operator = BinaryOperator::try_from(previous_token.token_type.clone()).unwrap();
            let right_expr_result = self.parse_factor();
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

    fn parse_factor(&mut self) -> Result<Expr, ParserError> {
        let result_expr = self.parse_unary();
        let mut expression = match result_expr {
            Ok(expr) => expr,
            e => return e,
        };
        while self.matches(&[TokenType::Slash, TokenType::Star]) {
            let previous_token = self.previous(); // We know this is now / or *
            let operator = BinaryOperator::try_from(previous_token.token_type.clone()).unwrap();
            let right_expr_result = self.parse_unary();
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

    fn parse_unary(&mut self) -> Result<Expr, ParserError> {
        if self.matches(&[TokenType::Bang, TokenType::Minus]) {
            let previous_token = self.previous(); // We know this is now ! or -
            let unary_operator =
                UnaryOperator::try_from(previous_token.token_type.clone()).unwrap();
            let result_expression = self.parse_unary();
            match result_expression {
                Ok(expression) => {
                    return Ok(Expr::Unary {
                        operator: unary_operator,
                        expression: Box::new(expression),
                    });
                }
                Err(e) => return Err(e),
            }
        }
        self.parse_primary()
    }

    fn parse_primary(&mut self) -> Result<Expr, ParserError> {
        let expr = match self.peek().token_type.clone() {
            TokenType::False => Expr::Boolean(false),
            TokenType::True => Expr::Boolean(true),
            TokenType::Nil => Expr::Nil,
            TokenType::Number(x) => Expr::Number(x),
            TokenType::String(x) => Expr::String(x),
            TokenType::LeftParen => {
                self.advance();
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
            TokenType::Identifier(var_name) => Expr::Variable(var_name),
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
                    if self.is_at_end() {
                        return;
                    }
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
        let return_message = match token.token_type.clone() {
            TokenType::EOF => format!("{}, at end {}", line_number, message),
            t => format!("{:?}, at '{:?}' {}", t, line_number, message),
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
