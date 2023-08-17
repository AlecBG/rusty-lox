use std::convert::TryFrom;

use crate::scanning::Token;
use crate::scanning::TokenType;

use super::expression::{BinaryOperator, Expr, UnaryOperator};
use super::function_types::FunctionType;
use super::statement::Stmt;
use super::BlockStatement;
use super::ClassStatement;
use super::FunctionStatement;
use super::IfStatement;
use super::LogicalOperator;
use super::ParserError;
use super::ParserErrors;
use super::ReturnStatement;
use super::VariableDeclaration;
use super::WhileStatement;

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
        let mut parser_errors_line: Vec<ParserError> = vec![];
        while !self.is_at_end() {
            match self.parse_declaration() {
                Ok(stmt) => statements.push(stmt),
                Err(error) => {
                    parser_errors_line.push(error);
                }
            }
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
        let result = if self.matches(&[TokenType::Class]) {
            self.parse_class_declaration().map(Stmt::Class)
        } else if self.matches(&[TokenType::Fun]) {
            self.parse_function_declaration(FunctionType::Function)
                .map(Stmt::Function)
        } else if self.matches(&[TokenType::Var]) {
            self.parse_variable_declaration().map(Stmt::Var)
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

    fn parse_class_declaration(&mut self) -> Result<ClassStatement, ParserError> {
        let name = match self.peek().token_type.clone() {
            TokenType::Identifier(n) => n.clone(),
            _ => return Err(Parser::error(self.peek(), "Expect function name")),
        };
        self.advance();
        self.consume(TokenType::LeftBrace, "Expect '{' after class name")?;
        let mut methods: Vec<FunctionStatement> = vec![];
        while !self.check(&TokenType::RightBrace) && !self.is_at_end() {
            methods.push(self.parse_function_declaration(FunctionType::Method)?);
        }

        self.consume(TokenType::RightBrace, "Expect '}' after class body")?;

        Ok(ClassStatement { name, methods })
    }

    fn parse_function_declaration(
        &mut self,
        function_type: FunctionType,
    ) -> Result<FunctionStatement, ParserError> {
        let name = match self.peek().token_type.clone() {
            TokenType::Identifier(name) => name,
            _ => {
                return Err(Parser::error(
                    self.peek(),
                    &format!("Expect {function_type} name"),
                ))
            }
        };
        self.advance();
        self.consume(
            TokenType::LeftParen,
            &format!("Expect '(' after {function_type} name"),
        )?;
        let mut params: Vec<String> = vec![];
        if self.peek().token_type != TokenType::RightParen {
            loop {
                match self.peek().token_type.clone() {
                    TokenType::Identifier(name) => {
                        params.push(name);
                        if params.len() > 255 {
                            return Err(Self::error(
                                self.peek(),
                                &format!("Cannot have more than 255 parameters in a {function_type} declaration"),
                            ));
                        }
                        self.advance();
                        match self.peek().token_type.clone() {
                            TokenType::Comma => self.advance(),
                            TokenType::RightParen => break,
                            _ => return Err(Self::error(
                                self.peek(),
                                &format!("Expect ')' or ',' after parameter in {function_type} signature"),
                            )),
                        }
                    }
                    _ => return Err(Parser::error(self.peek(), "Expected parameter name")),
                };
            }
        }
        self.consume(TokenType::RightParen, "Expect ')' after parameters")?;
        self.consume(
            TokenType::LeftBrace,
            &format!("Expect '{{' before {function_type} body"),
        )?;
        let block_stmt = self.parse_block_statement()?;
        Ok(FunctionStatement {
            name,
            params,
            body: block_stmt.0,
        })
    }

    fn parse_variable_declaration(&mut self) -> Result<VariableDeclaration, ParserError> {
        let token = self.peek().token_type.clone();
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
                self.consume(
                    TokenType::Semicolon,
                    "Expect ';' after variable declaration",
                )?;
                Ok(VariableDeclaration { name, initializer })
            }
            _ => Err(Parser::error(self.peek(), "Expect variable name")),
        }
    }

    fn parse_statement(&mut self) -> Result<Stmt, ParserError> {
        if self.matches(&[TokenType::For]) {
            self.parse_for_statement()
        } else if self.matches(&[TokenType::If]) {
            self.parse_if_statement().map(Stmt::If)
        } else if self.matches(&[TokenType::Print]) {
            self.parse_print_statement()
        } else if self.matches(&[TokenType::Return]) {
            self.parse_return_statement().map(Stmt::Return)
        } else if self.matches(&[TokenType::While]) {
            self.parse_while_statement().map(Stmt::While)
        } else if self.matches(&[TokenType::LeftBrace]) {
            self.parse_block_statement().map(Stmt::Block)
        } else {
            self.parse_expression_statement()
        }
    }

    fn parse_for_statement(&mut self) -> Result<Stmt, ParserError> {
        self.consume(TokenType::LeftParen, "Expect '(' after for.")?;
        let initializer = if self.matches(&[TokenType::Semicolon]) {
            None
        } else if self.matches(&[TokenType::Var]) {
            let stmt = match self.parse_variable_declaration() {
                Ok(s) => Stmt::Var(s),
                Err(err) => return Err(err),
            };
            Some(stmt)
        } else {
            let stmt = match self.parse_expression_statement() {
                Ok(s) => s,
                Err(err) => return Err(err),
            };
            Some(stmt)
        };

        let condition = if self.matches(&[TokenType::Semicolon]) {
            None
        } else {
            let expr = match self.parse_expression() {
                Ok(expr) => expr,
                Err(err) => return Err(err),
            };
            Some(expr)
        };
        self.consume(TokenType::Semicolon, "Expect ';' after loop condition.")?;

        let increment = if self.matches(&[TokenType::RightParen]) {
            None
        } else {
            let expr = match self.parse_expression() {
                Ok(expr) => expr,
                Err(err) => return Err(err),
            };
            Some(expr)
        };
        self.consume(TokenType::RightParen, "Expect ')' after for clauses")?;

        let mut body = match self.parse_statement() {
            Ok(s) => s,
            Err(err) => return Err(err),
        };

        body = match increment {
            Some(expr) => Stmt::Block(BlockStatement(vec![body, Stmt::Expression(expr)])),
            None => body,
        };
        body = match condition {
            Some(expr) => Stmt::While(WhileStatement {
                condition: expr,
                body: Box::new(body),
            }),
            None => Stmt::While(WhileStatement {
                condition: Expr::Boolean(true),
                body: Box::new(body),
            }),
        };
        body = match initializer {
            Some(stmt) => Stmt::Block(BlockStatement(vec![stmt, body])),
            None => body,
        };
        Ok(body)
    }

    fn parse_if_statement(&mut self) -> Result<IfStatement, ParserError> {
        self.consume(TokenType::LeftParen, "Expect '(' after if.")?;
        let condition = match self.parse_expression() {
            Ok(expr) => expr,
            Err(err) => return Err(err),
        };
        self.consume(TokenType::RightParen, "Expect ')' after if condition.")?;
        let then_stmt = match self.parse_statement() {
            Ok(stmt) => Box::new(stmt),
            Err(err) => return Err(err),
        };
        let else_stmt = if self.matches(&[TokenType::Else]) {
            match self.parse_statement() {
                Ok(stmt) => Some(Box::new(stmt)),
                Err(err) => return Err(err),
            }
        } else {
            None
        };
        Ok(IfStatement {
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
        self.consume(TokenType::Semicolon, "Expect ';' after value.")?;

        Ok(Stmt::Print(value))
    }

    fn parse_return_statement(&mut self) -> Result<ReturnStatement, ParserError> {
        let return_token = self.previous().clone();
        let value = if self.check(&TokenType::Semicolon) {
            Expr::Nil
        } else {
            self.parse_expression()?
        };
        self.consume(TokenType::Semicolon, "Expect ';' after return value.")?;
        Ok(ReturnStatement {
            return_token,
            value,
        })
    }

    fn parse_while_statement(&mut self) -> Result<WhileStatement, ParserError> {
        self.consume(TokenType::LeftParen, "Expect '(' after 'while'.")?;
        let condition_result = self.parse_expression();
        let condition = match condition_result {
            Ok(expr) => expr,
            Err(err) => return Err(err),
        };
        self.consume(TokenType::RightParen, "Expect ')' after condition.")?;
        let body_result = self.parse_statement();
        let body = match body_result {
            Ok(stmt) => stmt,
            Err(err) => return Err(err),
        };
        Ok(WhileStatement {
            condition,
            body: Box::new(body),
        })
    }

    fn parse_block_statement(&mut self) -> Result<BlockStatement, ParserError> {
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
        self.consume(TokenType::RightBrace, "Expect '}' after block.")?;
        Ok(BlockStatement(statements))
    }

    fn parse_expression_statement(&mut self) -> Result<Stmt, ParserError> {
        let value_result = self.parse_expression();
        let value = match value_result {
            Ok(expr) => expr,
            Err(err) => return Err(err),
        };
        self.consume(TokenType::Semicolon, "Expect ';' after expression.")?;
        Ok(Stmt::Expression(value))
    }

    fn parse_expression(&mut self) -> Result<Expr, ParserError> {
        self.parse_assignment()
    }

    fn parse_assignment(&mut self) -> Result<Expr, ParserError> {
        let expression = match self.parse_or() {
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

    fn parse_or(&mut self) -> Result<Expr, ParserError> {
        let mut expression = match self.parse_and() {
            Ok(e) => e,
            Err(err) => return Err(err),
        };
        if self.matches(&[TokenType::Or]) {
            let previous_token = self.previous(); // We know this is now or
            let operator = LogicalOperator::try_from(previous_token.token_type.clone()).unwrap();
            let right_result = self.parse_and();
            let right = match right_result {
                Ok(expr) => expr,
                Err(err) => return Err(err),
            };
            expression = Expr::LogicalOperator {
                left: Box::new(expression),
                operator,
                right: Box::new(right),
            }
        }
        Ok(expression)
    }

    fn parse_and(&mut self) -> Result<Expr, ParserError> {
        let mut expression = match self.parse_equality() {
            Ok(e) => e,
            Err(err) => return Err(err),
        };
        if self.matches(&[TokenType::And]) {
            let previous_token = self.previous(); // We know this is now and
            let operator = LogicalOperator::try_from(previous_token.token_type.clone()).unwrap();
            let right_result = self.parse_equality();
            let right = match right_result {
                Ok(expr) => expr,
                Err(err) => return Err(err),
            };
            expression = Expr::LogicalOperator {
                left: Box::new(expression),
                operator,
                right: Box::new(right),
            }
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
        self.parse_call()
    }

    fn parse_call(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.parse_primary()?;
        loop {
            if self.matches(&[TokenType::LeftParen]) {
                expr = self.finish_call(expr)?;
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn finish_call(&mut self, callee: Expr) -> Result<Expr, ParserError> {
        let mut arguments: Vec<Expr> = vec![];
        if self.peek().token_type != TokenType::RightParen {
            loop {
                let expr = self.parse_expression()?;
                arguments.push(expr);
                if arguments.len() > 255 {
                    return Err(Self::error(
                        self.peek(),
                        "Cannot have more than 255 arguments in a function call",
                    ));
                }
                match self.peek().token_type.clone() {
                    TokenType::Comma => self.advance(),
                    TokenType::RightParen => break,
                    _ => {
                        return Err(Self::error(
                            self.peek(),
                            "Expect ')' or ',' after argument in function call",
                        ))
                    }
                };
            }
        }
        let paren = self.consume(TokenType::RightParen, "Expect ')' after arguments.")?;
        Ok(Expr::Call {
            callee: Box::new(callee),
            paren: paren.clone(),
            arguments,
        })
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
                        self.consume(TokenType::RightParen, "Expect ')' after expression.")?;
                        Expr::Grouping(Box::new(expr))
                    }
                    e => return e,
                }
            }
            TokenType::Identifier(var_name) => Expr::Variable(var_name),
            unexpected_type => {
                return Err(Parser::error(
                    self.peek(),
                    &format!("Expect expression, found type {unexpected_type:?}."),
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
        let message = match token.token_type.clone() {
            TokenType::EOF => format!("{line_number}, at end {message}"),
            t => format!("{t:?}, at '{line_number}' {message}"),
        };
        ParserError {
            token: token.clone(),
            message,
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
