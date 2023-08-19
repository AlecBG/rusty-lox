use crate::scanning::{Token, TokenType};
use std::convert::TryFrom;

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum BinaryOperator {
    BangEqual,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    Plus,
    Minus,
    Star,
    Slash,
}

impl TryFrom<TokenType> for BinaryOperator {
    type Error = ();

    fn try_from(value: TokenType) -> Result<Self, Self::Error> {
        match value {
            TokenType::BangEqual => Ok(BinaryOperator::BangEqual),
            TokenType::EqualEqual => Ok(BinaryOperator::EqualEqual),
            TokenType::Greater => Ok(BinaryOperator::Greater),
            TokenType::GreaterEqual => Ok(BinaryOperator::GreaterEqual),
            TokenType::Less => Ok(BinaryOperator::Less),
            TokenType::LessEqual => Ok(BinaryOperator::LessEqual),
            TokenType::Plus => Ok(BinaryOperator::Plus),
            TokenType::Minus => Ok(BinaryOperator::Minus),
            TokenType::Star => Ok(BinaryOperator::Star),
            TokenType::Slash => Ok(BinaryOperator::Slash),
            _ => Err(()),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum LogicalOperator {
    And,
    Or,
}

impl TryFrom<TokenType> for LogicalOperator {
    type Error = ();

    fn try_from(value: TokenType) -> Result<Self, Self::Error> {
        match value {
            TokenType::And => Ok(LogicalOperator::And),
            TokenType::Or => Ok(LogicalOperator::Or),
            _ => Err(()),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum UnaryOperator {
    Minus,
    Bang,
}

impl TryFrom<TokenType> for UnaryOperator {
    type Error = ();

    fn try_from(value: TokenType) -> Result<Self, Self::Error> {
        match value {
            TokenType::Minus => Ok(UnaryOperator::Minus),
            TokenType::Bang => Ok(UnaryOperator::Bang),
            _ => Err(()),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Assign {
        name: String,
        expression: Box<Expr>,
    },
    Binary {
        left: Box<Expr>,
        operator: BinaryOperator,
        right: Box<Expr>,
    },
    Call {
        callee: Box<Expr>,
        paren: Token, // Used for runtime errors that can locate the line of source code.
        arguments: Vec<Expr>,
    },
    Get {
        object: Box<Expr>,
        name: String,
    },
    Set {
        object: Box<Expr>,
        name: String,
        value: Box<Expr>,
    },
    LogicalOperator {
        left: Box<Expr>,
        operator: LogicalOperator,
        right: Box<Expr>,
    },
    Unary {
        operator: UnaryOperator,
        expression: Box<Expr>,
    },
    This(Token),
    Grouping(Box<Expr>),
    // Literals
    Number(f64),
    String(String),
    Boolean(bool),
    Nil,

    Variable(String),
}
