use crate::scanner::TokenType;
use std::convert::TryFrom;

#[derive(Debug)]
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

#[derive(Debug)]
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

#[derive(Debug)]
pub enum Expr {
    Binary {
        left: Box<Expr>,
        operator: BinaryOperator,
        right: Box<Expr>,
    },
    Unary {
        operator: UnaryOperator,
        expression: Box<Expr>,
    },
    Grouping(Box<Expr>),
    // Literals
    Number(f64),
    String(String),
    Boolean(bool),
    Nil,
}