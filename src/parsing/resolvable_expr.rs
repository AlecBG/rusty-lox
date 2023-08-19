use std::{error::Error, fmt::Display};

use super::Expr;

#[derive(Debug)]
pub struct ResolutionError {}

impl Display for ResolutionError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("Tried to resolve an expression that was not a variable")
    }
}

impl Error for ResolutionError {}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum ResolvableExpr {
    Variable(String),
}

impl TryFrom<Expr> for ResolvableExpr {
    type Error = ResolutionError;

    fn try_from(value: Expr) -> Result<Self, Self::Error> {
        match value {
            Expr::Variable(x) => Ok(Self::Variable(x)),
            _ => Err(ResolutionError {}),
        }
    }
}

impl Display for ResolvableExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Variable(name) => f.write_str(name),
        }
    }
}
