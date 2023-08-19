use std::fmt::Display;

#[derive(Clone, Debug, PartialEq)]
pub enum FunctionType {
    Function,
    Method,
    None, // Not a function
}

impl Display for FunctionType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Function => f.write_str("function"),
            Self::Method => f.write_str("method"),
            Self::None => f.write_str("NOT A FUNCTION"),
        }
    }
}
