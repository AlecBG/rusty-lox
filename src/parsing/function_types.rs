use std::fmt::Display;

#[derive(Debug)]
pub enum FunctionType {
    Function,
    Method,
}

impl Display for FunctionType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Function => f.write_str("function"),
            Self::Method => f.write_str("method"),
        }
    }
}
