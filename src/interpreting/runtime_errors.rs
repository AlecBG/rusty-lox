use std::{error::Error, fmt::Display};

use super::values::Value;

#[derive(Clone, Debug, PartialEq)]
pub struct RuntimeError {
    pub message: String,
}

impl Display for RuntimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&format!("Runtime error: {}", self.message))
    }
}

impl Error for RuntimeError {}

#[derive(Clone, Debug, PartialEq)]
pub enum RuntimeErrorOrReturnValue {
    RuntimeError(RuntimeError),
    ReturnValue(Value),
}

impl From<RuntimeError> for RuntimeErrorOrReturnValue {
    fn from(error: RuntimeError) -> Self {
        Self::RuntimeError(error)
    }
}
