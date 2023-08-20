use std::fmt::Display;

use super::{
    classes::{LoxClass, LoxInstance},
    functions::{LoxFunction, NativeFunction},
};

#[derive(Clone, Debug, PartialEq)]
pub enum ValueType {
    Number,
    String,
    Boolean,
    Nil,
    Class { name: String },
    Instance { class_name: String },
    Function { num_args: usize },
}

impl Display for ValueType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ValueType::Number => f.write_str("Number"),
            ValueType::String => f.write_str("String"),
            ValueType::Boolean => f.write_str("Boolean"),
            ValueType::Nil => f.write_str("Nil"),
            ValueType::Class { name } => f.write_str(&format!("Class<{}>", name)),
            ValueType::Instance { class_name } => f.write_str(&format!("Instance<{}>", class_name)),
            ValueType::Function { num_args } => {
                f.write_str(&format!("Function<{} args>", num_args))
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Number(f64),
    String(String),
    Boolean(bool),
    Nil,
    Class(LoxClass),
    Instance(LoxInstance),
    Function(LoxFunction),
    NativeFunction(NativeFunction),
}

impl Value {
    pub fn get_type(&self) -> ValueType {
        match self {
            Value::Number(_) => ValueType::Number,
            Value::String(_) => ValueType::String,
            Value::Boolean(_) => ValueType::Boolean,
            Value::Nil => ValueType::Nil,
            Value::Function(f) => ValueType::Function {
                num_args: f.function.params.len(),
            },
            Value::Class(class) => ValueType::Class {
                name: class.name.clone(),
            },
            Value::Instance(instance) => ValueType::Instance {
                class_name: instance.class.name.clone(),
            },
            Value::NativeFunction(f) => f.get_type(),
        }
    }

    pub fn is_truthy(&self) -> bool {
        !matches!(self, Self::Nil | Self::Boolean(false))
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Number(x) => {
                let x_s = x.to_string();
                if x_s.ends_with(".0") {
                    f.write_str(x_s[..x_s.len() - 2].into())
                } else {
                    f.write_str(&x_s)
                }
            }
            Value::String(x) => f.write_str(x),
            Value::Boolean(x) => f.write_str(&format!("{x:?}")),
            Value::Nil => f.write_str("nil"),
            Value::Function(LoxFunction {
                function,
                environment: _,
                locals: _,
                with_resolver: _,
                is_initializer: _,
            }) => f.write_str(&format!("{function:?}")),
            Value::Class(lox_class) => f.write_str(&format!("{lox_class}")),
            Value::Instance(instance) => f.write_str(&format!("{instance}")),
            Value::NativeFunction(func) => f.write_str(&format!("{func}")),
        }
    }
}
