use std::{
    cell::RefCell,
    fmt::Display,
    rc::Rc,
    time::{SystemTime, UNIX_EPOCH},
};

use crate::parsing::FunctionStatement;

use super::{
    classes::LoxInstance,
    environment::Environment,
    runtime_errors::{RuntimeError, RuntimeErrorOrReturnValue},
    values::{Value, ValueType},
    Interpreter,
};

pub trait LoxCallable {
    fn call(
        &mut self,
        arguments: Vec<Value>,
    ) -> Result<Rc<RefCell<Value>>, RuntimeErrorOrReturnValue>;
}

#[derive(Clone, Debug, PartialEq)]
pub enum NativeFunction {
    Clock,
}

impl NativeFunction {
    pub fn get_type(&self) -> ValueType {
        match self {
            Self::Clock => ValueType::Function { num_args: 0 },
        }
    }
}

impl Display for NativeFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Clock => f.write_str("Native function: clock"),
        }
    }
}

impl LoxCallable for NativeFunction {
    fn call(
        &mut self,
        arguments: Vec<Value>,
    ) -> Result<Rc<RefCell<Value>>, RuntimeErrorOrReturnValue> {
        match self {
            Self::Clock => {
                if !arguments.is_empty() {
                    return Err(RuntimeError {
                        message: "clock expects zero arguments".to_string(),
                    }
                    .into());
                }
                Ok(Rc::new(RefCell::new(Value::Number(
                    SystemTime::now()
                        .duration_since(UNIX_EPOCH)
                        .unwrap()
                        .as_secs_f64(),
                ))))
            }
        }
    }
}

#[derive(Clone, Debug)]
pub struct LoxFunction {
    pub function: FunctionStatement,
    pub environment: Box<dyn Environment>,
    pub with_resolver: bool,
    pub is_initializer: bool,
}

impl LoxFunction {
    pub fn bind(&self, instance: LoxInstance) -> Self {
        let mut environment = self.environment.clone();
        environment.define("this".to_string(), Value::Instance(instance));
        Self {
            function: self.function.clone(),
            environment,
            with_resolver: self.with_resolver,
            is_initializer: true,
        }
    }
}

impl LoxCallable for LoxFunction {
    fn call(
        &mut self,
        arguments: Vec<Value>,
    ) -> Result<Rc<RefCell<Value>>, RuntimeErrorOrReturnValue> {
        if arguments.len() != self.function.params.len() {
            return Err(RuntimeError {
                message: format!(
                    "{} expects {} arguments and received {}",
                    self.function.name,
                    self.function.params.len(),
                    arguments.len()
                ),
            }
            .into());
        }
        self.environment.push();
        self.environment
            .define(self.function.name.clone(), Value::Function(self.clone()));
        for (arg, param) in arguments.into_iter().zip(self.function.params.iter()) {
            self.environment.define(param.clone(), arg);
        }
        let mut interpreter: Interpreter =
            Interpreter::new_without_resolver(self.environment.clone());

        for stmt in &self.function.body {
            match interpreter.execute(stmt.clone()) {
                Ok(_) => {}
                Err(err) => match err {
                    RuntimeErrorOrReturnValue::RuntimeError(e) => {
                        self.environment.pop();
                        return Err(RuntimeErrorOrReturnValue::RuntimeError(e));
                    }
                    RuntimeErrorOrReturnValue::ReturnValue(v) => {
                        self.environment.pop();
                        return Ok(Rc::new(RefCell::new(v)));
                    }
                },
            };
        }
        self.environment.pop();
        if self.is_initializer {
            self.environment
                .get_at(&(self.environment.get_depth() - 1), "this")
        } else {
            Ok(Rc::new(RefCell::new(Value::Nil)))
        }
    }
}

impl Display for LoxFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("LoxFunction")
    }
}

impl PartialEq<LoxFunction> for LoxFunction {
    /// Ignore the equality on the environment in the equality (it's a bit of a pain to
    /// implement PartialEq for Environment due to the object safety rules)
    fn eq(&self, other: &LoxFunction) -> bool {
        self.function == other.function
    }
}
