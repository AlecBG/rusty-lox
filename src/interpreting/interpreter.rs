use crate::parsing::{BinaryOperator, Expr, LogicalOperator, Stmt, UnaryOperator};
use crate::scanning::Token;
use std::cmp::PartialEq;
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::error::Error;
use std::fmt::{Debug, Display};
use std::string::ToString;
use std::time::{SystemTime, UNIX_EPOCH};

#[derive(Debug)]
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
enum ValueType {
    Number,
    String,
    Boolean,
    Nil,
    Callable { num_args: usize },
}

#[derive(Clone)]
enum Value {
    Number(f64),
    String(String),
    Boolean(bool),
    Nil,
    Callable {
        f: fn(&mut Interpreter, Vec<Value>) -> Value,
        num_args: usize,
    },
}

impl Value {
    fn get_type(&self) -> ValueType {
        match self {
            Value::Number(_) => ValueType::Number,
            Value::String(_) => ValueType::String,
            Value::Boolean(_) => ValueType::Boolean,
            Value::Nil => ValueType::Nil,
            Value::Callable { f: _, num_args } => ValueType::Callable {
                num_args: *num_args,
            },
        }
    }

    fn is_truthy(&self) -> bool {
        !matches!(self, Self::Nil | Self::Boolean(false))
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Number(l0), Self::Number(r0)) => l0 == r0,
            (Self::String(l0), Self::String(r0)) => l0 == r0,
            (Self::Boolean(l0), Self::Boolean(r0)) => l0 == r0,
            // todo: Allow better comparison of functions
            (Self::Callable { f: _, num_args: _ }, Self::Callable { f: _, num_args: _ }) => false,
            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
        }
    }
}

impl Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Number(arg0) => f.debug_tuple("Number").field(arg0).finish(),
            Self::String(arg0) => f.debug_tuple("String").field(arg0).finish(),
            Self::Boolean(arg0) => f.debug_tuple("Boolean").field(arg0).finish(),
            Self::Nil => write!(f, "Nil"),
            Self::Callable { f: _, num_args } => f.debug_tuple("Callable").field(num_args).finish(),
        }
    }
}

impl ToString for Value {
    fn to_string(&self) -> String {
        match self {
            Value::Number(x) => {
                let x_s = x.to_string();
                if x_s.ends_with(".0") {
                    x_s[..x_s.len() - 2].to_string()
                } else {
                    x_s
                }
            }
            Value::String(x) => x.to_string(),
            Value::Boolean(x) => x.to_string(),
            Value::Nil => "nil".to_string(),
            Value::Callable { f: _, num_args } => format!("Function {num_args} args"),
        }
    }
}

fn construct_global_values() -> HashMap<String, Value> {
    // fn clock_function(_: &mut Interr)

    let clock = Value::Callable {
        f: |_: &mut Interpreter, _: Vec<Value>| {
            Value::Number(
                SystemTime::now()
                    .duration_since(UNIX_EPOCH)
                    .unwrap()
                    .as_secs_f64(),
            )
        },
        num_args: 0,
    };
    HashMap::from([("clock".to_string(), clock)])
}

#[derive(Debug)]
pub struct Environment {
    enclosing_environment: Option<Box<Environment>>,
    values: HashMap<String, Value>,
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            enclosing_environment: None,
            values: construct_global_values(),
        }
    }

    fn create_enclosed_environment(self) -> Environment {
        Environment {
            enclosing_environment: Some(Box::new(self)),
            values: HashMap::new(),
        }
    }

    fn define(&mut self, name: String, value: Value) {
        self.values.insert(name, value);
    }

    fn assign(&mut self, name: String, value: Value) -> Result<(), RuntimeError> {
        if let Entry::Occupied(mut e) = self.values.entry(name.clone()) {
            e.insert(value);
            Ok(())
        } else {
            match &mut self.enclosing_environment {
                Some(environment) => environment.assign(name, value),
                None => Err(RuntimeError {
                    message: format!("Undefined variable '{name}'."),
                }),
            }
        }
    }

    fn get(&self, name: &str) -> Result<Value, RuntimeError> {
        match self.values.get(name) {
            Some(v) => Ok(v.clone()),
            None => match &self.enclosing_environment {
                Some(environment) => environment.get(name),
                None => Err(RuntimeError {
                    message: format!("Undefined variable '{name}'."),
                }),
            },
        }
    }
}

impl Default for Environment {
    fn default() -> Self {
        Self::new()
    }
}

pub fn interpret(statements: Vec<Stmt>) -> Result<(), RuntimeError> {
    match interpret_with_optional_environment(statements, None) {
        Ok(_) => Ok(()),
        Err(err) => Err(err),
    }
}

pub fn interpret_with_environment(
    statements: Vec<Stmt>,
    environment: Environment,
) -> Result<Environment, (Environment, RuntimeError)> {
    let mut interpreter = Interpreter::new(Some(environment));
    for statement in statements {
        let res = interpreter.execute(statement);
        interpreter = match res {
            Ok(i) => i,
            Err((i, e)) => return Err((i.environment, e)),
        };
    }
    Ok(interpreter.environment)
}

fn interpret_with_optional_environment(
    statements: Vec<Stmt>,
    optional_environment: Option<Environment>,
) -> Result<Option<Environment>, RuntimeError> {
    let mut interpreter = Interpreter::new(optional_environment);
    for statement in statements {
        let res = interpreter.execute(statement);
        interpreter = match res {
            Ok(i) => i,
            Err((_, e)) => return Err(e),
        };
    }
    match interpreter.environment.enclosing_environment {
        Some(env) => Ok(Some(*env)),
        None => Ok(None),
    }
}

struct Interpreter {
    environment: Environment,
}

impl Interpreter {
    pub fn new(optional_environment: Option<Environment>) -> Self {
        match optional_environment {
            Some(environment) => Self {
                environment: Environment::create_enclosed_environment(environment),
            },
            None => Interpreter {
                environment: Environment::new(),
            },
        }
    }

    fn execute(mut self, statement: Stmt) -> Result<Interpreter, (Interpreter, RuntimeError)> {
        match statement {
            Stmt::Block(statements) => {
                match interpret_with_optional_environment(statements, Some(self.environment)) {
                    Ok(optional_environment) => Ok(Interpreter::new(optional_environment)),
                    // TODO: Figure out how to get a prompt environment to have a block statement throw a runtime error and not reset the state.
                    Err(err) => Err((Interpreter::new(None), err)),
                }
            }
            Stmt::If {
                condition,
                then_stmt,
                else_stmt,
            } => {
                let condition_value_result = self.evaluate(condition);
                let condition_value = match condition_value_result {
                    Ok(value) => value,
                    Err(err) => return Err((self, err)),
                };
                let interpreter = if condition_value.is_truthy() {
                    match interpret_with_optional_environment(
                        vec![*then_stmt],
                        Some(self.environment),
                    ) {
                        Ok(optional_environment) => Interpreter::new(optional_environment),
                        // TODO: Figure out how to get a prompt environment to have a block statement throw a runtime error and not reset the state.
                        Err(err) => return Err((Interpreter::new(None), err)),
                    }
                } else {
                    match else_stmt {
                        Some(stmt) => match interpret_with_optional_environment(
                            vec![*stmt],
                            Some(self.environment),
                        ) {
                            Ok(optional_environment) => Interpreter::new(optional_environment),
                            // TODO: Figure out how to get a prompt environment to have a block statement throw a runtime error and not reset the state.
                            Err(err) => return Err((Interpreter::new(None), err)),
                        },
                        None => self,
                    }
                };

                Ok(interpreter)
            }
            Stmt::Print(expr) => {
                let value_result = self.evaluate(expr);
                let value = match value_result {
                    Ok(v) => v,
                    Err(err) => return Err((self, err)),
                };
                println!("{}", value.to_string());
                Ok(self)
            }
            Stmt::Expression(expr) => {
                let value_result = self.evaluate(expr);
                match value_result {
                    Ok(_) => Ok(self),
                    Err(err) => Err((self, err)),
                }
            }
            Stmt::Var { name, initializer } => {
                let value = match self.evaluate(initializer) {
                    Ok(v) => v,
                    Err(err) => return Err((self, err)),
                };
                self.environment.define(name, value);
                Ok(self)
            }
            Stmt::While { condition, body } => {
                let mut interpreter = self;
                // let mut condition_c: Expr = condition.clone();
                while {
                    match interpreter.evaluate(condition.clone()) {
                        Ok(value) => value.is_truthy(),
                        Err(err) => return Err((Interpreter::new(None), err)),
                    }
                } {
                    interpreter = match interpret_with_optional_environment(
                        vec![*body.clone()],
                        Some(interpreter.environment),
                    ) {
                        Ok(optional_environment) => Interpreter::new(optional_environment),
                        // TODO: Figure out how to get a prompt environment to have a block statement throw a runtime error and not reset the state.
                        Err(err) => return Err((Interpreter::new(None), err)),
                    }
                }
                Ok(interpreter)
            }
        }
    }

    fn evaluate(&mut self, expression: Expr) -> Result<Value, RuntimeError> {
        match expression {
            Expr::Assign { name, expression } => {
                let value = self.evaluate(*expression)?;
                self.environment.assign(name, value.clone())?;
                Ok(value)
            }
            Expr::Binary {
                left,
                operator,
                right,
            } => self.evaluate_binary_expression(*left, *right, operator),
            Expr::Call {
                callee,
                paren,
                arguments,
            } => self.evaluate_call_expression(*callee, paren, arguments),
            Expr::LogicalOperator {
                left,
                operator,
                right,
            } => self.evaluate_logical_operator_expression(*left, *right, operator),
            Expr::Unary {
                operator,
                expression,
            } => self.evaluate_unary_expression(*expression, operator),
            Expr::Grouping(expression) => self.evaluate(*expression),
            Expr::Number(x) => Ok(Value::Number(x)),
            Expr::String(x) => Ok(Value::String(x)),
            Expr::Boolean(x) => Ok(Value::Boolean(x)),
            Expr::Nil => Ok(Value::Nil),
            Expr::Variable(name) => self.environment.get(&name),
        }
    }

    fn evaluate_binary_expression(
        &mut self,
        left_expression: Expr,
        right_expression: Expr,
        operator: BinaryOperator,
    ) -> Result<Value, RuntimeError> {
        let left_value_result = self.evaluate(left_expression);
        let left_value = match left_value_result {
            Ok(v) => v,
            Err(e) => return Err(e),
        };
        let right_value_result = self.evaluate(right_expression);
        let right_value = match right_value_result {
            Ok(v) => v,
            Err(e) => return Err(e),
        };
        match operator {
            BinaryOperator::BangEqual => {
                if left_value == right_value {
                    Ok(Value::Boolean(false))
                } else {
                    Ok(Value::Boolean(true))
                }
            }
            BinaryOperator::EqualEqual => {
                if left_value == right_value {
                    Ok(Value::Boolean(true))
                } else {
                    Ok(Value::Boolean(false))
                }
            }
            BinaryOperator::Greater => match (left_value, right_value) {
                (Value::Number(l), Value::Number(r)) => {
                    if l > r {
                        Ok(Value::Boolean(true))
                    } else {
                        Ok(Value::Boolean(false))
                    }
                }
                (lv, rv) => Err(RuntimeError {
                    message: format!("Cannot compare {:?} and {:?}", lv.get_type(), rv.get_type()),
                }),
            },
            BinaryOperator::GreaterEqual => match (left_value, right_value) {
                (Value::Number(l), Value::Number(r)) => {
                    if l >= r {
                        Ok(Value::Boolean(true))
                    } else {
                        Ok(Value::Boolean(false))
                    }
                }
                (lv, rv) => Err(RuntimeError {
                    message: format!("Cannot compare {:?} and {:?}", lv.get_type(), rv.get_type()),
                }),
            },
            BinaryOperator::Less => match (left_value, right_value) {
                (Value::Number(l), Value::Number(r)) => {
                    if l < r {
                        Ok(Value::Boolean(true))
                    } else {
                        Ok(Value::Boolean(false))
                    }
                }
                (lv, rv) => Err(RuntimeError {
                    message: format!("Cannot compare {:?} and {:?}", lv.get_type(), rv.get_type()),
                }),
            },
            BinaryOperator::LessEqual => match (left_value, right_value) {
                (Value::Number(l), Value::Number(r)) => {
                    if l <= r {
                        Ok(Value::Boolean(true))
                    } else {
                        Ok(Value::Boolean(false))
                    }
                }
                (lv, rv) => Err(RuntimeError {
                    message: format!("Cannot compare {:?} and {:?}", lv.get_type(), rv.get_type()),
                }),
            },
            BinaryOperator::Plus => match (left_value, right_value) {
                (Value::Number(l), Value::Number(r)) => Ok(Value::Number(l + r)),
                (Value::String(l), Value::String(r)) => Ok(Value::String(format!("{l}{r}"))),
                (lv, rv) => Err(RuntimeError {
                    message: format!("Cannot add {:?} and {:?}", lv.get_type(), rv.get_type()),
                }),
            },
            BinaryOperator::Minus => match (left_value, right_value) {
                (Value::Number(l), Value::Number(r)) => Ok(Value::Number(l - r)),
                (lv, rv) => Err(RuntimeError {
                    message: format!(
                        "Cannot subtract {:?} from {:?}",
                        rv.get_type(),
                        lv.get_type()
                    ),
                }),
            },
            BinaryOperator::Star => match (left_value, right_value) {
                (Value::Number(l), Value::Number(r)) => Ok(Value::Number(l * r)),
                (lv, rv) => Err(RuntimeError {
                    message: format!(
                        "Cannot multiply {:?} and {:?}",
                        lv.get_type(),
                        rv.get_type()
                    ),
                }),
            },
            BinaryOperator::Slash => match (left_value, right_value) {
                (Value::Number(l), Value::Number(r)) => Ok(Value::Number(l / r)),
                (lv, rv) => Err(RuntimeError {
                    message: format!("Cannot divide {:?} by {:?}", lv.get_type(), rv.get_type()),
                }),
            },
        }
    }

    fn evaluate_call_expression(
        &mut self,
        callee_expr: Expr,
        paren: Token,
        arguments: Vec<Expr>,
    ) -> Result<Value, RuntimeError> {
        let callee_value = self.evaluate(callee_expr)?;
        let mut args = vec![];
        for arg_expr in arguments {
            args.push(self.evaluate(arg_expr)?);
        }

        let Value::Callable { f, num_args } = callee_value else {
                             return Err(RuntimeError {
                                 message: format!("Can only call functions and classes at {paren:?}"),
                            })
                       };
        let arg_length = args.len();
        if arg_length != num_args {
            return Err(RuntimeError {
                message: format!("Expected {num_args} arguments, but received {arg_length}."),
            });
        }
        Ok(f(self, args))
    }

    fn evaluate_logical_operator_expression(
        &mut self,
        left_expression: Expr,
        right_expression: Expr,
        operator: LogicalOperator,
    ) -> Result<Value, RuntimeError> {
        let left_value = self.evaluate(left_expression)?;
        match operator {
            LogicalOperator::And => {
                if left_value.is_truthy() {
                    self.evaluate(right_expression)
                } else {
                    Ok(Value::Boolean(false))
                }
            }
            LogicalOperator::Or => {
                if left_value.is_truthy() {
                    self.evaluate(right_expression)
                } else {
                    Ok(left_value)
                }
            }
        }
    }

    fn evaluate_unary_expression(
        &mut self,
        expression: Expr,
        operator: UnaryOperator,
    ) -> Result<Value, RuntimeError> {
        let value = self.evaluate(expression)?;
        match operator {
            UnaryOperator::Bang => {
                if value == Value::Nil || value == Value::Boolean(false) {
                    Ok(Value::Boolean(true))
                } else {
                    Ok(Value::Boolean(false))
                }
            }
            UnaryOperator::Minus => match value {
                Value::Number(x) => Ok(Value::Number(x)),
                v => Err(RuntimeError {
                    message: format!("Cannot take minus of {:?}", v.get_type()),
                }),
            },
        }
    }
}
