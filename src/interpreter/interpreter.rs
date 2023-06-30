use crate::parser::{BinaryOperator, Expr, Stmt, UnaryOperator};
use std::string::ToString;

#[derive(Debug)]
pub struct RuntimeError {
    message: String,
}

#[derive(Debug, PartialEq)]
enum ValueType {
    Number,
    String,
    Boolean,
    Nil,
}

#[derive(Debug, PartialEq)]
enum Value {
    Number(f64),
    String(String),
    Boolean(bool),
    Nil,
}

impl Value {
    fn get_type(&self) -> ValueType {
        match self {
            Value::Number(_) => ValueType::Number,
            Value::String(_) => ValueType::String,
            Value::Boolean(_) => ValueType::Boolean,
            Value::Nil => ValueType::Nil,
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
        }
    }
}

pub struct Interpreter {}

impl Interpreter {
    pub fn new() -> Self {
        Self {}
    }

    pub fn interpret(&mut self, statements: Vec<Stmt>) -> Result<(), RuntimeError> {
        for statement in statements {
            let res = self.execute(statement);
            match res {
                Ok(_) => {}
                Err(e) => return Err(e),
            };
        }
        Ok(())
    }

    fn execute(&mut self, statement: Stmt) -> Result<(), RuntimeError> {
        match statement {
            Stmt::Print(expr) => {
                let value_result = self.evaluate(expr);
                let value = match value_result {
                    Ok(v) => v,
                    Err(err) => return Err(err),
                };
                println!("{}", value.to_string());
                Ok(())
            }
            Stmt::Expression(expr) => {
                let value_result = self.evaluate(expr);
                match value_result {
                    Ok(_) => Ok(()),
                    Err(err) => return Err(err),
                }
            }
        }
    }

    /// Later will have some global state that will be mutated, hence has mutable reference to self.
    fn evaluate(&mut self, expression: Expr) -> Result<Value, RuntimeError> {
        match expression {
            Expr::Binary {
                left,
                operator,
                right,
            } => self.evaluate_binary_expression(*left, *right, operator),
            Expr::Unary {
                operator,
                expression,
            } => self.evaluate_unary_expression(*expression, operator),
            Expr::Grouping(expression) => self.evaluate(*expression),
            Expr::Number(x) => Ok(Value::Number(x)),
            Expr::String(x) => Ok(Value::String(x)),
            Expr::Boolean(x) => Ok(Value::Boolean(x)),
            Expr::Nil => Ok(Value::Nil),
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
                (Value::String(l), Value::String(r)) => Ok(Value::String(format!("{}{}", l, r))),
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

    fn evaluate_unary_expression(
        &mut self,
        expression: Expr,
        operator: UnaryOperator,
    ) -> Result<Value, RuntimeError> {
        let value_result = self.evaluate(expression);
        let value = match value_result {
            Ok(v) => v,
            Err(e) => return Err(e),
        };
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
