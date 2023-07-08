use crate::parsing::{
    BinaryOperator, Expr, FunctionStatement, IfStatement, LogicalOperator, Stmt, UnaryOperator,
    VariableDeclaration, WhileStatement,
};
use crate::scanning::Token;
use std::cell::RefCell;
use std::cmp::PartialEq;
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::error::Error;
use std::fmt::{Debug, Display};
use std::rc::Rc;
use std::string::ToString;
use std::time::{SystemTime, UNIX_EPOCH};

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
enum RuntimeErrorOrReturnValue {
    RuntimeError(RuntimeError),
    ReturnValue(Value),
}

impl From<RuntimeError> for RuntimeErrorOrReturnValue {
    fn from(error: RuntimeError) -> Self {
        Self::RuntimeError(error)
    }
}

trait LoxCallable {
    fn call(&mut self, arguments: Vec<Value>) -> Result<Value, RuntimeErrorOrReturnValue>;
}

#[derive(Clone, Debug, PartialEq)]
enum NativeFunction {
    Clock,
}

impl NativeFunction {
    fn get_type(&self) -> ValueType {
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
    fn call(&mut self, arguments: Vec<Value>) -> Result<Value, RuntimeErrorOrReturnValue> {
        match self {
            Self::Clock => {
                if !arguments.is_empty() {
                    return Err(RuntimeError {
                        message: "clock expects zero arguments".to_string(),
                    }
                    .into());
                }
                Ok(Value::Number(
                    SystemTime::now()
                        .duration_since(UNIX_EPOCH)
                        .unwrap()
                        .as_secs_f64(),
                ))
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
struct LoxFunction {
    function: FunctionStatement,
    environment: Environment,
}

impl LoxCallable for LoxFunction {
    fn call(&mut self, arguments: Vec<Value>) -> Result<Value, RuntimeErrorOrReturnValue> {
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
        let mut interpreter: Interpreter = Interpreter::new(&mut self.environment);

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
                        return Ok(v);
                    }
                },
            };
        }
        self.environment.pop();
        Ok(Value::Nil)
    }
}

impl Display for LoxFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("LoxFunction")
    }
}

#[derive(Clone, Debug, PartialEq)]
enum ValueType {
    Number,
    String,
    Boolean,
    Nil,
    Function { num_args: usize },
}

#[derive(Clone, Debug, PartialEq)]
enum Value {
    Number(f64),
    String(String),
    Boolean(bool),
    Nil,
    Function(LoxFunction),
    NativeFunction(NativeFunction),
}

impl Value {
    fn get_type(&self) -> ValueType {
        match self {
            Value::Number(_) => ValueType::Number,
            Value::String(_) => ValueType::String,
            Value::Boolean(_) => ValueType::Boolean,
            Value::Nil => ValueType::Nil,
            Value::Function(f) => ValueType::Function {
                num_args: f.function.params.len(),
            },
            Value::NativeFunction(f) => f.get_type(),
        }
    }

    fn is_truthy(&self) -> bool {
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
            }) => f.write_str(&format!("{function:?}")),
            Value::NativeFunction(func) => f.write_str(&format!("{func}")),
        }
    }
}

fn construct_global_values() -> HashMap<String, Rc<RefCell<Value>>> {
    let clock = Value::NativeFunction(NativeFunction::Clock);
    HashMap::from([("clock".to_string(), Rc::new(RefCell::new(clock)))])
}

#[derive(Clone, Debug, PartialEq)]
pub struct Environment {
    values: Vec<HashMap<String, Rc<RefCell<Value>>>>,
    pos: usize,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            values: vec![construct_global_values()],
            pos: 0,
        }
    }

    fn push(&mut self) {
        self.values.push(HashMap::new());
        self.pos += 1;
    }

    fn pop(&mut self) -> Option<&mut Self> {
        self.pos = self.values.len() - 1;
        if self.pos == 0 {
            return None;
        }
        self.values.pop();
        self.pos -= 1;
        Some(self)
    }

    fn define(&mut self, name: String, value: Value) {
        self.values[self.pos].insert(name, Rc::new(RefCell::new(value)));
    }

    fn assign(&mut self, name: String, value: Value) -> Result<(), RuntimeErrorOrReturnValue> {
        let pos = self.pos;
        if let Entry::Occupied(mut e) = self.values[self.pos].entry(name.clone()) {
            e.insert(Rc::new(RefCell::new(value)));
            Ok(())
        } else {
            if self.pos == 0 {
                return Err(RuntimeError {
                    message: format!("Undefined variable '{name}'."),
                }
                .into());
            }
            self.pos -= 1;
            self.assign(name, value)?;
            self.pos = pos;
            Ok(())
        }
    }

    fn get(&self, name: &str) -> Result<Rc<RefCell<Value>>, RuntimeErrorOrReturnValue> {
        for values in self.values.iter().rev() {
            if let Some(v) = values.get(name) {
                return Ok(v.clone());
            }
        }
        Err(RuntimeError {
            message: format!("Undefined variable '{name}'."),
        }
        .into())
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
        Err(err) => match err {
            RuntimeErrorOrReturnValue::RuntimeError(e) => Err(e),
            RuntimeErrorOrReturnValue::ReturnValue(_) => Err(RuntimeError {
                message: "Invalid location for return statement".to_string(),
            }),
        },
    }
}

pub fn interpret_with_environment(
    statements: Vec<Stmt>,
    environment: &mut Environment,
) -> Result<(), RuntimeError> {
    let mut interpreter = Interpreter::new(environment);
    for statement in statements {
        interpreter.execute(statement).map_err(|err| match err {
            RuntimeErrorOrReturnValue::RuntimeError(e) => e,
            RuntimeErrorOrReturnValue::ReturnValue(_) => RuntimeError {
                message: "Invalid location for return statement".to_string(),
            },
        })?;
    }
    Ok(())
}

fn interpret_with_optional_environment(
    statements: Vec<Stmt>,
    optional_environment: Option<&mut Environment>,
) -> Result<(), RuntimeErrorOrReturnValue> {
    let mut potential_new_environment = Environment::new();
    let mut interpreter = if let Some(env) = optional_environment {
        Interpreter::new(env)
    } else {
        Interpreter::new(&mut potential_new_environment)
    };
    for statement in statements {
        interpreter.execute(statement)?;
    }
    Ok(())
}

struct Interpreter<'a> {
    environment: &'a mut Environment,
}

impl<'a> Interpreter<'a> {
    pub fn new(environment: &'a mut Environment) -> Self {
        environment.push();
        Self { environment }
    }

    fn execute(&mut self, statement: Stmt) -> Result<(), RuntimeErrorOrReturnValue> {
        match statement {
            Stmt::Block(block_statement) => {
                for stmt in block_statement.0 {
                    self.execute(stmt)?;
                }
                Ok(())
            }
            Stmt::Function(function) => {
                // We give the function a frozen snapshot of the environment at the
                //  definition time of the function.
                let mut environment = self.environment.clone();
                environment.push();
                let lox_function = LoxFunction {
                    function,
                    environment,
                };
                let function_name = lox_function.function.name.clone();
                self.environment
                    .define(function_name, Value::Function(lox_function));

                Ok(())
            }
            Stmt::Return(stmt) => {
                let value = match self.evaluate(stmt.value) {
                    Ok(v) => v,
                    Err(e) => return Err(e),
                };
                Err(RuntimeErrorOrReturnValue::ReturnValue(
                    (*value).clone().into_inner(),
                ))
            }
            Stmt::If(IfStatement {
                condition,
                then_stmt,
                else_stmt,
            }) => {
                let condition_value = self.evaluate(condition)?;
                if condition_value.borrow().is_truthy() {
                    self.execute(*then_stmt)?;
                } else if let Some(stmt) = else_stmt {
                    self.execute(*stmt)?;
                };
                Ok(())
            }
            Stmt::Print(expr) => {
                let value_result = self.evaluate(expr);
                let value = match value_result {
                    Ok(v) => v,
                    Err(err) => return Err(err),
                };
                println!("{}", value.borrow());
                Ok(())
            }
            Stmt::Expression(expr) => {
                self.evaluate(expr)?;
                Ok(())
            }
            Stmt::Var(VariableDeclaration { name, initializer }) => {
                let value = match self.evaluate(initializer) {
                    Ok(v) => v,
                    Err(err) => return Err(err),
                };
                self.environment.define(name, value.borrow().clone());
                Ok(())
            }
            Stmt::While(WhileStatement { condition, body }) => {
                while {
                    match self.evaluate(condition.clone()) {
                        Ok(value) => value.borrow().is_truthy(),
                        Err(err) => return Err(err),
                    }
                } {
                    self.execute(*body.clone())?;
                }
                Ok(())
            }
        }
    }

    fn evaluate(
        &mut self,
        expression: Expr,
    ) -> Result<Rc<RefCell<Value>>, RuntimeErrorOrReturnValue> {
        match expression {
            Expr::Assign { name, expression } => {
                let value = self.evaluate(*expression)?;
                self.environment.assign(name, value.borrow().clone())?;
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
            Expr::Number(x) => Ok(Rc::new(RefCell::new(Value::Number(x)))),
            Expr::String(x) => Ok(Rc::new(RefCell::new(Value::String(x)))),
            Expr::Boolean(x) => Ok(Rc::new(RefCell::new(Value::Boolean(x)))),
            Expr::Nil => Ok(Rc::new(RefCell::new(Value::Nil))),
            Expr::Variable(name) => self.environment.get(&name),
        }
    }

    fn evaluate_binary_expression(
        &mut self,
        left_expression: Expr,
        right_expression: Expr,
        operator: BinaryOperator,
    ) -> Result<Rc<RefCell<Value>>, RuntimeErrorOrReturnValue> {
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
                    Ok(Rc::new(RefCell::new(Value::Boolean(false))))
                } else {
                    Ok(Rc::new(RefCell::new(Value::Boolean(true))))
                }
            }
            BinaryOperator::EqualEqual => {
                if left_value == right_value {
                    Ok(Rc::new(RefCell::new(Value::Boolean(true))))
                } else {
                    Ok(Rc::new(RefCell::new(Value::Boolean(false))))
                }
            }
            BinaryOperator::Greater => {
                match (left_value.borrow().clone(), right_value.borrow().clone()) {
                    (Value::Number(l), Value::Number(r)) => {
                        if l > r {
                            Ok(Rc::new(RefCell::new(Value::Boolean(true))))
                        } else {
                            Ok(Rc::new(RefCell::new(Value::Boolean(false))))
                        }
                    }
                    (lv, rv) => Err(RuntimeError {
                        message: format!(
                            "Cannot compare {:?} and {:?}",
                            lv.get_type(),
                            rv.get_type()
                        ),
                    }
                    .into()),
                }
            }
            BinaryOperator::GreaterEqual => {
                match (left_value.borrow().clone(), right_value.borrow().clone()) {
                    (Value::Number(l), Value::Number(r)) => {
                        if l >= r {
                            Ok(Rc::new(RefCell::new(Value::Boolean(true))))
                        } else {
                            Ok(Rc::new(RefCell::new(Value::Boolean(false))))
                        }
                    }
                    (lv, rv) => Err(RuntimeError {
                        message: format!(
                            "Cannot compare {:?} and {:?}",
                            lv.get_type(),
                            rv.get_type()
                        ),
                    }
                    .into()),
                }
            }
            BinaryOperator::Less => {
                match (left_value.borrow().clone(), right_value.borrow().clone()) {
                    (Value::Number(l), Value::Number(r)) => {
                        if l < r {
                            Ok(Rc::new(RefCell::new(Value::Boolean(true))))
                        } else {
                            Ok(Rc::new(RefCell::new(Value::Boolean(false))))
                        }
                    }
                    (lv, rv) => Err(RuntimeError {
                        message: format!(
                            "Cannot compare {:?} and {:?}",
                            lv.get_type(),
                            rv.get_type()
                        ),
                    }
                    .into()),
                }
            }
            BinaryOperator::LessEqual => {
                match (left_value.borrow().clone(), right_value.borrow().clone()) {
                    (Value::Number(l), Value::Number(r)) => {
                        if l <= r {
                            Ok(Rc::new(RefCell::new(Value::Boolean(true))))
                        } else {
                            Ok(Rc::new(RefCell::new(Value::Boolean(false))))
                        }
                    }
                    (lv, rv) => Err(RuntimeError {
                        message: format!(
                            "Cannot compare {:?} and {:?}",
                            lv.get_type(),
                            rv.get_type()
                        ),
                    }
                    .into()),
                }
            }
            BinaryOperator::Plus => {
                match (left_value.borrow().clone(), right_value.borrow().clone()) {
                    (Value::Number(l), Value::Number(r)) => {
                        Ok(Rc::new(RefCell::new(Value::Number(l + r))))
                    }
                    (Value::String(l), Value::String(r)) => {
                        Ok(Rc::new(RefCell::new(Value::String(format!("{l}{r}")))))
                    }
                    (lv, rv) => Err(RuntimeError {
                        message: format!("Cannot add {:?} and {:?}", lv.get_type(), rv.get_type()),
                    }
                    .into()),
                }
            }
            BinaryOperator::Minus => {
                match (left_value.borrow().clone(), right_value.borrow().clone()) {
                    (Value::Number(l), Value::Number(r)) => {
                        Ok(Rc::new(RefCell::new(Value::Number(l - r))))
                    }
                    (lv, rv) => Err(RuntimeError {
                        message: format!(
                            "Cannot subtract {:?} from {:?}",
                            rv.get_type(),
                            lv.get_type()
                        ),
                    }
                    .into()),
                }
            }
            BinaryOperator::Star => {
                match (left_value.borrow().clone(), right_value.borrow().clone()) {
                    (Value::Number(l), Value::Number(r)) => {
                        Ok(Rc::new(RefCell::new(Value::Number(l * r))))
                    }
                    (lv, rv) => Err(RuntimeError {
                        message: format!(
                            "Cannot multiply {:?} and {:?}",
                            lv.get_type(),
                            rv.get_type()
                        ),
                    }
                    .into()),
                }
            }
            BinaryOperator::Slash => {
                match (left_value.borrow().clone(), right_value.borrow().clone()) {
                    (Value::Number(l), Value::Number(r)) => {
                        Ok(Rc::new(RefCell::new(Value::Number(l / r))))
                    }
                    (lv, rv) => Err(RuntimeError {
                        message: format!(
                            "Cannot divide {:?} by {:?}",
                            lv.get_type(),
                            rv.get_type()
                        ),
                    }
                    .into()),
                }
            }
        }
    }

    fn evaluate_call_expression(
        &mut self,
        callee_expr: Expr,
        paren: Token,
        arguments: Vec<Expr>,
    ) -> Result<Rc<RefCell<Value>>, RuntimeErrorOrReturnValue> {
        let callee_value = self.evaluate(callee_expr)?;
        let mut args: Vec<Value> = vec![];
        for arg_expr in arguments {
            let tmp = self.evaluate(arg_expr)?.borrow().clone();
            args.push(tmp);
        }

        let c_v = &mut *callee_value.borrow_mut();
        match c_v {
            Value::Function(f) => f.call(args).map(|x| Rc::new(RefCell::new(x))),
            Value::NativeFunction(f) => f.call(args).map(|x| Rc::new(RefCell::new(x))),
            _ => Err(RuntimeError {
                message: format!("Can only call functions and classes at {paren:?}"),
            }
            .into()),
        }
    }

    fn evaluate_logical_operator_expression(
        &mut self,
        left_expression: Expr,
        right_expression: Expr,
        operator: LogicalOperator,
    ) -> Result<Rc<RefCell<Value>>, RuntimeErrorOrReturnValue> {
        let left_value = self.evaluate(left_expression)?;
        match operator {
            LogicalOperator::And => {
                if left_value.borrow().is_truthy() {
                    self.evaluate(right_expression)
                } else {
                    Ok(Rc::new(RefCell::new(Value::Boolean(false))))
                }
            }
            LogicalOperator::Or => {
                if left_value.borrow().is_truthy() {
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
    ) -> Result<Rc<RefCell<Value>>, RuntimeErrorOrReturnValue> {
        let value = self.evaluate(expression)?;
        match operator {
            UnaryOperator::Bang => {
                if *value.borrow() == Value::Nil || *value.borrow() == Value::Boolean(false) {
                    Ok(Rc::new(RefCell::new(Value::Boolean(true))))
                } else {
                    Ok(Rc::new(RefCell::new(Value::Boolean(false))))
                }
            }
            UnaryOperator::Minus => match &*value.borrow() {
                Value::Number(x) => Ok(Rc::new(RefCell::new(Value::Number(*x)))),
                v => Err(RuntimeError {
                    message: format!("Cannot take minus of {:?}", v.clone().get_type()),
                }
                .into()),
            },
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        interpreting::interpreter::Value,
        parsing::{
            BinaryOperator, BlockStatement, Expr, FunctionStatement, ReturnStatement, Stmt,
            VariableDeclaration,
        },
        scanning::{Token, TokenType},
    };

    use super::{Environment, Interpreter};

    // This test currently fails because the 'f' retreived from the environment is a clone
    //  and not a mutable reference.
    #[test]
    fn test_closure() {
        let inner_func_stmt = Stmt::Function(FunctionStatement {
            name: "inner".to_string(),
            params: vec![],
            body: vec![Stmt::Expression(Expr::Assign {
                name: "x".to_string(),
                expression: Box::new(Expr::Binary {
                    left: Box::new(Expr::Variable("x".to_string())),
                    operator: BinaryOperator::Plus,
                    right: Box::new(Expr::Number(1.0)),
                }),
            })],
        });

        let outer_function_stmt = Stmt::Function(FunctionStatement {
            name: "outer".to_string(),
            params: vec![],
            body: vec![
                Stmt::Var(VariableDeclaration {
                    name: "x".to_string(),
                    initializer: Expr::Number(0.0),
                }),
                inner_func_stmt,
                Stmt::Return(ReturnStatement {
                    return_token: Token {
                        token_type: TokenType::Return,
                        line: 3,
                    },
                    value: Expr::Variable("inner".to_string()),
                }),
            ],
        });

        let program = Stmt::Block(BlockStatement(vec![
            outer_function_stmt,
            Stmt::Var(VariableDeclaration {
                name: "f".to_string(),
                initializer: Expr::Call {
                    callee: Box::new(Expr::Variable("outer".to_string())),
                    paren: Token {
                        token_type: TokenType::LeftParen,
                        line: 4,
                    },
                    arguments: vec![],
                },
            }),
            Stmt::Expression(Expr::Call {
                callee: Box::new(Expr::Variable("f".to_string())),
                paren: Token {
                    token_type: TokenType::LeftParen,
                    line: 5,
                },
                arguments: vec![],
            }),
        ]));
        let environment = &mut Environment::new();
        let mut interpreter = Interpreter::new(environment);
        match interpreter.execute(program) {
            Ok(_) => {}
            Err(_) => panic!("should execute without error"),
        };
        let f_func = match environment.get("f") {
            Ok(f) => f,
            Err(_) => panic!("'f' should be defined"),
        };
        let binding = f_func.borrow();
        let lox_func = match &*binding {
            Value::Function(f) => f,
            _ => panic!("'f' should be a function"),
        };

        assert_eq!(
            lox_func
                .environment
                .get("x")
                .clone()
                .map(|x| x.borrow().clone()),
            Ok(Value::Number(1.0))
        );
    }
}
