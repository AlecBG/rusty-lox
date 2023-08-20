use crate::parsing::{
    BinaryOperator, Expr, IfStatement, LogicalOperator, ResolvableExpr, SaveExpression, Stmt,
    UnaryOperator, VariableDeclaration, WhileStatement,
};
use crate::scanning::Token;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use super::classes::LoxClass;
use super::environment::Environment;
use super::functions::{LoxCallable, LoxFunction};
use super::runtime_errors::{RuntimeError, RuntimeErrorOrReturnValue};
use super::values::Value;

pub struct Interpreter {
    environment: Box<dyn Environment>,
    locals: HashMap<ResolvableExpr, usize>,
    with_resolver: bool,
}

impl Interpreter {
    pub fn new(
        mut environment: Box<dyn Environment>,
        locals: HashMap<ResolvableExpr, usize>,
    ) -> Self {
        environment.push();
        Self {
            environment,
            locals,
            with_resolver: true,
        }
    }

    pub fn new_without_resolver(mut environment: Box<dyn Environment>) -> Self {
        environment.push();
        Self {
            environment,
            locals: HashMap::new(),
            with_resolver: false,
        }
    }

    pub fn execute_statements(&mut self, stmts: Vec<Stmt>) -> Result<(), RuntimeError> {
        for stmt in stmts {
            if let Err(RuntimeErrorOrReturnValue::RuntimeError(err)) = self.execute(stmt) {
                return Err(err);
            }
        }
        Ok(())
    }

    pub fn execute(&mut self, statement: Stmt) -> Result<(), RuntimeErrorOrReturnValue> {
        match statement {
            Stmt::Block(block_statement) => {
                self.environment.push();
                for stmt in block_statement.0 {
                    self.execute(stmt)?;
                }
                self.environment.pop();
                Ok(())
            }
            Stmt::Class(class_statement) => {
                let class_name = class_statement.name.clone();
                let methods: HashMap<String, LoxFunction> = class_statement
                    .methods
                    .into_iter()
                    .map(|m| {
                        let mut environment = self.environment.clone();
                        environment.push();
                        (
                            m.clone().name,
                            LoxFunction {
                                function: m,
                                environment,
                                with_resolver: self.with_resolver,
                                is_initializer: false,
                            },
                        )
                    })
                    .collect();

                let lox_class = LoxClass {
                    name: class_name.clone(),
                    methods,
                };
                self.environment.define(class_name, Value::Class(lox_class));
                Ok(())
            }
            Stmt::Function(function) => {
                let mut environment = self.environment.clone();
                environment.push();
                let lox_function = LoxFunction {
                    function,
                    environment,
                    with_resolver: self.with_resolver,
                    is_initializer: false,
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
            Stmt::Test(SaveExpression {
                value_to_save,
                values,
            }) => {
                let value = match self.evaluate(value_to_save) {
                    Ok(v) => v.borrow().clone(),
                    Err(err) => return Err(err),
                };
                let mut_vs = &mut *values.borrow_mut();
                mut_vs.push(value);
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
            Expr::Get { object, name } => {
                let obj_value = self.evaluate(*object)?;
                let o = &*obj_value.borrow();
                match o {
                    Value::Instance(instance) => instance.get(&name).map_err(|e| e.into()),
                    _ => Err(RuntimeError {
                        message: "Only instances have properties".to_string(),
                    }
                    .into()),
                }
            }
            Expr::Set {
                object,
                name,
                value,
            } => {
                let obj_value = self.evaluate(*object)?;
                let v = self.evaluate(*value)?;
                let o = &mut *obj_value.borrow_mut();
                match o {
                    Value::Instance(instance) => {
                        instance.set(name, v.clone());
                        Ok(v.clone())
                    }
                    _ => Err(RuntimeError {
                        message: "Only instances have fields".to_string(),
                    }
                    .into()),
                }
            }
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
            Expr::This(_) => {
                if self.with_resolver {
                    let resolvable_expr = ResolvableExpr::Variable("this".to_string());
                    self.lookup_variable(resolvable_expr)
                } else {
                    self.environment.get("this")
                }
            }
            Expr::Number(x) => Ok(Rc::new(RefCell::new(Value::Number(x)))),
            Expr::String(x) => Ok(Rc::new(RefCell::new(Value::String(x)))),
            Expr::Boolean(x) => Ok(Rc::new(RefCell::new(Value::Boolean(x)))),
            Expr::Nil => Ok(Rc::new(RefCell::new(Value::Nil))),
            Expr::Variable(name) => {
                if self.with_resolver {
                    let resolvable_expr = ResolvableExpr::Variable(name);
                    self.lookup_variable(resolvable_expr)
                } else {
                    self.environment.get(&name)
                }
            }
        }
    }

    fn lookup_variable(
        &mut self,
        expr: ResolvableExpr,
    ) -> Result<Rc<RefCell<Value>>, RuntimeErrorOrReturnValue> {
        if let Some(distance) = self.locals.get(&expr) {
            match expr {
                ResolvableExpr::Variable(name) => self.environment.get_at(&(distance + 1), &name),
            }
        } else {
            match expr {
                ResolvableExpr::Variable(name) => self.environment.get_at(&0, &name),
            }
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
        let out = match c_v {
            Value::Function(callable) => callable.call(args),
            Value::NativeFunction(callable) => callable.call(args),
            Value::Class(callable) => callable.call(args),
            _ => Err(RuntimeError {
                message: format!("Can only call functions and classes at {paren:?}"),
            }
            .into()),
        }?;
        Ok(out)
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
    use std::{cell::RefCell, rc::Rc};

    use super::Interpreter;
    use crate::{
        interpreting::{
            environment::{RefCellEnvironment, SingleCopyEnvironment},
            interpreter::Value,
            runtime_errors::RuntimeErrorOrReturnValue,
        },
        parsing::{
            BinaryOperator, BlockStatement, Expr, FunctionStatement, IfStatement, ReturnStatement,
            SaveExpression, Stmt, VariableDeclaration,
        },
        scanning::{Token, TokenType},
    };

    #[test]
    fn test_variable_declaration() {
        // var x = 0;
        // <SAVE VALUE OF x>
        let saved_values: Rc<RefCell<Vec<Value>>> = Rc::new(RefCell::new(vec![]));
        let program = Stmt::Block(BlockStatement(vec![
            Stmt::Var(VariableDeclaration {
                name: "x".to_string(),
                initializer: Expr::Number(0.0),
            }),
            Stmt::Test(SaveExpression {
                value_to_save: Expr::Variable("x".to_string()),
                values: saved_values.clone(),
            }),
        ]));
        let environment = Box::new(RefCellEnvironment::new());
        let mut interpreter = Interpreter::new_without_resolver(environment);
        match interpreter.execute(program) {
            Ok(_) => {}
            Err(e) => {
                println!("{e:#?}");
                panic!("should execute without error")
            }
        };
        assert_eq!(saved_values.borrow().clone(), vec![Value::Number(0.0)]);
    }

    #[test]
    fn test_function_declaration() {
        // fun f() {
        //     return 1.0;
        // }
        // var x = f();
        // <SAVE VALUE OF x>
        let saved_values: Rc<RefCell<Vec<Value>>> = Rc::new(RefCell::new(vec![]));
        let program = Stmt::Block(BlockStatement(vec![
            Stmt::Function(FunctionStatement {
                name: "f".to_string(),
                params: vec![],
                body: vec![Stmt::Return(ReturnStatement {
                    return_token: Token {
                        token_type: TokenType::Return,
                        line: 2,
                    },
                    value: Expr::Number(1.0),
                })],
            }),
            Stmt::Var(VariableDeclaration {
                name: "x".to_string(),
                initializer: Expr::Call {
                    callee: Box::new(Expr::Variable("f".to_string())),
                    paren: Token {
                        token_type: TokenType::LeftParen,
                        line: 4,
                    },
                    arguments: vec![],
                },
            }),
            Stmt::Test(SaveExpression {
                value_to_save: Expr::Variable("x".to_string()),
                values: saved_values.clone(),
            }),
        ]));
        let environment = Box::new(RefCellEnvironment::new());
        let mut interpreter = Interpreter::new_without_resolver(environment);
        match interpreter.execute(program) {
            Ok(_) => {}
            Err(e) => {
                println!("{e:#?}");
                panic!("should execute without error")
            }
        };
        assert_eq!(saved_values.borrow().clone(), vec![Value::Number(1.0)]);
    }

    #[test]
    fn test_recursion() {
        // var numCalls = 0;
        // fun f(i) {
        //     numCalls = numCalls + 1;
        //     if (i <= 0) {
        //       <SAVE VALUE OF numCalls>
        //       return;
        //     }
        //     f(i - 1);
        // }
        // f(3);
        let saved_values: Rc<RefCell<Vec<Value>>> = Rc::new(RefCell::new(vec![]));
        let func_def: Stmt = Stmt::Function(FunctionStatement {
            name: "f".to_string(),
            params: vec!["i".to_string()],
            body: vec![
                // Stmt::Var(VariableDeclaration {
                //     name: "numCalls".to_string(),
                //     initializer: Expr::Number(0.0),
                // }),
                Stmt::Expression(Expr::Assign {
                    name: "numCalls".to_string(),
                    expression: Box::new(Expr::Binary {
                        left: Box::new(Expr::Variable("numCalls".to_string())),
                        operator: BinaryOperator::Plus,
                        right: Box::new(Expr::Number(1.0)),
                    }),
                }),
                Stmt::If(IfStatement {
                    condition: Expr::Binary {
                        left: Box::new(Expr::Variable("i".to_string())),
                        operator: BinaryOperator::LessEqual,
                        right: Box::new(Expr::Number(0.0)),
                    },
                    then_stmt: Box::new(Stmt::Block(BlockStatement(vec![
                        Stmt::Test(SaveExpression {
                            value_to_save: Expr::Variable("numCalls".to_string()),
                            values: saved_values.clone(),
                        }),
                        Stmt::Return(ReturnStatement {
                            return_token: Token {
                                token_type: TokenType::Return,
                                line: 3,
                            },
                            value: Expr::Nil,
                        }),
                    ]))),
                    else_stmt: None,
                }),
                Stmt::Expression(Expr::Call {
                    callee: Box::new(Expr::Variable("f".to_string())),
                    paren: Token {
                        token_type: TokenType::LeftParen,
                        line: 4,
                    },
                    arguments: vec![Expr::Binary {
                        left: Box::new(Expr::Variable("i".to_string())),
                        operator: BinaryOperator::Minus,
                        right: Box::new(Expr::Number(1.0)),
                    }],
                }),
            ],
        });
        let program = Stmt::Block(BlockStatement(vec![
            Stmt::Var(VariableDeclaration {
                name: "numCalls".to_string(),
                initializer: Expr::Number(0.0),
            }),
            func_def,
            Stmt::Expression(Expr::Call {
                callee: Box::new(Expr::Variable("f".to_string())),
                paren: Token {
                    token_type: TokenType::LeftParen,
                    line: 6,
                },
                arguments: vec![Expr::Number(3.0)],
            }),
        ]));
        let environment = Box::new(RefCellEnvironment::new());
        let mut interpreter = Interpreter::new_without_resolver(environment);
        match interpreter.execute(program) {
            Ok(_) => {}
            Err(e) => {
                println!("{e:#?}");
                panic!("should execute without error")
            }
        };
        assert_eq!(saved_values.borrow().clone(), vec![Value::Number(4.0)]);
    }

    #[test]
    fn test_closure() {
        // fun outer() {
        //   var x = 0.0;
        //   fun inner()  {
        //     x = x + 1;
        //     <SAVE VALUE OF x>
        //   }
        // }
        // f = outer();
        // f();
        // f();

        let saved_values: Rc<RefCell<Vec<Value>>> = Rc::new(RefCell::new(vec![]));
        // fun inner() {
        //      x = x + 1;
        //      <SAVE VALUE OF x>
        // }
        let inner_func_stmt = Stmt::Function(FunctionStatement {
            name: "inner".to_string(),
            params: vec![],
            body: vec![
                Stmt::Expression(Expr::Assign {
                    name: "x".to_string(),
                    expression: Box::new(Expr::Binary {
                        left: Box::new(Expr::Variable("x".to_string())),
                        operator: BinaryOperator::Plus,
                        right: Box::new(Expr::Number(1.0)),
                    }),
                }),
                Stmt::Test(SaveExpression {
                    value_to_save: Expr::Variable("x".to_string()),
                    values: saved_values.clone(),
                }),
            ],
        });

        // fun outer() {
        //     var x = 0.0;
        //     <definition of inner>
        // }
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

        // <definition of outer>
        // f = outer();
        // f();
        // f();
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
            Stmt::Expression(Expr::Call {
                callee: Box::new(Expr::Variable("f".to_string())),
                paren: Token {
                    token_type: TokenType::LeftParen,
                    line: 5,
                },
                arguments: vec![],
            }),
        ]));
        // If we use the refcell environment, then we find that the value of x has been increased by one.
        let environment = Box::new(RefCellEnvironment::new());
        let mut interpreter = Interpreter::new_without_resolver(environment);
        match interpreter.execute(program.clone()) {
            Ok(_) => {}
            Err(e) => {
                println!("{e:#?}");
                panic!("should execute without error")
            }
        };
        assert_eq!(
            saved_values.borrow().clone(),
            vec![Value::Number(1.0), Value::Number(2.0)]
        );
        saved_values.borrow_mut().pop();
        saved_values.borrow_mut().pop();

        // If we use the single copy environment, then we find that the value of x is unchanged by the closure.
        let environment = Box::new(SingleCopyEnvironment::new());
        let mut interpreter = Interpreter::new_without_resolver(environment);
        match interpreter.execute(program) {
            Ok(_) => {}
            Err(e) => {
                println!("{e:#?}");
                panic!("should execute without error")
            }
        };
        assert_eq!(
            saved_values.borrow().clone(),
            vec![Value::Number(1.0), Value::Number(1.0)]
        );
    }

    #[test]
    fn test_native_clock() {
        // clock();
        let program = Stmt::Block(BlockStatement(vec![Stmt::Expression(Expr::Call {
            callee: Box::new(Expr::Variable("clock".to_string())),
            paren: Token {
                token_type: TokenType::LeftParen,
                line: 5,
            },
            arguments: vec![],
        })]));
        let environment = Box::new(RefCellEnvironment::new());
        let mut interpreter = Interpreter::new_without_resolver(environment);
        match interpreter.execute(program.clone()) {
            Ok(_) => {}
            Err(e) => {
                println!("{e:#?}");
                panic!("should execute without error")
            }
        };
        let environment = Box::new(SingleCopyEnvironment::new());
        let mut interpreter = Interpreter::new_without_resolver(environment);
        match interpreter.execute(program) {
            Ok(_) => {}
            Err(e) => {
                println!("{e:#?}");
                panic!("should execute without error")
            }
        };
    }

    #[test]
    fn test_lexically_scoped_environments() {
        // {
        //   var a = 1;
        // }
        // print a;  // expect exception here
        let program = Stmt::Block(BlockStatement(vec![
            Stmt::Block(BlockStatement(vec![Stmt::Var(VariableDeclaration {
                name: "a".to_string(),
                initializer: Expr::Number(1.0),
            })])),
            Stmt::Print(Expr::Variable("a".to_string())),
        ]));
        let environment = Box::new(RefCellEnvironment::new());
        let mut interpreter = Interpreter::new_without_resolver(environment);
        match interpreter.execute(program.clone()) {
            Ok(_) => panic!("Should throw runtime exception"),
            Err(e) => match e {
                RuntimeErrorOrReturnValue::RuntimeError(x) => {
                    assert!(x.message.contains("Undefined variable 'a'"))
                }
                _ => panic!("Should throw runtime exception"),
            },
        };
        let environment = Box::new(SingleCopyEnvironment::new());
        let mut interpreter = Interpreter::new_without_resolver(environment);
        match interpreter.execute(program.clone()) {
            Ok(_) => panic!("Should throw runtime exception"),
            Err(e) => match e {
                RuntimeErrorOrReturnValue::RuntimeError(x) => {
                    assert!(x.message.contains("Undefined variable 'a'"))
                }
                _ => panic!("Should throw runtime exception"),
            },
        };
    }
}
