use std::{collections::HashMap, error::Error, fmt::Display};

use crate::parsing::{ClassStatement, Expr, FunctionStatement, ResolvableExpr, Stmt, Variable};

use super::{class_types::ClassType, function_types::FunctionType};

#[derive(Debug)]
pub struct ResolverError {
    message: String,
}

impl Display for ResolverError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&format!("Resolver error: {}", self.message))
    }
}

impl Error for ResolverError {}

impl ResolverError {
    fn new(message: String) -> ResolverError {
        ResolverError { message }
    }
}

type Scopes = Vec<HashMap<String, bool>>;

pub fn resolve(statements: Vec<Stmt>) -> Result<HashMap<ResolvableExpr, usize>, ResolverError> {
    let mut locals = HashMap::<ResolvableExpr, usize>::new();
    let mut resolver = Resolver::new(&mut locals);
    resolver.resolve(statements)?;
    Ok(locals)
}

struct Resolver<'a> {
    scopes: Scopes,
    locals: &'a mut HashMap<ResolvableExpr, usize>,
    current_function_type: FunctionType,
    current_class_type: ClassType,
}

impl<'a> Resolver<'a> {
    fn new(locals: &'a mut HashMap<ResolvableExpr, usize>) -> Self {
        let mut new_res = Self {
            scopes: Scopes::new(),
            locals,
            current_function_type: FunctionType::None,
            current_class_type: ClassType::None,
        };
        new_res.begin_scope();
        new_res
    }

    fn resolve(&mut self, statements: Vec<Stmt>) -> Result<(), ResolverError> {
        for stmt in statements {
            self.resolve_statement(stmt)?;
        }
        Ok(())
    }

    fn resolve_statement(&mut self, statement: Stmt) -> Result<(), ResolverError> {
        match statement {
            Stmt::Block(block_statement) => {
                self.begin_scope();
                self.resolve(block_statement.0)?;
                self.end_scope();
            }
            Stmt::Class(ClassStatement {
                name,
                line_number,
                methods,
                superclass,
            }) => {
                let enclosing_class_type = self.current_class_type.clone();
                self.current_class_type = ClassType::Class;
                self.declare(name.clone())?;
                self.define(name.clone())?;
                if let Some(sc) = superclass.clone() {
                    if &sc == &name {
                        return Err(ResolverError::new(
                            "A class cannot inherit from itself.".to_string(),
                        ));
                    }
                    self.resolve_expression(Expr::Variable(Variable {
                        name: sc,
                        line_number,
                    }))?;
                }

                if superclass.is_some() {
                    self.begin_scope();
                    self.declare("super".to_string())?;
                    self.define("super".to_string())?;
                }

                self.begin_scope();
                self.declare("this".to_string())?;
                self.define("this".to_string())?;

                for method in methods {
                    let function_type = if method.name == "init" {
                        FunctionType::Initializer
                    } else {
                        FunctionType::Method
                    };
                    self.resolve_function(method, function_type)?;
                }
                self.end_scope();

                if superclass.is_some() {
                    self.end_scope();
                }

                self.current_class_type = enclosing_class_type;
            }
            Stmt::Var(variable_declaration) => {
                self.declare(variable_declaration.name.clone())?;
                self.resolve_expression(variable_declaration.initializer)?;
                self.define(variable_declaration.name.clone())?;
            }
            Stmt::Function(function_stmt) => {
                self.declare(function_stmt.name.clone())?;
                self.define(function_stmt.name.clone())?;
                self.resolve_function(function_stmt, FunctionType::Function)?;
            }
            Stmt::Expression(expr) => self.resolve_expression(expr)?,
            Stmt::If(if_stmt) => {
                self.resolve_expression(if_stmt.condition)?;
                self.resolve_statement(*if_stmt.then_stmt)?;
                if let Some(else_stmt) = if_stmt.else_stmt {
                    self.resolve_statement(*else_stmt)?;
                }
            }
            Stmt::Print(print_stmt) => self.resolve_expression(print_stmt)?,
            Stmt::Return(return_stmt) => {
                match self.current_function_type {
                    FunctionType::None => Err(ResolverError::new(
                        "Cannot return from outside a function".to_string(),
                    )),
                    FunctionType::Initializer => {
                        if return_stmt.value != Expr::Nil {
                            Err(ResolverError::new(
                                "Cannot return a value from an initializer.".to_string(),
                            ))
                        } else {
                            self.resolve_expression(return_stmt.value)
                        }
                    }
                    FunctionType::Method | FunctionType::Function => {
                        self.resolve_expression(return_stmt.value)
                    }
                }?;
            }
            Stmt::While(while_stmt) => {
                self.resolve_expression(while_stmt.condition)?;
                self.resolve_statement(*while_stmt.body)?;
            }
            Stmt::Test(expr) => self.resolve_expression(expr.value_to_save)?,
        }
        Ok(())
    }

    fn resolve_function(
        &mut self,
        function_stmt: FunctionStatement,
        function_type: FunctionType,
    ) -> Result<(), ResolverError> {
        let enclosing_function_type = self.current_function_type.clone();
        self.current_function_type = function_type;
        self.begin_scope();
        for param in function_stmt.params {
            self.declare(param.clone())?;
            self.define(param)?;
        }
        self.resolve(function_stmt.body)?;
        self.end_scope();
        self.current_function_type = enclosing_function_type;
        Ok(())
    }

    fn resolve_expression(&mut self, expression: Expr) -> Result<(), ResolverError> {
        match &expression {
            Expr::Variable(var_expr) => {
                if !self.scopes.is_empty() && (self.peek().get(&var_expr.name) == Some(&false)) {
                    return Err(ResolverError::new(
                        "Cannot read local variable in its own initializer".to_string(),
                    ));
                }
                self.resolve_local(ResolvableExpr::Variable(var_expr.clone()))?;
            }
            Expr::Assign {
                variable,
                expression: expr,
            } => {
                self.resolve_expression(*expr.clone())?;
                self.resolve_local(ResolvableExpr::Variable(variable.clone()))?;
            }
            Expr::Binary {
                left,
                operator: _,
                right,
            }
            | Expr::LogicalOperator {
                left,
                operator: _,
                right,
            } => {
                self.resolve_expression(*left.clone())?;
                self.resolve_expression(*right.clone())?;
            }
            Expr::Unary {
                operator: _,
                expression,
            } => self.resolve_expression(*expression.clone())?,
            Expr::Call {
                callee,
                paren: _,
                arguments,
            } => {
                self.resolve_expression(*callee.clone())?;
                for arg in arguments {
                    self.resolve_expression(arg.clone())?;
                }
            }
            Expr::Super {
                method: _,
                line_number,
            } => self.resolve_local(ResolvableExpr::Super {
                line_number: *line_number,
            })?,
            Expr::Get { object, name: _ } => self.resolve_expression(*object.clone())?,
            Expr::Set {
                object,
                name: _,
                value,
            } => {
                self.resolve_expression(*value.clone())?;
                self.resolve_expression(*object.clone())?;
            }
            Expr::This { line_number } => {
                if self.current_class_type == ClassType::None {
                    return Err(ResolverError::new(
                        "Cannot use 'this' outside of a class.".to_string(),
                    ));
                }
                self.resolve_local(ResolvableExpr::This {
                    line_number: *line_number,
                })?
            }
            Expr::Grouping(expr) => self.resolve_expression(*expr.clone())?,

            Expr::String(_) | Expr::Nil | Expr::Number(_) | Expr::Boolean(_) => {}
        }
        Ok(())
    }

    /// Get depth of variable in stack at time of function call.
    fn resolve_local(&mut self, resolvable_expr: ResolvableExpr) -> Result<(), ResolverError> {
        if self.scopes.is_empty() {
            // Must be global scope
            return Ok(());
        }
        for i in (0..self.scopes.len()).rev() {
            if self.scopes[i].contains_key(&resolvable_expr.to_string()) {
                self.locals.insert(resolvable_expr.clone(), i);
                break;
            }
        }
        Ok(())
    }

    fn declare(&mut self, name: String) -> Result<(), ResolverError> {
        if self.scopes.is_empty() {
            return Ok(());
        }
        self.peek().insert(name, false);
        Ok(())
    }

    fn define(&mut self, name: String) -> Result<(), ResolverError> {
        if self.scopes.is_empty() {
            return Ok(());
        }
        self.peek().insert(name, true);
        Ok(())
    }

    fn peek(&mut self) -> &mut HashMap<String, bool> {
        let idx = self.scopes.len() - 1;
        &mut self.scopes[idx]
    }

    fn begin_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn end_scope(&mut self) {
        self.scopes.pop();
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use crate::{
        parsing::{
            BlockStatement, ClassStatement, Expr, FunctionStatement, ResolvableExpr,
            ReturnStatement, Stmt, Variable, VariableDeclaration,
        },
        scanning::{Token, TokenType},
    };

    use super::resolve;

    #[test]
    fn test_resolve() {
        // var x = 0;
        // {
        //     var y = 0;
        //     print x;
        // }
        let statements = vec![
            Stmt::Var(VariableDeclaration {
                name: "x".to_string(),
                initializer: Expr::Number(0.0),
            }),
            Stmt::Block(BlockStatement(vec![
                Stmt::Var(VariableDeclaration {
                    name: "y".to_string(),
                    initializer: Expr::Number(0.0),
                }),
                Stmt::Print(Expr::Variable(Variable {
                    name: "x".to_string(),
                    line_number: 4,
                })),
            ])),
        ];
        let locals = match resolve(statements) {
            Ok(v) => v,
            Err(_) => panic!("Should run without error."),
        };
        assert_eq!(
            locals,
            HashMap::<ResolvableExpr, usize>::from_iter([(
                ResolvableExpr::Variable(Variable {
                    name: "x".to_string(),
                    line_number: 4
                }),
                0
            ),])
        );
    }

    #[test]
    fn test_resolve2() {
        // var x = 0;
        // {
        //     var y = 0;
        //     print x;
        //     {
        //         var x = 1;
        //         print x;
        //     }
        // }
        let statements = vec![
            Stmt::Var(VariableDeclaration {
                name: "x".to_string(),
                initializer: Expr::Number(0.0),
            }),
            Stmt::Block(BlockStatement(vec![
                Stmt::Var(VariableDeclaration {
                    name: "y".to_string(),
                    initializer: Expr::Number(0.0),
                }),
                Stmt::Print(Expr::Variable(Variable {
                    name: "x".to_string(),
                    line_number: 4,
                })),
                Stmt::Block(BlockStatement(vec![
                    Stmt::Var(VariableDeclaration {
                        name: "x".to_string(),
                        initializer: Expr::Number(0.0),
                    }),
                    Stmt::Print(Expr::Variable(Variable {
                        name: "x".to_string(),
                        line_number: 7,
                    })),
                ])),
            ])),
        ];
        let locals = match resolve(statements) {
            Ok(v) => v,
            Err(_) => panic!("Should run without error."),
        };
        assert_eq!(
            locals,
            HashMap::<ResolvableExpr, usize>::from_iter([
                (
                    ResolvableExpr::Variable(Variable {
                        name: "x".to_string(),
                        line_number: 4
                    }),
                    0
                ),
                (
                    ResolvableExpr::Variable(Variable {
                        name: "x".to_string(),
                        line_number: 7
                    }),
                    2
                )
            ])
        );
    }

    #[test]
    fn test_class_resolution() {
        // class Thing {
        //     getCallback() {
        //         fun localFunction() {
        //             print this;
        //         }
        //        return localFunction;
        //    }
        // }
        let local_function = FunctionStatement {
            name: "localFunction".to_string(),
            params: vec![],
            body: vec![Stmt::Print(Expr::This { line_number: 4 })],
        };
        let get_callback = FunctionStatement {
            name: "getCallback".to_string(),
            params: vec![],
            body: vec![
                Stmt::Function(local_function),
                Stmt::Return(ReturnStatement {
                    return_token: Token {
                        token_type: TokenType::Return,
                        line: 6,
                    },
                    value: Expr::Variable(Variable {
                        name: "localFunction".to_string(),
                        line_number: 6,
                    }),
                }),
            ],
        };
        let statements = vec![Stmt::Class(ClassStatement {
            name: "Thing".to_string(),
            line_number: 1,
            superclass: None,
            methods: vec![get_callback],
        })];
        let locals = match resolve(statements) {
            Ok(v) => v,
            Err(_) => panic!("Should run without error."),
        };
        assert_eq!(
            locals,
            HashMap::<ResolvableExpr, usize>::from_iter([
                (ResolvableExpr::This { line_number: 4 }, 1),
                (
                    ResolvableExpr::Variable(Variable {
                        name: "localFunction".to_string(),
                        line_number: 6
                    }),
                    2
                )
            ])
        )
    }
}
