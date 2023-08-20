use std::{collections::HashMap, error::Error, fmt::Display};

use crate::parsing::{ClassStatement, Expr, FunctionStatement, ResolvableExpr, Stmt};

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
            Stmt::Class(ClassStatement { name, methods }) => {
                let enclosing_class_type = self.current_class_type.clone();
                self.current_class_type = ClassType::Class;
                self.declare(name.clone())?;
                self.define(name)?;

                self.begin_scope();
                if let Some(vals) = self.scopes.last_mut() {
                    vals.insert("this".to_string(), true);
                } else {
                    panic!();
                }

                for method in methods {
                    let function_type = if method.name == "init" {
                        FunctionType::Initializer
                    } else {
                        FunctionType::Method
                    };
                    self.resolve_function(method, function_type)?;
                }
                self.end_scope();
                self.current_class_type = enclosing_class_type;
            }
            Stmt::Var(variable_declaration) => {
                self.declare(variable_declaration.name.clone())?;
                self.resolve_expression(variable_declaration.initializer)?;
                self.define(variable_declaration.name.clone())?;
                self.resolve_local(variable_declaration.name)?;
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
            Stmt::Test(_) => {}
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
            Expr::Variable(name) => {
                if !self.scopes.is_empty() && (self.peek().get(name) == Some(&false)) {
                    return Err(ResolverError::new(
                        "Cannot read local variable in its own initializer".to_string(),
                    ));
                }
                self.resolve_local(name.clone())?;
            }
            Expr::Assign {
                name,
                expression: expr,
            } => {
                self.resolve_expression(*expr.clone())?;
                self.resolve_local(name.clone())?;
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
            Expr::Get { object, name: _ } => self.resolve_expression(*object.clone())?,
            Expr::Set {
                object,
                name: _,
                value,
            } => {
                self.resolve_expression(*value.clone())?;
                self.resolve_expression(*object.clone())?;
            }
            Expr::This(_) => {
                if self.current_class_type == ClassType::None {
                    return Err(ResolverError::new(
                        "Cannot use 'this' outside of a class.".to_string(),
                    ));
                }
                self.resolve_local("this".to_string())?
            }
            Expr::Grouping(expr) => self.resolve_expression(*expr.clone())?,

            Expr::String(_) | Expr::Nil | Expr::Number(_) | Expr::Boolean(_) => {}
        }
        Ok(())
    }

    /// Get depth of variable in stack at time of function call.
    fn resolve_local(&mut self, name: String) -> Result<(), ResolverError> {
        if self.scopes.is_empty() {
            // Must be global scope
            return Ok(());
        }
        for i in (0..self.scopes.len()).rev() {
            if self.scopes[i].contains_key(&name) {
                self.locals
                    .insert(ResolvableExpr::Variable(name.clone()), i);
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

    use crate::parsing::{BlockStatement, Expr, ResolvableExpr, Stmt, VariableDeclaration};

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
                Stmt::Print(Expr::Variable("x".to_string())),
            ])),
        ];
        let locals = match resolve(statements) {
            Ok(v) => v,
            Err(_) => panic!("Should run without error."),
        };
        assert_eq!(
            locals,
            HashMap::<ResolvableExpr, usize>::from_iter([
                (ResolvableExpr::Variable("x".to_string()), 0),
                (ResolvableExpr::Variable("y".to_string()), 1)
            ])
        );
    }
}
