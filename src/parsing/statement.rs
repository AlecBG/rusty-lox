use std::{cell::RefCell, rc::Rc};

use crate::{interpreting::values::Value, parsing::expression::Expr, scanning::Token};

#[derive(Clone, Debug, PartialEq)]
pub struct FunctionStatement {
    pub name: String,
    pub params: Vec<String>,
    pub body: Vec<Stmt>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct BlockStatement(pub Vec<Stmt>);

#[derive(Clone, Debug, PartialEq)]
pub struct ClassStatement {
    pub name: String,
    pub line_number: usize,
    pub superclass: Option<String>,
    pub methods: Vec<FunctionStatement>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct VariableDeclaration {
    pub name: String,
    pub initializer: Expr,
}

#[derive(Clone, Debug, PartialEq)]
pub struct IfStatement {
    pub condition: Expr,
    pub then_stmt: Box<Stmt>,
    pub else_stmt: Option<Box<Stmt>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct WhileStatement {
    pub condition: Expr,
    pub body: Box<Stmt>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ReturnStatement {
    pub return_token: Token, // Kept for error reporting
    pub value: Expr,
}

/// Used to capture a value for testing purposes
#[derive(Clone, Debug, PartialEq)]
pub struct SaveExpression {
    pub value_to_save: Expr,
    pub values: Rc<RefCell<Vec<Value>>>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Stmt {
    Block(BlockStatement),
    Expression(Expr),
    Function(FunctionStatement),
    Class(ClassStatement),
    Print(Expr),
    Var(VariableDeclaration),
    If(IfStatement),
    While(WhileStatement),
    Return(ReturnStatement),
    Test(SaveExpression),
}
