use crate::{parsing::expression::Expr, scanning::Token};

#[derive(Clone, Debug, PartialEq)]
pub struct FunctionStatement {
    pub name: String,
    pub params: Vec<String>,
    pub body: Vec<Stmt>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct BlockStatement(pub Vec<Stmt>);

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

#[derive(Clone, Debug, PartialEq)]
pub enum Stmt {
    Block(BlockStatement),
    Expression(Expr),
    Function(FunctionStatement),
    Print(Expr),
    Var(VariableDeclaration),
    If(IfStatement),
    While(WhileStatement),
    Return(ReturnStatement),
}
