use crate::parser::expression::Expr;

#[derive(Debug)]
pub enum Stmt {
    Block(Vec<Stmt>),
    Expression(Expr),
    Print(Expr),
    Var {
        name: String,
        initializer: Expr,
    },
    If {
        condition: Expr,
        then_stmt: Box<Stmt>,
        else_stmt: Option<Box<Stmt>>,
    },
}
