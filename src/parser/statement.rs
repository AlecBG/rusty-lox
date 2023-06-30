use crate::parser::expression::Expr;

pub enum Stmt {
    Expression(Expr),
    Print(Expr),
}
