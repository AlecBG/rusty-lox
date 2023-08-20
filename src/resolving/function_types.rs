#[derive(Clone, Debug, PartialEq)]
pub enum FunctionType {
    Function,
    Method,
    Initializer,
    None, // Not a function
}
