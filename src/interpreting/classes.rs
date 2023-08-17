use std::fmt::Display;

#[derive(Clone, Debug, PartialEq)]
pub struct LoxClass {
    pub name: String,
}

impl Display for LoxClass {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.name)
    }
}
