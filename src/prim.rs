#[derive(Debug, Clone)]
pub struct Symbol(pub String);

#[derive(Debug, Clone)]
pub enum Literal {
    Int(i64),
    Bool(bool),
}
