use crate::types::TypeId;

#[derive(Debug, Clone)]
pub enum Value {
    Unit,
    Int(u64),
    Float(f64),
    String(String),
    Identifier(String),
    Type(TypeId),
}
