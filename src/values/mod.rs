use crate::assets::Handle;
use crate::lir::GlobId;
use crate::types::TypeId;

pub type FuncId = GlobId;
pub type ValueId = Handle<Value>;

#[derive(Debug, Clone)]
pub enum Value {
    Unit,
    Int(u64),
    Float(f64),
    String(String),
    Identifier(String),
    Type(TypeId),
    Function(FuncId),
}
