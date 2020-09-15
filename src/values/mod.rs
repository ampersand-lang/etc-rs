use crate::assets::Handle;
use crate::lir::GlobId;
use crate::types::TypeId;

pub type FuncId = GlobId;

#[derive(Debug, Clone, Copy)]
pub enum Payload {
    Unit,
    Integer(u64),
    Float(f64),
    String(Handle<String>),
    Identifier(Handle<String>),
    Type(TypeId),
    Function(FuncId),
}
