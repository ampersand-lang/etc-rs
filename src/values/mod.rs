//! Contains the definition of a high-level value.
use crate::assets::Handle;
use crate::lir::GlobId;
use crate::types::TypeId;

/// Alias for lir function identifiers.
pub type FuncId = GlobId;

/// High-level value of an ast node.
#[derive(Debug, Clone, Copy)]
pub enum Payload {
    /// The empty tuple.
    Unit,
    /// Any non-negative integer.
    Integer(u64),
    /// Any (non-negative) real number according to IEEE-754.
    Float(f64),
    /// Any "-delimited string.
    String(Handle<String>),
    /// Any identifier.
    Identifier(Handle<String>),
    /// Any type, type placeholder or dispatch result.
    Type(TypeId),
    /// Any lir function.
    Function(FuncId),
}
