use smallvec::SmallVec;

use crate::assets::Handle;
use crate::types::TypeId;
use crate::ast::NodeId;

pub mod builder;
pub mod compile;
pub mod context;
pub mod target;
pub mod repr;

pub type ThreadId = Handle<context::ExecutionContext>;

#[derive(Debug, Clone, Copy)]
pub struct Binding(pub(crate) usize, pub(crate) i32);
pub type GlobId = usize;

#[derive(Debug, Clone)]
pub struct TypedValue {
    pub typ: TypeId,
    pub val: Value,
}

#[derive(Debug, Clone)]
pub enum Value {
    Ref(Binding),
    Global(GlobId),
    Address(context::VirtualAddress),
    Uint(u64),
    Type(TypeId),
    Node(NodeId),
}

#[derive(Debug, Clone)]
pub enum Instruction {
    /// Two arguments: a type and a u64
    Alloca,
    /// Three arguments: a type, an address and anything
    Store,
    /// Two argument: a type and an address
    Load,
    /// At least one argument: a global
    Call,
    /// At least one argument: a u64 (node kind)
    NewNode,
    /// One argument: a u64 (scope id)
    Begin,
    End,
    /// One argument: any value
    Return,
}

#[derive(Debug, Clone)]
pub struct Ir {
    pub(crate) binding: Option<Binding>,
    pub(crate) instr: Instruction,
    pub(crate) args: SmallVec<[Value; 4]>,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub(crate) param_types: SmallVec<[TypeId; 4]>,
    pub(crate) result_type: TypeId,
    pub(crate) body: Vec<Ir>,
}

#[derive(Debug, Clone)]
pub enum Global {
    Function(Function),
    Constant(Value),
}
