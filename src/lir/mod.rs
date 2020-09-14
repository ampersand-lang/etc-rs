use smallvec::SmallVec;

use crate::assets::Handle;
use crate::ast::NodeId;
use crate::types::TypeId;

pub mod builder;
pub mod compile;
pub mod context;
pub mod repr;
pub mod target;

pub type ThreadId = Handle<context::ExecutionContext>;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Binding(pub(crate) usize, pub(crate) i32);
pub type GlobId = usize;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypedValue {
    pub typ: TypeId,
    pub val: Value,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Value {
    Unit,
    Global(GlobId),
    Unref(context::VirtualAddress),
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

#[cfg(test)]
mod tests {
    use crate::assets::*;
    use crate::types::*;

    use super::context::*;
    use super::target::*;
    use super::*;

    #[test]
    fn sanity() {
        let world = World::new();
        world.init_asset::<NamedType>();

        let mut lazy = LazyUpdate::new();

        let mut main = 0;
        let (res, mut ctx) =
            ExecutionContext::builder(world.resources::<&NamedType>(), Target::default())
                .function()
                .result(*primitive::SINT)
                .build_return(Value::Uint(5))
                .build(&mut main)
                .build();
        assert_eq!(
            ctx.call(&mut lazy, &res, main, &[]).unwrap(),
            Value::Uint(5)
        );
    }
}
