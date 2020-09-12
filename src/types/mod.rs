use smallvec::SmallVec;

use crate::assets::Handle;

#[derive(Debug, Clone, Copy)]
pub struct TypeId {
    pub group: TypeGroup,
    pub concrete: TypeOrPlaceholder,
}

#[derive(Debug, Clone, Copy)]
pub enum TypeOrPlaceholder {
    Type(Handle<NamedType>),
    Placeholder(Handle<String>),
}

#[derive(Debug, Clone, Copy)]
pub enum TypeGroup {
    Int,
    Float,
    Struct,
    Tagged,
    Enum,
    Union,
    Function,
    Pointer,
    Array,
    Slice,
}

#[derive(Debug, Clone)]
pub struct NamedType {
    pub name: Option<String>,
    pub t: Type,
}

#[derive(Debug, Clone)]
pub enum Type {
    S8,
    S16,
    S32,
    S64,
    Sint,
    U8,
    U16,
    U32,
    U64,
    Uint,
    Float32,
    Float64,
    Float,
    Struct {
        fields: SmallVec<[TypeId; 4]>,
    },
    Tagged {
        fields: SmallVec<[TypeId; 4]>,
        variants: SmallVec<[SmallVec<[TypeId; 4]>; 4]>,
    },
    Enum {
        width: usize,
    },
    Union {
        fields: SmallVec<[TypeId; 4]>,
    },
    Function {
        result_type: TypeId,
        parameters: SmallVec<[TypeId; 4]>,
    },
    Pointer(TypeId),
    Array(TypeId, usize),
    Slice(TypeId),
}
