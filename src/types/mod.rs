use std::convert::TryFrom;

use smallvec::SmallVec;
use num_enum::{TryFromPrimitive, IntoPrimitive};

use crate::assets::{Handle, Resources};
use crate::lir::target::Target;
use crate::lir::repr::{Repr, ReprExt};

pub mod primitive {
    use lazy_static::lazy_static;

    use super::*;
    
    lazy_static! {
        pub static ref UNIT: TypeId = {
            TypeId {
                group: TypeGroup::Struct,
                concrete: TypeOrPlaceholder::Type(Handle::new()),
            }
        };
        
        pub static ref TYPE: TypeId = {
            TypeId {
                group: TypeGroup::Type,
                concrete: TypeOrPlaceholder::Type(Handle::new()),
            }
        };
        
        pub static ref S8: TypeId = {
            TypeId {
                group: TypeGroup::Int,
                concrete: TypeOrPlaceholder::Type(Handle::new()),
            }
        };
        
        pub static ref S16: TypeId = {
            TypeId {
                group: TypeGroup::Int,
                concrete: TypeOrPlaceholder::Type(Handle::new()),
            }
        };
        
        pub static ref S32: TypeId = {
            TypeId {
                group: TypeGroup::Int,
                concrete: TypeOrPlaceholder::Type(Handle::new()),
            }
        };
        
        pub static ref S64: TypeId = {
            TypeId {
                group: TypeGroup::Int,
                concrete: TypeOrPlaceholder::Type(Handle::new()),
            }
        };
        
        pub static ref SINT: TypeId = {
            TypeId {
                group: TypeGroup::Int,
                concrete: TypeOrPlaceholder::Type(Handle::new()),
            }
        };
        
        pub static ref U8: TypeId = {
            TypeId {
                group: TypeGroup::Int,
                concrete: TypeOrPlaceholder::Type(Handle::new()),
            }
        };
        
        pub static ref U16: TypeId = {
            TypeId {
                group: TypeGroup::Int,
                concrete: TypeOrPlaceholder::Type(Handle::new()),
            }
        };
        
        pub static ref U32: TypeId = {
            TypeId {
                group: TypeGroup::Int,
                concrete: TypeOrPlaceholder::Type(Handle::new()),
            }
        };
        
        pub static ref U64: TypeId = {
            TypeId {
                group: TypeGroup::Int,
                concrete: TypeOrPlaceholder::Type(Handle::new()),
            }
        };
        
        pub static ref UINT: TypeId = {
            TypeId {
                group: TypeGroup::Int,
                concrete: TypeOrPlaceholder::Type(Handle::new()),
            }
        };
        
        pub static ref FLOAT32: TypeId = {
            TypeId {
                group: TypeGroup::Float,
                concrete: TypeOrPlaceholder::Type(Handle::new()),
            }
        };
        
        pub static ref FLOAT64: TypeId = {
            TypeId {
                group: TypeGroup::Float,
                concrete: TypeOrPlaceholder::Type(Handle::new()),
            }
        };
        
        pub static ref FLOAT: TypeId = {
            TypeId {
                group: TypeGroup::Float,
                concrete: TypeOrPlaceholder::Type(Handle::new()),
            }
        };
    }
}

#[derive(Debug, Clone, Copy)]
pub struct TypeInfo {
    pub size: usize,
    pub align: usize,
}

impl TypeInfo {
    pub const fn new(size: usize, align: usize) -> Self {
        Self { size, align }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct TypeId {
    pub group: TypeGroup,
    pub concrete: TypeOrPlaceholder,
}

impl TypeId {
    fn size_of(&self, res: &Resources<&NamedType>, target: &Target) -> Option<usize> {
        match self.concrete {
            TypeOrPlaceholder::Type(handle) => {
                let t = res.get::<NamedType>(handle).unwrap();
                match t.t {
                    Type::S8 => Some(1),
                    Type::S16 => Some(2),
                    Type::S32 => Some(4),
                    Type::S64 => Some(8),
                    Type::Sint => Some(target.pointer_width / 8),
                    Type::U8 => Some(1),
                    Type::U16 => Some(2),
                    Type::U32 => Some(4),
                    Type::U64 => Some(8),
                    Type::Uint => Some(target.pointer_width / 8),
                    Type::Float32 => Some(4),
                    Type::Float64 => Some(8),
                    Type::Float => Some(target.pointer_width / 8),
                    // TODO: alignment rules
                    // TODO: None-returns
                    Type::Struct { ref fields } => Some(fields.iter().map(|t| t.size_of(res, target).unwrap_or(0)).sum::<usize>()),
                    Type::Tagged { .. } => todo!(),
                    Type::Enum { width } => Some(width / 8),
                    Type::Union { ref fields } => Some(fields.iter().map(|t| t.size_of(res, target).unwrap_or(0)).max().unwrap_or(0)),
                    Type::Function { .. } => Some(target.pointer_width / 8),
                    Type::Pointer(..) => Some(target.pointer_width / 8),
                    Type::Array(t, n) => t.size_of(res, target).map(|size| size * n),
                    Type::Slice(..) => Some(target.pointer_width / 8 * 2),
                }
            }
            _ => None,
        }
    }
    
    fn align_of(&self, res: &Resources<&NamedType>, target: &Target) -> Option<usize> {
        match self.concrete {
            TypeOrPlaceholder::Type(handle) => {
                let t = res.get::<NamedType>(handle).unwrap();
                match t.t {
                    Type::S8 => Some(1),
                    Type::S16 => Some(2),
                    Type::S32 => Some(4),
                    Type::S64 => Some(8),
                    Type::Sint => Some(target.pointer_align / 8),
                    Type::U8 => Some(1),
                    Type::U16 => Some(2),
                    Type::U32 => Some(4),
                    Type::U64 => Some(8),
                    Type::Uint => Some(target.pointer_align / 8),
                    Type::Float32 => Some(4),
                    Type::Float64 => Some(8),
                    Type::Float => Some(target.pointer_align / 8),
                    // TODO: alignment rules
                    // TODO: None-returns
                    Type::Struct { ref fields } => fields.get(0).map(|t| t.size_of(res, target).unwrap_or(1)),
                    Type::Tagged { .. } => todo!(),
                    Type::Enum { width } => Some(width / 8),
                    Type::Union { ref fields } => Some(fields.iter().map(|t| t.size_of(res, target).unwrap_or(1)).max().unwrap_or(1)),
                    Type::Function { .. } => Some(target.pointer_align / 8),
                    Type::Pointer(..) => Some(target.pointer_align / 8),
                    Type::Array(t, n) => t.size_of(res, target).map(|size| size * n),
                    Type::Slice(..) => Some(target.pointer_align / 8 * 2),
                }
            }
            _ => None,
        }
    }
    
    pub fn type_info(&self, res: &Resources<&NamedType>, target: &Target) -> TypeInfo {
        TypeInfo {
            size: self.size_of(res, target).unwrap_or(0),
            align: self.align_of(res, target).unwrap_or(1),
        }
    }

    pub fn matches(&self, other: &TypeId) -> bool {
        match (self.group, other.group) {
            (_, TypeGroup::None) => return true,
            (TypeGroup::Type, TypeGroup::Type) => return true,
            (TypeGroup::Int, TypeGroup::Int) => return true,
            (TypeGroup::Float, TypeGroup::Float) => return true,
            _ => {}
        }

        match (self.concrete, other.concrete) {
            // TODO: deep matches
            (TypeOrPlaceholder::Type(a), TypeOrPlaceholder::Type(b)) => a == b,
            _ => true,
        }
    }
}

impl Repr for TypeId {
    fn type_info(&self) -> TypeInfo {
        let t = match self.concrete {
            TypeOrPlaceholder::Type(t) => t,
            _ => panic!("placeholder doesn't have type info"),
        };
        let tuple: (u8, u128) = (self.group.into(), t.as_u128());
        tuple.type_info()
    }
    
    fn write_bytes(&self, out: &mut [u8]) {
        let t = match self.concrete {
            TypeOrPlaceholder::Type(t) => t,
            _ => panic!("placeholder doesn't have type info"),
        };
        let tuple: (u8, u128) = (self.group.into(), t.as_u128());
        tuple.write_bytes(out);
    }
    
    fn copy_from_bytes(&mut self, _: &[u8]) {
        panic!("TypeId is immutable");
    }
}

impl ReprExt for TypeId {
    fn static_type_info() -> TypeInfo {
        <(u8, u128)>::static_type_info()
    }
    
    fn from_bytes(bytes: &[u8]) -> Self {
        let tuple = <(u8, u128)>::from_bytes(bytes);
        Self {
            group: TypeGroup::try_from(tuple.0).expect("invalid TypeId"),
            concrete: TypeOrPlaceholder::Type(Handle::from_u128(tuple.1)),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum TypeOrPlaceholder {
    Type(Handle<NamedType>),
    Placeholder(Handle<String>),
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, TryFromPrimitive, IntoPrimitive)]
pub enum TypeGroup {
    None,
    Type,
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
