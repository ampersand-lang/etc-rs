use std::convert::TryFrom;
use std::fmt::{self, Display};

use num_enum::{IntoPrimitive, TryFromPrimitive};
use smallvec::SmallVec;

use crate::assets::{Handle, Resources};
use crate::lir::repr::{Repr, ReprExt};
use crate::lir::target::Target;
use crate::scope::ScopeId;
use crate::utils::IntPtr;

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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TypeId {
    pub group: TypeGroup,
    pub concrete: TypeOrPlaceholder,
}

impl TypeId {
    pub fn new_placeholder() -> Self {
        Self {
            group: TypeGroup::None,
            // XXX: is it correct to put a random handle here?
            concrete: TypeOrPlaceholder::Placeholder(Handle::new()),
        }
    }

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
                    Type::Struct { ref fields } => {
                        let mut size = 0;
                        let mut align = 1;
                        for t in fields {
                            size = size.align_up(align);
                            size += t.size_of(res, target)?;
                            align = t.align_of(res, target)?;
                        }
                        Some(size)
                    }
                    Type::Tagged { .. } => todo!(),
                    Type::Enum { width } => Some(width / 8),
                    Type::Union { ref fields } => Some(
                        fields
                            .iter()
                            .map(|t| t.size_of(res, target).unwrap_or(0))
                            .max()
                            .unwrap_or(0),
                    ),
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
                    Type::Struct { ref fields } => fields
                        .get(0)
                        .map(|t| t.align_of(res, target))
                        .unwrap_or(Some(1)),
                    Type::Tagged { .. } => todo!(),
                    Type::Enum { width } => Some(width / 8),
                    Type::Union { ref fields } => Some(
                        fields
                            .iter()
                            .map(|t| t.size_of(res, target).unwrap_or(1))
                            .max()
                            .unwrap_or(1),
                    ),
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

    pub fn matches(&self, other: &TypeId, res: &Resources<&NamedType>) -> bool {
        match (self.group, other.group) {
            (_, TypeGroup::None) => return true,
            (TypeGroup::Type, TypeGroup::Type) => return true,
            (TypeGroup::Int, TypeGroup::Int) => return true,
            (TypeGroup::Float, TypeGroup::Float) => return true,
            _ => {}
        }

        match (self.concrete, other.concrete) {
            (TypeOrPlaceholder::Type(a), TypeOrPlaceholder::Type(b)) => a.matches(b, res),
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TypeOrPlaceholder {
    Type(Handle<NamedType>),
    Placeholder(Handle<String>),
    Dispatch(ScopeId, Handle<String>),
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, TryFromPrimitive, IntoPrimitive)]
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

trait Matches {
    fn matches(&self, other: &Self, res: &Resources<&NamedType>) -> bool;
}

impl Matches for SmallVec<[TypeId; 4]> {
    fn matches(&self, other: &Self, res: &Resources<&NamedType>) -> bool {
        if self.len() != other.len() {
            return false;
        }

        for (a, b) in self.iter().zip(other) {
            if !a.matches(b, res) {
                return false;
            }
        }
        true
    }
}

impl Handle<NamedType> {
    pub fn matches(self, other: Self, res: &Resources<&NamedType>) -> bool {
        let this = res.get::<NamedType>(self).unwrap();
        let other = res.get::<NamedType>(other).unwrap();
        match (&this.t, &other.t) {
            (Type::S8, Type::S8) => true,
            (Type::S16, Type::S16) => true,
            (Type::S32, Type::S32) => true,
            (Type::S64, Type::S64) => true,
            (Type::Sint, Type::Sint) => true,
            (Type::U8, Type::U8) => true,
            (Type::U16, Type::U16) => true,
            (Type::U32, Type::U32) => true,
            (Type::U64, Type::U64) => true,
            (Type::Uint, Type::Uint) => true,
            (Type::Float32, Type::Float32) => true,
            (Type::Float64, Type::Float64) => true,
            (Type::Float, Type::Float) => true,
            (
                Type::Struct {
                    fields: ref a_fields,
                },
                Type::Struct {
                    fields: ref b_fields,
                },
            ) => this.name == other.name && a_fields.matches(b_fields, res),
            (Type::Enum { width: a_width }, Type::Enum { width: b_width }) => {
                this.name == other.name && a_width == b_width
            }
            (
                Type::Union {
                    fields: ref a_fields,
                },
                Type::Union {
                    fields: ref b_fields,
                },
            ) => this.name == other.name && a_fields.matches(b_fields, res),
            (
                Type::Function {
                    result_type: ref a_result,
                    param_types: ref a_fields,
                },
                Type::Function {
                    result_type: ref b_result,
                    param_types: ref b_fields,
                },
            ) => a_result.matches(b_result, res) && a_fields.matches(b_fields, res),
            (Type::Pointer(a), Type::Pointer(b)) => a.matches(&b, res),
            (Type::Array(a, al), Type::Array(b, bl)) => al == bl && a.matches(&b, res),
            (Type::Slice(a), Type::Slice(b)) => a.matches(&b, res),
            _ => false,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NamedType {
    pub name: Option<String>,
    pub t: Type,
}

#[derive(Debug, Clone, PartialEq, Eq)]
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
        param_types: SmallVec<[TypeId; 4]>,
    },
    Pointer(TypeId),
    Array(TypeId, usize),
    Slice(TypeId),
}

/// The configuration for pretty-printing a type.
#[derive(Default, Debug, Clone)]
pub struct PrettyConfig {}

/// Wraps a type in a `Debug`- and `Display`-implementing structure, that can also query `TypeId`s
pub struct PrettyPrinter<'res> {
    /// The global configuration for this type.
    config: PrettyConfig,
    /// Immutable access to types.
    res: Resources<&'res NamedType>,
    /// Root type id.
    id: TypeId,
}

impl<'res> PrettyPrinter<'res> {
    /// Initializes a pretty-printer with all the fields.
    pub fn new(config: PrettyConfig, res: Resources<&'res NamedType>, id: TypeId) -> Self {
        Self { config, res, id }
    }

    /// Initializes a pretty-printer with a default config.
    pub fn with_default(res: Resources<&'res NamedType>, id: TypeId) -> Self {
        Self {
            config: Default::default(),
            res,
            id,
        }
    }

    #[allow(missing_docs)]
    pub(self) fn as_ref(&self) -> PrettyPrinterRef<'_, 'res> {
        PrettyPrinterRef {
            config: &self.config,
            res: &self.res,
            id: self.id,
        }
    }
}

impl<'res> Display for PrettyPrinter<'res> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.as_ref().fmt(f)
    }
}

#[doc(hidden)]
#[allow(missing_docs)]
struct PrettyPrinterRef<'a, 'res> {
    config: &'a PrettyConfig,
    res: &'a Resources<&'res NamedType>,
    id: TypeId,
}

#[doc(hidden)]
#[allow(missing_docs)]
impl<'a, 'res> PrettyPrinterRef<'a, 'res> {
    pub fn new(config: &'a PrettyConfig, res: &'a Resources<&'res NamedType>, id: TypeId) -> Self {
        Self { config, res, id }
    }
}

impl<'a, 'res> Display for PrettyPrinterRef<'a, 'res> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.id.concrete {
            TypeOrPlaceholder::Type(t) => {
                let ty = self.res.get::<NamedType>(t).unwrap();
                if let Some(name) = &ty.name {
                    write!(f, "{}", name)
                } else {
                    match &ty.t {
    			Type::S8 => write!(f, "s8"),
    			Type::S16 => write!(f, "s16"),
    			Type::S32 => write!(f, "s32"),
    			Type::S64 => write!(f, "s64"),
    			Type::Sint => write!(f, "sint"),
    			Type::U8 => write!(f, "u8"),
    			Type::U16 => write!(f, "u16"),
    			Type::U32 => write!(f, "u32"),
    			Type::U64 => write!(f, "u64"),
    			Type::Uint => write!(f, "uint"),
    			Type::Float32 => write!(f, "float32"),
    			Type::Float64 => write!(f, "float64"),
    			Type::Float => write!(f, "float"),
    			Type::Struct { ..} => todo!(),
                        Type::Tagged { .. } => todo!(),
                        Type::Enum { .. } => todo!(),
                        Type::Union { .. } => todo!(),
                        Type::Function { .. } => todo!(),
                        Type::Pointer(pointee) => write!(f, "^{}", PrettyPrinterRef::new(self.config, self.res, *pointee)),
                        Type::Array(elem, size) => write!(f, "$[{}; {}]", PrettyPrinterRef::new(self.config, self.res, *elem), size),
                        Type::Slice(elem) => write!(f, "$[{}]", PrettyPrinterRef::new(self.config, self.res, *elem)),
                    }
                }
            }
            _ => Err(fmt::Error),
        }
    }
}
