use std::convert::TryFrom;
use std::fmt::{self, Display};

use num_enum::{IntoPrimitive, TryFromPrimitive};

use crate::assets::{AssetBundle, Handle, Resources};
use crate::ast::NodeId;
use crate::lir::repr::{Repr, ReprExt};
use crate::lir::target::Target;
use crate::scope::ScopeId;
use crate::utils::IntPtr;

pub use assoc::*;

mod assoc;

pub mod primitive {
    use lazy_static::lazy_static;

    use super::*;

    pub fn init(mut res: Resources<&mut NamedType>) {
        res.insert(
            UNIT.concrete.to_type(),
            NamedType {
                name: Some("unit".to_string()),
                t: Type::Struct {
                    fields: Assoc::new(),
                },
            },
        );

        res.insert(
            BOOL.concrete.to_type(),
            NamedType {
                name: Some("bool".to_string()),
                t: Type::Bool,
            },
        );

        res.insert(
            TYPE.concrete.to_type(),
            NamedType {
                name: Some("type".to_string()),
                t: Type::Struct {
                    fields: Assoc::new()
                        .with("kind", *U8)
                        .with("id1", *U64)
                        .with("id2", *U64),
                },
            },
        );

        res.insert(
            NODE.concrete.to_type(),
            NamedType {
                name: Some("node".to_string()),
                t: Type::Struct {
                    fields: Assoc::new().with("id1", *U64).with("id2", *U64),
                },
            },
        );

        res.insert(
            S8.concrete.to_type(),
            NamedType {
                name: Some("s8".to_string()),
                t: Type::S8,
            },
        );

        res.insert(
            S16.concrete.to_type(),
            NamedType {
                name: Some("s16".to_string()),
                t: Type::S16,
            },
        );

        res.insert(
            S32.concrete.to_type(),
            NamedType {
                name: Some("s32".to_string()),
                t: Type::S32,
            },
        );

        res.insert(
            S64.concrete.to_type(),
            NamedType {
                name: Some("s64".to_string()),
                t: Type::S64,
            },
        );

        res.insert(
            SINT.concrete.to_type(),
            NamedType {
                name: Some("sint".to_string()),
                t: Type::Sint,
            },
        );

        res.insert(
            U8.concrete.to_type(),
            NamedType {
                name: Some("u8".to_string()),
                t: Type::U8,
            },
        );

        res.insert(
            U16.concrete.to_type(),
            NamedType {
                name: Some("u16".to_string()),
                t: Type::U16,
            },
        );

        res.insert(
            U32.concrete.to_type(),
            NamedType {
                name: Some("u32".to_string()),
                t: Type::U32,
            },
        );

        res.insert(
            U64.concrete.to_type(),
            NamedType {
                name: Some("u64".to_string()),
                t: Type::U64,
            },
        );

        res.insert(
            UINT.concrete.to_type(),
            NamedType {
                name: Some("uint".to_string()),
                t: Type::Uint,
            },
        );

        res.insert(
            FLOAT32.concrete.to_type(),
            NamedType {
                name: Some("float32".to_string()),
                t: Type::Float32,
            },
        );

        res.insert(
            FLOAT64.concrete.to_type(),
            NamedType {
                name: Some("float64".to_string()),
                t: Type::Float64,
            },
        );

        res.insert(
            FLOAT.concrete.to_type(),
            NamedType {
                name: Some("float".to_string()),
                t: Type::Float,
            },
        );

        res.insert(
            UNIT_BUILDER.concrete.to_type(),
            NamedType {
                name: Some("builder".to_string()),
                t: Type::Function {
                    result_type: *primitive::UNIT,
                    param_types: Assoc::new(),
                },
            },
        );

        res.insert(
            NODE_BUILDER.concrete.to_type(),
            NamedType {
                name: Some("builder".to_string()),
                t: Type::Function {
                    result_type: *primitive::NODE,
                    param_types: Assoc::new(),
                },
            },
        );

        res.insert(
            PTR_BUILDER.concrete.to_type(),
            NamedType {
                name: Some("ptr".to_string()),
                t: Type::Function {
                    result_type: *primitive::TYPE,
                    param_types: Assoc::new().with("pointee", *primitive::TYPE),
                },
            },
        );

        res.insert(
            METATYPE.concrete.to_type(),
            NamedType {
                name: None,
                t: Type::Function {
                    result_type: *primitive::TYPE,
                    param_types: Assoc::new(),
                },
            },
        );
    }

    lazy_static! {
        pub static ref UNIT: TypeId = {
            TypeId {
                group: TypeGroup::Struct,
                concrete: NonConcrete::Type(Handle::new()),
            }
        };
        pub static ref BOOL: TypeId = {
            TypeId {
                group: TypeGroup::Bool,
                concrete: NonConcrete::Type(Handle::new()),
            }
        };
        pub static ref TYPE: TypeId = {
            TypeId {
                group: TypeGroup::Type,
                concrete: NonConcrete::Type(Handle::new()),
            }
        };
        pub static ref NODE: TypeId = {
            TypeId {
                group: TypeGroup::Node,
                concrete: NonConcrete::Type(Handle::new()),
            }
        };
        pub static ref S8: TypeId = {
            TypeId {
                group: TypeGroup::Int,
                concrete: NonConcrete::Type(Handle::new()),
            }
        };
        pub static ref S16: TypeId = {
            TypeId {
                group: TypeGroup::Int,
                concrete: NonConcrete::Type(Handle::new()),
            }
        };
        pub static ref S32: TypeId = {
            TypeId {
                group: TypeGroup::Int,
                concrete: NonConcrete::Type(Handle::new()),
            }
        };
        pub static ref S64: TypeId = {
            TypeId {
                group: TypeGroup::Int,
                concrete: NonConcrete::Type(Handle::new()),
            }
        };
        pub static ref SINT: TypeId = {
            TypeId {
                group: TypeGroup::Int,
                concrete: NonConcrete::Type(Handle::new()),
            }
        };
        pub static ref U8: TypeId = {
            TypeId {
                group: TypeGroup::Int,
                concrete: NonConcrete::Type(Handle::new()),
            }
        };
        pub static ref U16: TypeId = {
            TypeId {
                group: TypeGroup::Int,
                concrete: NonConcrete::Type(Handle::new()),
            }
        };
        pub static ref U32: TypeId = {
            TypeId {
                group: TypeGroup::Int,
                concrete: NonConcrete::Type(Handle::new()),
            }
        };
        pub static ref U64: TypeId = {
            TypeId {
                group: TypeGroup::Int,
                concrete: NonConcrete::Type(Handle::new()),
            }
        };
        pub static ref UINT: TypeId = {
            TypeId {
                group: TypeGroup::Int,
                concrete: NonConcrete::Type(Handle::new()),
            }
        };
        pub static ref FLOAT32: TypeId = {
            TypeId {
                group: TypeGroup::Float,
                concrete: NonConcrete::Type(Handle::new()),
            }
        };
        pub static ref FLOAT64: TypeId = {
            TypeId {
                group: TypeGroup::Float,
                concrete: NonConcrete::Type(Handle::new()),
            }
        };
        pub static ref FLOAT: TypeId = {
            TypeId {
                group: TypeGroup::Float,
                concrete: NonConcrete::Type(Handle::new()),
            }
        };
        pub static ref UNIT_BUILDER: TypeId = {
            TypeId {
                group: TypeGroup::Function,
                concrete: NonConcrete::Type(Handle::new()),
            }
        };
        pub static ref NODE_BUILDER: TypeId = {
            TypeId {
                group: TypeGroup::Function,
                concrete: NonConcrete::Type(Handle::new()),
            }
        };
        pub static ref PTR_BUILDER: TypeId = {
            TypeId {
                group: TypeGroup::Function,
                concrete: NonConcrete::Type(Handle::new()),
            }
        };
        pub static ref METATYPE: TypeId = {
            TypeId {
                group: TypeGroup::Function,
                concrete: NonConcrete::Type(Handle::new()),
            }
        };
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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
    pub concrete: NonConcrete,
}

impl TypeId {
    pub fn unptr<A: AssetBundle>(&self, res: &Resources<A>) -> Option<TypeId> {
        match self.concrete {
            NonConcrete::Type(handle) => {
                let t = res.get::<NamedType>(handle).unwrap();
                match t.t {
                    Type::Pointer(t) => Some(t),
                    _ => None,
                }
            }
            _ => None,
        }
    }

    pub fn index_of<A: AssetBundle>(&self, res: &Resources<A>, field: &str) -> Option<usize> {
        match self.concrete {
            NonConcrete::Type(handle) => {
                let t = res.get::<NamedType>(handle).unwrap();
                match t.t {
                    Type::Bool => None,
                    Type::S8 => None,
                    Type::S16 => None,
                    Type::S32 => None,
                    Type::S64 => None,
                    Type::Sint => None,
                    Type::U8 => None,
                    Type::U16 => None,
                    Type::U32 => None,
                    Type::U64 => None,
                    Type::Uint => None,
                    Type::Float32 => None,
                    Type::Float64 => None,
                    Type::Float => None,
                    Type::Struct { ref fields } => fields.position(field),
                    Type::Tagged { .. } => todo!(),
                    Type::Enum { .. } => None,
                    Type::Union { .. } => None,
                    Type::Function { .. } => None,
                    Type::Constructor(_) => None,
                    Type::Pointer(..) => None,
                    Type::Array(_, _) => None,
                    Type::Slice(_) => match field {
                        "ptr" => Some(0),
                        "len" => Some(1),
                        _ => None,
                    },
                }
            }
            _ => None,
        }
    }

    pub fn offset_of<A: AssetBundle>(
        &self,
        res: &Resources<A>,
        target: &Target,
        field: usize,
    ) -> Option<usize> {
        match self.concrete {
            NonConcrete::Type(handle) => {
                let t = res.get::<NamedType>(handle).unwrap();
                match t.t {
                    Type::Bool => Some(0),
                    Type::S8 => Some(0),
                    Type::S16 => Some(0),
                    Type::S32 => Some(0),
                    Type::S64 => Some(0),
                    Type::Sint => Some(0),
                    Type::U8 => Some(0),
                    Type::U16 => Some(0),
                    Type::U32 => Some(0),
                    Type::U64 => Some(0),
                    Type::Uint => Some(0),
                    Type::Float32 => Some(0),
                    Type::Float64 => Some(0),
                    Type::Float => Some(0),
                    Type::Struct { ref fields } => {
                        if field >= fields.len() {
                            return None;
                        }
                        let mut offset = 0;
                        let mut align = 1;
                        for t in &fields[..field] {
                            offset = offset.align_up(align);
                            offset += t.size_of(res, target)?;
                            align = t.align_of(res, target)?;
                        }
                        Some(offset)
                    }
                    Type::Tagged { .. } => todo!(),
                    Type::Enum { .. } => Some(0),
                    Type::Union { .. } => Some(0),
                    Type::Function { .. } => Some(0),
                    Type::Constructor(_) => Some(0),
                    Type::Pointer(..) => Some(0),
                    Type::Array(t, _) => t.size_of(res, target).map(|size| size * field),
                    Type::Slice(_) => match field {
                        0 => Some(0),
                        1 => Some(target.pointer_width),
                        _ => None,
                    },
                }
            }
            _ => None,
        }
    }

    pub fn size_of<A: AssetBundle>(&self, res: &Resources<A>, target: &Target) -> Option<usize> {
        match self.concrete {
            NonConcrete::Type(handle) => {
                let t = res.get::<NamedType>(handle).unwrap();
                match t.t {
                    Type::Bool => Some(1),
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
                        let mut align;
                        for t in fields.values() {
                            align = t.align_of(res, target)?;
                            size = size.align_up(align);
                            size += t.size_of(res, target)?;
                        }
                        Some(size)
                    }
                    Type::Tagged { .. } => todo!(),
                    Type::Enum { width } => Some(width / 8),
                    Type::Union { ref fields } => Some(
                        fields
                            .values()
                            .map(|t| t.size_of(res, target).unwrap_or(0))
                            .max()
                            .unwrap_or(0),
                    ),
                    Type::Function { .. } => Some(target.pointer_width / 8),
                    Type::Constructor(_) => None,
                    Type::Pointer(..) => Some(target.pointer_width / 8),
                    Type::Array(t, n) => t.size_of(res, target).map(|size| size * n),
                    Type::Slice(..) => Some(target.pointer_width / 8 * 2),
                }
            }
            _ => None,
        }
    }

    pub fn align_of<A: AssetBundle>(&self, res: &Resources<A>, target: &Target) -> Option<usize> {
        match self.concrete {
            NonConcrete::Type(handle) => {
                let t = res.get::<NamedType>(handle).unwrap();
                match t.t {
                    Type::Bool => Some(1),
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
                        .nth(0)
                        .map(|t| t.align_of(res, target))
                        .unwrap_or(Some(1)),
                    Type::Tagged { .. } => todo!(),
                    Type::Enum { width } => Some(width / 8),
                    Type::Union { ref fields } => Some(
                        fields
                            .values()
                            .map(|t| t.size_of(res, target).unwrap_or(1))
                            .max()
                            .unwrap_or(1),
                    ),
                    Type::Function { .. } => Some(target.pointer_align / 8),
                    Type::Constructor(_) => None,
                    Type::Pointer(..) => Some(target.pointer_align / 8),
                    Type::Array(t, n) => t.size_of(res, target).map(|size| size * n),
                    Type::Slice(..) => Some(target.pointer_align / 8 * 2),
                }
            }
            _ => None,
        }
    }

    pub fn type_info<A: AssetBundle>(&self, res: &Resources<A>, target: &Target) -> TypeInfo {
        TypeInfo {
            size: self.size_of(res, target).unwrap_or(0),
            align: self.align_of(res, target).unwrap_or(1),
        }
    }

    pub fn matches<A: AssetBundle>(&self, other: &TypeId, res: &Resources<A>) -> bool {
        match (self.group, other.group) {
            (_, TypeGroup::None) => return true,
            (TypeGroup::Bool, TypeGroup::Bool) => return true,
            (TypeGroup::Type, TypeGroup::Type) => return true,
            (TypeGroup::Int, TypeGroup::Int) => return true,
            (TypeGroup::Float, TypeGroup::Float) => return true,
            _ => {}
        }

        match (self.concrete, other.concrete) {
            (NonConcrete::Type(a), NonConcrete::Type(b)) => a.matches(b, res),
            _ => true,
        }
    }
}

impl Repr for TypeId {
    fn type_info(&self) -> TypeInfo {
        let t = match self.concrete {
            NonConcrete::Type(t) => t,
            _ => panic!("placeholder doesn't have type info"),
        };
        let tuple: (u8, u128) = (self.group.into(), t.as_u128());
        tuple.type_info()
    }

    fn write_bytes(&self, out: &mut [u8]) {
        let t = match self.concrete {
            NonConcrete::Type(t) => t,
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
            concrete: NonConcrete::Type(Handle::from_u128(tuple.1)),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NonConcrete {
    Type(Handle<NamedType>),
    Dispatch(ScopeId, Handle<String>),
    Typeof(NodeId),
}

impl NonConcrete {
    pub(self) fn to_type(&self) -> Handle<NamedType> {
        match self {
            Self::Type(t) => *t,
            _ => panic!("called to_type on something not a type"),
        }
    }
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, TryFromPrimitive, IntoPrimitive)]
pub enum TypeGroup {
    None,
    Bool,
    Type,
    Node,
    Int,
    Float,
    Struct,
    Tagged,
    Enum,
    Union,
    Function,
    Constructor,
    Pointer,
    Array,
    Slice,
}

trait Matches {
    fn matches<A: AssetBundle>(&self, other: &Self, res: &Resources<A>) -> bool;
}

impl Matches for Assoc<TypeId> {
    fn matches<A: AssetBundle>(&self, other: &Self, res: &Resources<A>) -> bool {
        if self.len() != other.len() {
            return false;
        }

        for (a, b) in self.values().zip(other.values()) {
            if !a.matches(b, res) {
                return false;
            }
        }
        true
    }
}

impl Handle<NamedType> {
    pub fn matches<A: AssetBundle>(self, other: Self, res: &Resources<A>) -> bool {
        let this = res.get::<NamedType>(self).unwrap();
        let other = res.get::<NamedType>(other).unwrap();
        match (&this.t, &other.t) {
            (Type::Bool, Type::Bool) => true,
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
    Bool,
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
        fields: Assoc<TypeId>,
    },
    Tagged {
        fields: Assoc<TypeId>,
        variants: Assoc<Assoc<TypeId>>,
    },
    Enum {
        width: usize,
    },
    Union {
        fields: Assoc<TypeId>,
    },
    Function {
        result_type: TypeId,
        param_types: Assoc<TypeId>,
    },
    Constructor(TypeId),
    Pointer(TypeId),
    Array(TypeId, usize),
    Slice(TypeId),
}

impl Type {
    pub fn is_signed(&self) -> bool {
        matches!(
            self,
            Type::S8 | Type::S16 | Type::S32 | Type::S64 | Type::Sint
        )
    }

    pub fn is_unsigned(&self) -> bool {
        matches!(
            self,
            Type::U8 | Type::U16 | Type::U32 | Type::U64 | Type::Uint
        )
    }

    pub fn is_bool(&self) -> bool {
        matches!(self, Type::Bool)
    }
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
pub(crate) struct PrettyPrinterRef<'a, 'res> {
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
            NonConcrete::Type(t) => {
                let ty = self.res.get::<NamedType>(t).unwrap();
                if let Some(name) = &ty.name {
                    write!(f, "{}", name)
                } else {
                    match &ty.t {
                        Type::Bool => write!(f, "bool"),
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
                        Type::Struct { .. } => todo!(),
                        Type::Tagged { .. } => todo!(),
                        Type::Enum { .. } => todo!(),
                        Type::Union { .. } => todo!(),
                        Type::Function {
                            result_type,
                            param_types,
                        } => {
                            write!(f, "(")?;
                            if let Some(param) = param_types.nth(0) {
                                write!(
                                    f,
                                    "{}",
                                    PrettyPrinterRef::new(self.config, self.res, *param)
                                )?;
                            }
                            for param in param_types.values().skip(1) {
                                write!(
                                    f,
                                    ", {}",
                                    PrettyPrinterRef::new(self.config, self.res, *param)
                                )?;
                            }
                            write!(
                                f,
                                ") -> {}",
                                PrettyPrinterRef::new(self.config, self.res, *result_type)
                            )
                        }
                        Type::Constructor(t) => {
                            write!(f, "cons ")?;
                            write!(f, "{}", PrettyPrinterRef::new(self.config, self.res, *t))
                        }
                        Type::Pointer(pointee) => write!(
                            f,
                            "^{}",
                            PrettyPrinterRef::new(self.config, self.res, *pointee)
                        ),
                        Type::Array(elem, size) => write!(
                            f,
                            "$[{}; {}]",
                            PrettyPrinterRef::new(self.config, self.res, *elem),
                            size
                        ),
                        Type::Slice(elem) => write!(
                            f,
                            "$[{}]",
                            PrettyPrinterRef::new(self.config, self.res, *elem)
                        ),
                    }
                }
            }
            NonConcrete::Dispatch(..) => write!(f, "<dispatch>"),
            NonConcrete::Typeof(..) => write!(f, "<typeof>"),
        }
    }
}
