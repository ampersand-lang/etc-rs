//! Contains the definition of a high-level value.

use std::fmt::{self, Display};

use crate::assets::{Handle, Resources};
use crate::lir::FuncId;
use crate::types::{self, NamedType, TypeId};

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

/// The configuration for pretty-printing a type.
#[derive(Default, Debug, Clone)]
pub struct PrettyConfig {}

/// Wraps a payload in a `Debug`- and `Display`-implementing structure, that can also query `TypeId`s
pub struct PrettyPrinter<'res> {
    /// The global configuration for this type.
    config: PrettyConfig,
    /// Immutable access to types.
    types: Resources<&'res NamedType>,
    /// Immutable access to types.
    strings: Resources<&'res String>,
    /// Root type id.
    payload: Payload,
}

impl<'res> PrettyPrinter<'res> {
    /// Initializes a pretty-printer with all the fields.
    pub fn new(
        config: PrettyConfig,
        types: Resources<&'res NamedType>,
        strings: Resources<&'res String>,
        payload: Payload,
    ) -> Self {
        Self {
            config,
            types,
            strings,
            payload,
        }
    }

    /// Initializes a pretty-printer with a default config.
    pub fn with_default(
        types: Resources<&'res NamedType>,
        strings: Resources<&'res String>,
        payload: Payload,
    ) -> Self {
        Self::new(Default::default(), types, strings, payload)
    }

    #[allow(missing_docs)]
    pub(self) fn as_ref(&self) -> PrettyPrinterRef<'_, 'res> {
        PrettyPrinterRef {
            config: self.config.clone(),
            types: &self.types,
            strings: &self.strings,
            payload: self.payload,
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
#[allow(dead_code)]
pub(crate) struct PrettyPrinterRef<'a, 'res> {
    config: PrettyConfig,
    types: &'a Resources<&'res NamedType>,
    strings: &'a Resources<&'res String>,
    payload: Payload,
}

#[doc(hidden)]
#[allow(missing_docs)]
impl<'a, 'res> PrettyPrinterRef<'a, 'res> {
    pub fn new(
        config: PrettyConfig,
        types: &'a Resources<&'res NamedType>,
        strings: &'a Resources<&'res String>,
        payload: Payload,
    ) -> Self {
        Self {
            config,
            types,
            strings,
            payload,
        }
    }
}

impl<'a, 'res> Display for PrettyPrinterRef<'a, 'res> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.payload {
            Payload::Unit => write!(f, "()"),
            Payload::Integer(u) => write!(f, "{}", u),
            Payload::Float(x) => write!(f, "{:?}", x),
            Payload::String(handle) => write!(
                f,
                "{:?}",
                self.strings.get::<String>(handle).unwrap().as_ref()
            ),
            Payload::Identifier(handle) => write!(
                f,
                "{}",
                self.strings.get::<String>(handle).unwrap().as_ref()
            ),
            Payload::Type(typ) => write!(
                f,
                "{}",
                types::PrettyPrinterRef::new(&Default::default(), self.types, typ)
            ),
            Payload::Function(func) => write!(f, "{:x}", func),
        }
    }
}
