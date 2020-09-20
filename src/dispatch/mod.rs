//! Contains primitives for querying and applying multiple-dispatch and overloaded functions.
use smallvec::SmallVec;

use crate::assets::{Handle, Resources};
use crate::scope::ScopeId;
use crate::types::{NamedType, TypeId};

/// Handle to a dispatcher.
///
/// Should be created with `DispatchId::from_name`.
pub type DispatchId = Handle<Dispatcher>;

/// A unique name.
///
/// Contains a scope and an identifier.
#[derive(Debug, Clone, Hash)]
pub struct Name(pub(crate) ScopeId, pub(crate) Handle<String>);

/// Defines if a `Query` should query functions.
#[derive(Debug, Clone, Copy)]
pub enum IsFunction {
    /// Query only functions.
    Yes,
    /// Query only non-functions.
    No,
    /// Query both functions and non-functions.
    Maybe,
}

/// A query into a single `Dispatcher`
#[derive(Debug, Clone)]
pub struct Query {
    name: Name,
    is_func: IsFunction,
    arg_types: Option<SmallVec<[TypeId; 4]>>,
    result_type: Option<TypeId>,
}

impl Query {
    /// Creates a new query with all fields.
    pub fn new(
        name: Name,
        is_func: IsFunction,
        arg_types: Option<SmallVec<[TypeId; 4]>>,
        result_type: Option<TypeId>,
    ) -> Self {
        Self {
            name,
            is_func,
            arg_types,
            result_type,
        }
    }

    /// Creates a `DispatchId` corresponding to this query.
    pub fn id(&self) -> DispatchId {
        Handle::from_name(self.name.0, &self.name.1.as_u128().to_le_bytes())
    }
}

/// A collection of definitions with the same name, but different types.
#[derive(Debug, Clone)]
pub struct Dispatcher {
    name: Name,
    definitions: Vec<Definition>,
}

impl Dispatcher {
    /// Creates a new empty dispatcher.
    pub fn new(name: Name) -> Self {
        Self {
            name,
            definitions: Vec::new(),
        }
    }

    /// Creates a dispatcher with some definitions.
    pub fn with_definitions<I>(name: Name, defs: I) -> Self
    where
        I: IntoIterator<Item = Definition>,
    {
        Self {
            name: name,
            definitions: defs.into_iter().collect(),
        }
    }

    /// Creates a `DispatchId` corresponding to this dispatcher.
    pub fn id(&self) -> DispatchId {
        Handle::from_name(self.name.0, &self.name.1.as_u128().to_le_bytes())
    }

    /// Pushes an element to the end of the definitions list.
    pub fn push(&mut self, def: Definition) {
        self.definitions.push(def);
    }

    /// Queries this dispatcher for a definition.
    pub fn query(&self, q: &Query, res: &Resources<&NamedType>) -> SmallVec<[&Definition; 1]> {
        let mut result = SmallVec::new();
        for def in &self.definitions {
            if def.matches(q, res) {
                result.push(def);
            }
        }
        result
    }
}

/// A single function, variable or constant implementation.
#[derive(Debug, Clone)]
pub struct Definition {
    universe: i32,
    is_func: bool,
    arg_types: Option<SmallVec<[TypeId; 4]>>,
    result_type: TypeId,
}

impl Definition {
    /// Creates a new function definition.
    pub fn new_function(
        universe: i32,
        arg_types: SmallVec<[TypeId; 4]>,
        result_type: TypeId,
    ) -> Self {
        Self {
            universe,
            is_func: true,
            arg_types: Some(arg_types),
            result_type,
        }
    }

    /// Creates a new variable or constant definition.
    pub fn new_variable(universe: i32, result_type: TypeId) -> Self {
        Self {
            universe,
            is_func: false,
            arg_types: None,
            result_type,
        }
    }

    /// Obtain a slice of the arguments for this definition, if any.
    pub fn arg_types(&self) -> Option<&[TypeId]> {
        self.arg_types.as_ref().map(AsRef::as_ref)
    }

    /// For functions obtains the result type, for variables and constants obtains the binding type.
    pub fn result_type(&self) -> TypeId {
        self.result_type
    }

    /// Returns true if the query matches this definition.
    pub fn matches(&self, q: &Query, res: &Resources<&NamedType>) -> bool {
        match (self.is_func, q.is_func) {
            (_, IsFunction::Maybe) => {}
            (true, IsFunction::Yes) => {}
            (true, IsFunction::No) => {}
            (false, IsFunction::No) => {}
            _ => return false,
        }

        if self.is_func {
            if let Some(b) = q.arg_types.as_ref() {
                let a = self.arg_types.as_ref().unwrap();
                // NOTE: auto-currying
                if a.len() > b.len() {
                    return false;
                }
                for (a, b) in a.iter().zip(b) {
                    if !a.matches(b, res) {
                        return false;
                    }
                }
            }
        }

        if let Some(b) = q.result_type.as_ref() {
            if !self.result_type.matches(b, res) {
                return false;
            }
        }

        true
    }
}
