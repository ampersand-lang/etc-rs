use smallvec::SmallVec;

use crate::assets::Handle;
use crate::scope::ScopeId;
use crate::types::TypeId;

pub type DispatchId = Handle<Dispatcher>;

#[derive(Debug, Clone, Hash)]
pub struct Name(pub(crate) ScopeId, pub(crate) Handle<String>);

#[derive(Debug, Clone, Copy)]
pub enum IsFunction {
    Yes,
    No,
    Maybe,
}

#[derive(Debug, Clone)]
pub struct Query {
    name: Name,
    is_func: IsFunction,
    arg_types: Option<SmallVec<[TypeId; 4]>>,
    result_type: Option<TypeId>,
}

impl Query {
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

    pub fn id(&self) -> DispatchId {
        Handle::from_name(self.name.0, &self.name.1.as_u128().to_le_bytes())
    }
}

#[derive(Debug, Clone)]
pub struct Dispatcher {
    name: Name,
    definitions: Vec<Definition>,
}

impl Dispatcher {
    pub fn new(name: Name) -> Self {
        Self {
            name,
            definitions: Vec::new(),
        }
    }

    pub fn with_definitions<I>(name: Name, defs: I) -> Self
    where
        I: IntoIterator<Item = Definition>,
    {
        Self {
            name: name,
            definitions: defs.into_iter().collect(),
        }
    }

    pub fn id(&self) -> DispatchId {
        Handle::from_name(self.name.0, &self.name.1.as_u128().to_le_bytes())
    }

    pub fn push(&mut self, def: Definition) {
        self.definitions.push(def);
    }

    pub fn query(&self, q: &Query) -> SmallVec<[&Definition; 1]> {
        let mut result = SmallVec::new();
        for def in &self.definitions {
            if def.matches(q) {
                result.push(def);
            }
        }
        result
    }
}

#[derive(Debug, Clone)]
pub struct Definition {
    is_func: bool,
    arg_types: Option<SmallVec<[TypeId; 4]>>,
    result_type: TypeId,
}

impl Definition {
    pub fn new_function(arg_types: SmallVec<[TypeId; 4]>, result_type: TypeId) -> Self {
        Self {
            is_func: true,
            arg_types: Some(arg_types),
            result_type,
        }
    }

    pub fn new_variable(result_type: TypeId) -> Self {
        Self {
            is_func: false,
            arg_types: None,
            result_type,
        }
    }

    pub fn arg_types(&self) -> Option<&[TypeId]> {
        self.arg_types.as_ref().map(AsRef::as_ref)
    }

    pub fn result_type(&self) -> TypeId {
        self.result_type
    }

    pub fn matches(&self, q: &Query) -> bool {
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
                    if !a.matches(b) {
                        return false;
                    }
                }
            }
        }

        if let Some(b) = q.result_type.as_ref() {
            if !self.result_type.matches(b) {
                return false;
            }
        }

        true
    }
}
