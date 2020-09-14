use crate::assets::Handle;

pub type ScopeId = Handle<Scope>;

#[derive(Debug, Clone)]
pub struct Scope {
    parent: Option<ScopeId>,
}

impl Scope {
    pub fn new() -> Self {
        Self {
            parent: None,
        }
    }

    pub fn with_parent(parent: ScopeId) -> Self {
        Self {
            parent: Some(parent),
        }
    }
}
