//! Contains a marker-structure for trees of scopes.
use crate::assets::Handle;

/// A unique handle to a scope.
pub type ScopeId = Handle<Scope>;

// A node in the scope tree with a parent or the root scope.
#[derive(Debug, Clone)]
pub struct Scope {
    parent: ScopeId,
}

impl Scope {
    /// Constructs a new root scope.
    pub fn new() -> Self {
        Self {
            parent: ScopeId::nil(),
        }
    }

    /// Constructs a new node scope.
    pub fn with_parent(parent: ScopeId) -> Self {
        Self { parent }
    }

    pub fn parent(&self) -> ScopeId {
        self.parent
    }
}
