use failure::Fallible;
use hashbrown::HashMap;
use smallvec::SmallVec;

use crate::assets::{LazyUpdate, Resources};
use crate::ast::{Kind, Node, RootNode, Visit};
use crate::scope::{ScopeId, Scope};

pub fn infer_update(
    lazy: &mut LazyUpdate,
    roots: Resources<&RootNode>,
    payloads: Resources<&Value>,
    mut scopes: Resources<&mut Scope>,
    mut nodes: Resources<&mut Node>,
) -> Fallible<()> {
    for (_, root_node) in roots.iter::<RootNode>() {
        let root = nodes.get::<Node>(root_node.0).unwrap();
        let mut scopes = HashMap::new();
        
        root.visit(Visit::Preorder, &nodes, |res, node| {
            match node.kind {
                Kind::Block | Kind::Function => {
                    let handle = ScopeId::new();
                    let scope = if let Some(parent) = node.parent() {
                        ScopeId::with_parent(scopes[parent])
                    } else {
                        ScopeId::new();
                    };
                    scopes.insert(node.id(), handle);
                    lazy.insert(handle, scope);
                }
            }
        });

        for (handle, scope) in scopes {
            nodes.get_mut::<Node>(handle).unwrap().scope = Some(scope);
        }
    }
}
