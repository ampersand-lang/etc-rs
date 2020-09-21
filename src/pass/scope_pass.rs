use failure::Fallible;
use hashbrown::HashMap;

use crate::assets::{LazyUpdate, Resources};
use crate::ast::{Kind, Node, RootNode, Visit, VisitResult};
use crate::scope::{Scope, ScopeId};

pub fn scope_update(
    lazy: &mut LazyUpdate,
    roots: Resources<&RootNode>,
    mut nodes: Resources<&mut Node>,
) -> Fallible<Option<&'static str>> {
    for (_, root_node) in roots.iter::<RootNode>() {
        let root = nodes.get::<Node>(root_node.0).unwrap();
        let global = Scope::new();
        let handle = ScopeId::new();
        lazy.insert(handle, global);
        let global = handle;
        let mut scopes = HashMap::new();

        root.visit(Visit::Preorder, &nodes, |_res, node| {
            match node.kind {
                Kind::Block | Kind::Function => {
                    let handle = ScopeId::new();
                    let scope = if let Some(parent) = node.parent() {
                        Scope::with_parent(scopes[&parent])
                    } else {
                        Scope::with_parent(ScopeId::new())
                    };
                    scopes.insert(node.id(), handle);
                    lazy.insert(handle, scope);
                }
                _ => {
                    let handle = if let Some(parent) = node.parent() {
                        scopes[&parent]
                    } else {
                        global
                    };
                    scopes.insert(node.id(), handle);
                }
            }
            VisitResult::Recurse
        });

        for (handle, scope) in scopes {
            nodes.get_mut::<Node>(handle).unwrap().scope = Some(scope);
        }
    }
    Ok(None)
}
