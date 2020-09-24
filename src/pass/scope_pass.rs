use failure::Fallible;
use hashbrown::HashMap;

use crate::assets::{LazyUpdate, Resources};
use crate::ast::{Kind, Node, RootNode, Visit, VisitResult};
use crate::scope::{Scope, ScopeId};

pub fn scope_update(
    _lazy: &mut LazyUpdate,
    mut scopes: Resources<&mut Scope>,
    roots: Resources<&RootNode>,
    mut nodes: Resources<&mut Node>,
) -> Fallible<Option<&'static str>> {
    for (_, root_node) in roots.iter::<RootNode>() {
        let root = nodes.get::<Node>(root_node.0).unwrap();
        let global = Scope::new();
        let handle = ScopeId::new();
        scopes.insert(handle, global);
        let global = handle;
        let mut new_scopes = HashMap::new();

        root.visit(Visit::Preorder, &nodes, |_res, node, parent| {
            match node.kind {
                Kind::Block | Kind::Function => {
                    let handle = if let Some(parent) = parent {
                        new_scopes[&parent.id()]
                    } else {
                        global
                    };
                    new_scopes.insert(node.id(), handle);
                    
                    let handle = ScopeId::new();
                    let scope = if let Some(parent) = parent {
                        Scope::with_parent(new_scopes[&parent.id()])
                    } else {
                        Scope::with_parent(global)
                    };
                    scopes.insert(handle, scope);
                }
                _ => {
                    let handle = if let Some(parent) = parent {
                        new_scopes[&parent.id()]
                    } else {
                        global
                    };
                    new_scopes.insert(node.id(), handle);
                }
            }
            VisitResult::Recurse
        });

        for (handle, scope) in new_scopes {
            nodes.get_mut::<Node>(handle).unwrap().scope = Some(scope);
        }
    }
    Ok(None)
}
