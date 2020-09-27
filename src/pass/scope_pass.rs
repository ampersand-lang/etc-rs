use failure::Fallible;
use hashbrown::HashMap;

use crate::assets::{LazyUpdate, Resources};
use crate::ast::{Kind, Node, RootNode, VisitResult, Which};
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
        let mut scope_list = vec![global];
        let mut valid_scopes = vec![global].into_iter().collect::<hashbrown::HashSet<_>>();

        root.visit_twice(&nodes, |_res, node, _, which| {
            match which {
                Which::Before => match node.kind {
                    Kind::Block | Kind::Function => {
                        let handle = scope_list.last().copied().unwrap();
                        new_scopes.insert(node.id(), handle);

                        let scope = Scope::with_parent(handle);
                        let handle = ScopeId::new();
                        scopes.insert(handle, scope);
                        scope_list.push(handle);
                        valid_scopes.insert(handle);
                    }
                    _ => {
                        let handle = scope_list.last().copied().unwrap();
                        new_scopes.insert(node.id(), handle);
                    }
                },
                Which::After => match node.kind {
                    Kind::Block | Kind::Function => {
                        scope_list.pop();
                    }
                    _ => {}
                },
            }
            VisitResult::Recurse
        });

        for (handle, scope) in new_scopes {
            nodes.get_mut::<Node>(handle).unwrap().scope = Some(scope);
        }
    }
    Ok(None)
}
