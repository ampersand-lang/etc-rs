use failure::Fallible;
use hashbrown::{HashMap, HashSet};

use crate::assets::{Handle, LazyUpdate, Resources};
use crate::ast::{Kind, Node, RootNode, Visit, VisitResult, Which};
use crate::values::Payload;

pub fn definition_update(
    _lazy: &mut LazyUpdate,
    roots: Resources<&RootNode>,
    mut nodes: Resources<&mut Node>,
    mut strings: Resources<&mut String>,
) -> Fallible<Option<&'static str>> {
    // TODO: all the builtins
    // TODO: move this elsewhere
    let builtins = vec!["sint", "type"]
        .into_iter()
        .map(|name| {
            let handle = Handle::from_hash(name.as_bytes());
            strings.insert(handle, name.to_string());
            handle
        })
        .collect::<HashSet<_>>();

    for (_, root_node) in roots.iter::<RootNode>() {
        let root = nodes.get::<Node>(root_node.0).unwrap();
        let mut bindings = vec![HashMap::new()];
        let mut definitions = HashMap::new();

        let mut skip = HashSet::new();

        root.visit(Visit::Preorder, &nodes, |_res, node, _| {
            match node.kind {
                Kind::Global | Kind::Binding | Kind::Declaration => {
                    skip.insert(node.children[0].unwrap());
                }
                Kind::Application => {
                    let ident = node.children[0].unwrap();
                    let ident = nodes.get(ident).unwrap();
                    if ident.alternative {
                        let ident = match ident.payload.unwrap() {
                            Payload::Identifier(ident) => Some(ident),
                            Payload::Struct => None,
                            _ => panic!("binding is not an identifier"),
                        };
                        if let Some(ident) = ident {
                            let string = strings.get(ident).unwrap();
                            match string.as_str() {
                                "replace" => {
                                    skip.insert(node.id());
                                }
                                _ => {}
                            }
                        }
                    }
                }
                _ => {}
            }
            VisitResult::Recurse
        });

        root.visit_twice(&nodes, |_res, node, _, which| {
            if skip.contains(&node.id()) {
                return VisitResult::Continue;
            }
            match which {
                Which::Before => match node.kind {
                    Kind::Block | Kind::Function => {
                        bindings.push(HashMap::new());
                    }
                    _ => {}
                },
                Which::After => match node.kind {
                    Kind::Global | Kind::Binding | Kind::Declaration => {
                        let ident = node.children[0].unwrap();
                        let ident = nodes.get(ident).unwrap();
                        let ident = match ident.payload.unwrap() {
                            Payload::Identifier(ident) => ident,
                            _ => panic!("binding is not an identifier"),
                        };
                        bindings.last_mut().unwrap().insert(ident, node.id());
                    }
                    Kind::Nil if !node.alternative => match node.payload.unwrap() {
                        Payload::Identifier(ident) => {
                            let mut found = false;
                            if builtins.contains(&ident) {
                                found = true;
                            } else {
                                for bindings in bindings.iter().rev() {
                                    if let Some(handle) = bindings.get(&ident) {
                                        found = true;
                                        definitions.insert(node.id(), *handle);
                                        break;
                                    }
                                }
                            }
                            if !found {
                                let string = strings.get(ident).unwrap();
                                panic!("binding not found: `{}`", string.as_str());
                            }
                        }
                        _ => {}
                    },
                    _ => {}
                },
            }
            match which {
                Which::Before => {}
                Which::After => match node.kind {
                    Kind::Block | Kind::Function => {
                        bindings.pop();
                    }
                    _ => {}
                },
            }
            VisitResult::Recurse
        });

        for (handle, def) in definitions {
            nodes.get_mut::<Node>(handle).unwrap().definition = Some(def);
        }
    }
    Ok(None)
}
