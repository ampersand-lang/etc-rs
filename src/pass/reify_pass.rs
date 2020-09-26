use std::iter;

use failure::Fallible;
use hashbrown::{HashMap, HashSet};
use smallvec::SmallVec;

use crate::assets::{Handle, LazyUpdate, Resources};
use crate::ast::{Kind, Node, RootNode, Visit, VisitResult};
use crate::values::Payload;

pub fn reify_update(
    _lazy: &mut LazyUpdate,
    roots: Resources<&RootNode>,
    mut strings: Resources<&mut String>,
    mut nodes: Resources<&mut Node>,
) -> Fallible<Option<&'static str>> {
    for (_, root_node) in roots.iter::<RootNode>() {
        let root = nodes.get::<Node>(root_node.0).unwrap();
        let mut types = HashSet::new();
        let type_raw = "type".to_string();
        let handle = Handle::from_hash(type_raw.as_bytes());
        strings.insert(handle, type_raw);
        types.insert(handle);
        let mut polymorphic = HashMap::new();

        root.visit(Visit::Postorder, &nodes, |_res, node, _| {
            match node.kind {
                Kind::Global => {
                    let ident = node.children[0].unwrap();
                    let ident = nodes.get(ident).unwrap();
                    let ident = match ident.payload.unwrap() {
                        Payload::Identifier(ident) => ident,
                        _ => todo!(),
                    };
                    let binding = node.children[2].unwrap();
                    let binding = nodes.get(binding).unwrap();

                    let is_generic = match binding.kind {
                        Kind::Function => {
                            let params = binding.children[0].unwrap();
                            let params = nodes.get(params).unwrap();
                            match params.kind {
                                Kind::Tuple => {
                                    let mut is_generic: Option<usize> = None;
                                    for param in &params.children {
                                        let param = param.unwrap();
                                        let param = nodes.get(param).unwrap();
                                        match param.kind {
                                            Kind::Declaration => {
                                                let t = param.children[1].unwrap();
                                                let t = nodes.get(t).unwrap();
                                                match t.payload {
                                                    Some(Payload::Identifier(ident)) => if types.contains(&ident) {
                                                        is_generic = is_generic.map(|n| n + 1).or(Some(1));
                                                    },
                                                    _ => {}
                                                }
                                            }
                                            _ => {}
                                        }
                                    }
                                    is_generic
                                }
                                _ => None,
                            }
                        }
                        Kind::Nil => {
                            let binding = match binding.payload.unwrap() {
                                Payload::Identifier(binding) => binding,
                                _ => todo!(),
                            };

                            if types.contains(&binding) {
                                types.insert(ident);
                            }
                            
                            None
                        }
                        _ => None,
                    };

                    if let Some(params) = is_generic {
                        polymorphic.insert(ident, (binding.id(), params));
                    }
                }
                _ => {}
            }
            VisitResult::Recurse
        });

        let mut new_nodes = Vec::new();
        let mut replace = Vec::new();

        for (_, &(node, _)) in &polymorphic {
            let node = nodes.get(node).unwrap();
            match node.kind {
                Kind::Function => {
                    let mut args0 = SmallVec::<[_; 4]>::new();
                    let mut args1 = SmallVec::<[_; 4]>::new();
                    let params = node.children[0].unwrap();
                    let params = nodes.get(params).unwrap();
                    match params.kind {
                        Kind::Tuple => {
                            for param in &params.children {
                                let param = param.unwrap();
                                let param = nodes.get(param).unwrap();
                                match param.kind {
                                    Kind::Declaration => {
                                        let t = param.children[1].unwrap();
                                        let t = nodes.get(t).unwrap();
                                        match t.payload {
                                            Some(Payload::Identifier(ident)) => if types.contains(&ident) {
                                                args0.push(Some(param.id()));
                                            } else {
                                                args1.push(Some(param.id()));
                                            },
                                            _ => {}
                                        }
                                    }
                                    _ => {}
                                }
                            }
                        }
                        _ => {},
                    }
                    
                    let args0 = Node::new(
                        Kind::Tuple,
                        Handle::nil(),
                        args0,
                    );
                    let h = args0.id();
                    new_nodes.push(args0);
                    let args0 = h;
                    
                    let args1 = Node::new(
                        Kind::Tuple,
                        Handle::nil(),
                        args1,
                    );
                    let h = args1.id();
                    new_nodes.push(args1);
                    let args1 = h;

                    let body = node.children[1].unwrap();
                    let func1 = Node::new(
                        Kind::Function,
                        Handle::nil(),
                        iter::once(Some(args1))
                            .chain(iter::once(Some(body)))
                    );
                    let h = func1.id();
                    new_nodes.push(func1);
                    let func1 = h;

                    let func0 = Node::new(
                        Kind::Function,
                        Handle::nil(),
                        iter::once(Some(args0))
                            .chain(iter::once(Some(func1)))
                    );
                    replace.push((node.id(), func0));
                }
                _ => {},
            }
        }

        for node in new_nodes {
            nodes.insert(node.id(), node);
        }

        for (handle, node) in replace {
            nodes.get_mut(handle).unwrap().clone_from(node);
        }

        let mut genapp = HashMap::new();
        let mut transaction = HashMap::new();
        let mut nomark = HashSet::new();
        let mut mark = HashSet::new();

        let root = nodes.get::<Node>(root_node.0).unwrap();
        root.visit(Visit::Postorder, &nodes, |_res, node, _| {
            match node.kind {
                Kind::Nil => {
                    let ident = match node.payload.unwrap() {
                        Payload::Identifier(ident) => ident,
                        _ => return VisitResult::Recurse,
                    };
                    if let Some((_, params)) = polymorphic.get(&ident) {
                        genapp.insert(node.id(), *params);
                    }
                }
                Kind::Application => {
                    let func = node.children[0].as_ref().unwrap();
                    nomark.insert(*func);
                    if let Some(n) = genapp.get(func).copied() {
                        transaction.insert(node.id(), n);
                        let args = &node.children[1..n + 1];
                        for arg in args {
                            let arg = arg.unwrap();
                            mark.insert(arg);
                        }
                    }
                }
                _ => {}
            }
            VisitResult::Recurse
        });

        for handle in nomark {
            nodes.get_mut(handle).unwrap().no_newnode = true;
        } 

        for handle in mark {
            nodes.get_mut(handle).unwrap().mark_newnode = true;
        } 

        for (handle, n) in transaction {
            let node = nodes.get(handle).unwrap();
            let func = node.children[0].unwrap();
            let func = nodes.get(func).unwrap();
            let args = match node.kind {
                Kind::Application => &node.children[1..],
                _ => todo!(),
            };
            if args.len() > n {
                let func = func.id();
                let args0 = args[..n].to_vec();
                let args1 = args[n..].to_vec();
                
                let mut app0 = Node::new(
                    Kind::Application,
                    Handle::nil(),
                    iter::once(Some(func))
                        .chain(args0)
                );
                app0.no_newnode = true;
                let h = app0.id();
                nodes.insert(app0.id(), app0);
                let app0 = h;

                let mut app1 = Node::new(
                    Kind::Application,
                    Handle::nil(),
                    iter::once(Some(app0))
                        .chain(args1)
                );
                app1.old_reify = Some(n);

                nodes.get_mut(handle).unwrap().clone_from(app1);
            }
        }
    }
    Ok(None)
}
