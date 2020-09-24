use failure::Fallible;
use hashbrown::HashMap;

use crate::assets::{Handle, LazyUpdate, Resources};
use crate::ast::{Kind, Node, NodeId, RootNode, Visit, VisitResult};
use crate::values::Payload;

struct Context {
    lowest: i32,
    universes: HashMap<NodeId, i32>,
    bindings: Vec<HashMap<Handle<String>, NodeId>>,
    rev: HashMap<Handle<String>, NodeId>,
}

fn types(
    _lazy: &mut LazyUpdate,
    nodes: &Resources<&mut Node>,
    handle: NodeId,
    _parent: Option<NodeId>,
    ctx: &mut Context,
) -> Fallible<()> {
    let node = nodes.get(handle).unwrap();

    if matches!(node.kind, Kind::Function) {
        for child in node.children.iter().rev() {
            if let Some(&child) = child.as_ref() {
                types(_lazy, nodes, child, Some(handle), ctx)?;
            }
        }
    } else {
        for child in &node.children {
            if let Some(&child) = child.as_ref() {
                types(_lazy, nodes, child, Some(handle), ctx)?;
            }
        }
    }

    match node.kind {
        Kind::Binding | Kind::Declaration => {
            if let Some(child) = node.children[1] {
                ctx.lowest -= 1;
                let level = ctx.lowest;
                ctx.universes.insert(child, level);
            }
        }
        _ => {}
    }

    Ok(())
}

fn non_nil(
    _lazy: &mut LazyUpdate,
    nodes: &Resources<&mut Node>,
    handle: NodeId,
    _parent: Option<NodeId>,
    ctx: &mut Context,
) -> Fallible<()> {
    let node = nodes.get(handle).unwrap();

    for child in &node.children {
        if let Some(&child) = child.as_ref() {
            non_nil(_lazy, nodes, child, Some(handle), ctx)?;
        }
    }

    let universe = match node.kind {
        Kind::Nil => ctx.universes[&node.id()],
        Kind::Function => {
            node.children.iter().map(Option::as_ref).flatten().map(|child| ctx.universes[child]).min().unwrap()
        }
        _ => {
            node.children.iter().map(Option::as_ref).flatten().map(|child| ctx.universes[child]).max().unwrap()
        }
    };
    ctx.universes.insert(node.id(), universe);

    Ok(())
}

fn reverse_dependencies(
    _lazy: &mut LazyUpdate,
    nodes: &Resources<&mut Node>,
    handle: NodeId,
    _parent: Option<NodeId>,
    ctx: &mut Context,
) -> Fallible<()> {
    let node = nodes.get(handle).unwrap();
    
    for child in &node.children {
        if let Some(&child) = child.as_ref() {
            reverse_dependencies(_lazy, nodes, child, Some(handle), ctx)?;
        }
    }

    match node.kind {
        Kind::Binding | Kind::Declaration => {
            if let Some(handle) = node.children[1] {
                let node = nodes.get(handle).unwrap();
                match node.payload.unwrap() {
                    Payload::Identifier(ident) => {
                        ctx.rev.insert(ident, node.id());
                    }
                    _ => {}
                }
            }
        }
        _ => {}
    }

    match node.kind {
        Kind::Function => {
            let tuple = nodes.get(node.children[0].unwrap()).unwrap();
            match tuple.kind {
                Kind::Nil => {
                    let ident = tuple;
                    let ident = match ident.payload.unwrap() {
                        Payload::Identifier(ident) => ident,
                        _ => todo!(),
                    };
                    if let Some(&rev_dep) = ctx.rev.get(&ident) {
                        let rev_dep = *ctx.universes.get(&rev_dep).unwrap();
                        ctx.universes.insert(node.id(), rev_dep);
                        ctx.rev.remove(&ident);
                    }
                }
                Kind::Tuple => {
                    let tuple = nodes.get(node.children[0].unwrap()).unwrap();
                    for node in &tuple.children {
                        if let Some(handle) = node {
                            let node = nodes.get(*handle).unwrap();
                            match node.kind {
                                Kind::Nil => {
                                    let ident = match node.payload.unwrap() {
                                        Payload::Identifier(ident) => ident,
                                        _ => todo!(),
                                    };
                                    if let Some(&rev_dep) = ctx.rev.get(&ident) {
                                        let rev_dep = *ctx.universes.get(&rev_dep).unwrap();
                                        ctx.universes.insert(node.id(), rev_dep);
                                        ctx.rev.remove(&ident);
                                    }
                                }
                                Kind::Binding | Kind::Declaration => {
                                    let node = nodes.get(node.children[0].unwrap()).unwrap();
                                    let ident = match node.payload.unwrap() {
                                        Payload::Identifier(ident) => ident,
                                        _ => todo!(),
                                    };
                                    if let Some(&rev_dep) = ctx.rev.get(&ident) {
                                        let rev_dep = *ctx.universes.get(&rev_dep).unwrap();
                                        ctx.universes.insert(node.id(), rev_dep);
                                        ctx.rev.remove(&ident);
                                    }
                                }
                                _ => {}
                            }
                        }
                    }
                }
                _ => {}
            }
        }
        Kind::Binding | Kind::Declaration => {
            let node = nodes.get(node.children[0].unwrap()).unwrap();
            let ident = match node.payload.unwrap() {
                Payload::Identifier(ident) => ident,
                _ => todo!(),
            };
            if let Some(&rev_dep) = ctx.rev.get(&ident) {
                let rev_dep = *ctx.universes.get(&rev_dep).unwrap();
                ctx.universes.insert(node.id(), rev_dep);
            }
        }
        _ => {}
    }

    Ok(())
}

fn dependencies_bind(
    _lazy: &mut LazyUpdate,
    nodes: &Resources<&mut Node>,
    handle: NodeId,
    _parent: Option<NodeId>,
    ctx: &mut Context,
) -> Fallible<()> {
    let node = nodes.get(handle).unwrap();

    match node.kind {
        Kind::Tuple => {
            for &child in &node.children {
                if let Some(child) = child {
                    dependencies_bind(_lazy, nodes, child, Some(handle), ctx)?;
                }
            }
        }
        Kind::Binding | Kind::Declaration => {
            let ident = nodes.get(node.children[0].unwrap()).unwrap();
            let ident = match ident.payload.unwrap() {
                Payload::Identifier(ident) => ident,
                _ => todo!(),
            };
            ctx.bindings.last_mut().unwrap().insert(ident, node.id());
        }
        _ => {}
    }

    Ok(())
}

fn dependencies(
    _lazy: &mut LazyUpdate,
    nodes: &Resources<&mut Node>,
    handle: NodeId,
    _parent: Option<NodeId>,
    ctx: &mut Context,
) -> Fallible<()> {
    let node = nodes.get(handle).unwrap();
    
    match node.kind {
        Kind::Block | Kind::Function => {
            ctx.bindings.push(HashMap::new());
        }
        _ => {}
    }

    if matches!(node.kind, Kind::Function) {
        let child = node.children[0].unwrap();
        dependencies_bind(_lazy, nodes, child, Some(handle), ctx)?;
        for child in node.children.iter().rev() {
            if let Some(&child) = child.as_ref() {
                dependencies(_lazy, nodes, child, Some(handle), ctx)?;
            }
        }
    } else {
        for child in &node.children {
            if let Some(&child) = child.as_ref() {
                dependencies(_lazy, nodes, child, Some(handle), ctx)?;
            }
        }
    }

    match node.kind {
        // both of those have the type in position 1, so this is fine
        Kind::Binding | Kind::Declaration => {
            let ident = nodes.get(node.children[0].unwrap()).unwrap();
            let ident = match ident.payload.unwrap() {
                Payload::Identifier(ident) => ident,
                _ => todo!(),
            };
            ctx.bindings.last_mut().unwrap().insert(ident, node.id());
        }
        Kind::Nil => {
            match node.payload.unwrap() {
                Payload::Identifier(ident) => {
                    for bindings in ctx.bindings.iter().rev() {
                        if let Some(handle) = bindings.get(&ident) {
                            let universe = *ctx.universes.get(handle).unwrap();
                            ctx.universes.insert(*handle, universe);
                        }
                    }
                }
                _ => {}
            };
        }
        _ => {}
    }
    
    if matches!(node.kind, Kind::Block | Kind::Function) {
        ctx.bindings.pop();
    }

    Ok(())
}

pub fn universe_update(
    _lazy: &mut LazyUpdate,
    roots: Resources<&RootNode>,
    mut nodes: Resources<&mut Node>,
) -> Fallible<Option<&'static str>> {
    for (_, root_node) in roots.iter::<RootNode>() {
        let root = nodes.get(root_node.0).unwrap();
        let lowest = 0;
        // map of NodeId to Either<i32, i32>, where i32 represents the universe
        // Either::Left represents a universe that may be changed to a lower value
        // Either::Right represents a universe that may be changed to a higher value
        let universes = HashMap::new();
        // a map of ad-hoc bindings of identifiers to nodes
        // required for dependencies
        let bindings = vec![HashMap::new()];
        let rev = HashMap::new();

        let mut ctx = Context {
            lowest,
            universes,
            bindings,
            rev,
        };

        root.visit(Visit::Postorder, &nodes, |_res, node, _| {
            ctx.universes.insert(node.id(), 0);
            VisitResult::Recurse
        });

        types(_lazy, &nodes, root_node.0, None, &mut ctx)?;
        non_nil(_lazy, &nodes, root_node.0, None, &mut ctx)?;
        reverse_dependencies(_lazy, &nodes, root_node.0, None, &mut ctx)?;
        dependencies(_lazy, &nodes, root_node.0, None, &mut ctx)?;
        non_nil(_lazy, &nodes, root_node.0, None, &mut ctx)?;

        for (handle, universe) in ctx.universes {
            nodes.get_mut::<Node>(handle).unwrap().universe = universe;
        }
    }
    Ok(None)
}
