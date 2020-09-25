use std::iter;

use failure::Fallible;
use hashbrown::{HashMap, HashSet};
use smallvec::SmallVec;

use crate::assets::{Handle, LazyUpdate, Resources};
use crate::ast::{Kind, Node, NodeId, RootNode, Visit, VisitResult};
use crate::types::primitive;
use crate::values::Payload;

struct Context {
    lowest: i32,
    universes: HashMap<NodeId, i32>,
    generic: HashMap<NodeId, SmallVec<[Handle<String>; 4]>>,
    generic_defs: HashSet<NodeId>,
    generic_call: HashMap<NodeId, NodeId>,
    bindings: Vec<HashMap<Handle<String>, (NodeId, NodeId)>>,
    rev: HashMap<Handle<String>, NodeId>,
    res_rev: HashMap<Handle<String>, NodeId>,
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
            if let Some(handle) = node.children[1] {
                let child = nodes.get(handle).unwrap();
                // XXX: is this hacky?
                match child.payload {
                    Some(Payload::Type(_)) => {}
                    _ => {
                        if ctx.universes[&handle] < ctx.lowest {
                            ctx.lowest = ctx.universes[&handle];
                        } else {
                            ctx.lowest -= 1;
                            let level = ctx.lowest;
                            ctx.universes.insert(handle, level);
                        }
                    }
                }
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
        Kind::Application => {
            let func = node.children[0].unwrap();
            if let Some(handle) = ctx.generic_call.get(&func) {
                let generic = nodes.get(*handle).unwrap();
                let universe = ctx.universes[generic.children[2].as_ref().unwrap()];
                universe
            } else {
                node
                    .children
                    .iter()
                    .map(Option::as_ref)
                    .flatten()
                    .map(|child| ctx.universes[child])
                    .max()
                    .unwrap()
            }
        }
        Kind::Function => node
            .children
            .iter()
            .map(Option::as_ref)
            .flatten()
            .map(|child| ctx.universes[child])
            .min()
            .unwrap(),
        _ => node
            .children
            .iter()
            .map(Option::as_ref)
            .flatten()
            .map(|child| ctx.universes[child])
            .max()
            .unwrap(),
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
                    let node_id = ident.id();
                    let ident = match ident.payload.unwrap() {
                        Payload::Identifier(ident) => ident,
                        _ => todo!(),
                    };
                    if let Some(&handle) = ctx.rev.get(&ident) {
                        let rev_dep = *ctx.universes.get(&handle).unwrap();
                        ctx.universes.insert(node_id, rev_dep);
                        ctx.rev.remove(&ident);
                        ctx.res_rev.insert(ident, handle);
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
                                        ctx.res_rev.insert(ident, node.id());
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
                                        ctx.res_rev.insert(ident, node.id());
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
        Kind::Declaration => {
            let ident = nodes.get(node.children[0].unwrap()).unwrap();
            match ident.payload.unwrap() {
                Payload::Identifier(ident) => {
                    ctx.bindings.last_mut().unwrap().insert(ident, (node.id(), node.children[0].unwrap()));
                }
                _ => {}
            }
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
        Kind::Binding => {
            let ident = nodes.get(node.children[0].unwrap()).unwrap();
            let ident = match ident.payload.unwrap() {
                Payload::Identifier(ident) => ident,
                _ => todo!(),
            };
            ctx.bindings.last_mut().unwrap().insert(ident, (node.id(), node.children[2].unwrap()));
        }
        Kind::Declaration => {
            let ident = nodes.get(node.children[0].unwrap()).unwrap();
            match ident.payload.unwrap() {
                Payload::Identifier(ident) => {
                    ctx.bindings.last_mut().unwrap().insert(ident, (node.id(), node.children[0].unwrap()));
                }
                _ => {}
            }
        }
        Kind::Nil => {
            match node.payload.unwrap() {
                Payload::Identifier(ident) => {
                    for bindings in ctx.bindings.iter().rev() {
                        if let Some((parent, h)) = bindings.get(&ident) {
                            if ctx.generic_defs.contains(parent) {
                                ctx.generic_call.insert(handle, *parent);
                            }
                            let current = ctx.universes[&handle];
                            let universe = ctx.universes[h].min(current);
                            ctx.universes.insert(handle, universe);
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

fn generic(
    _lazy: &mut LazyUpdate,
    nodes: &Resources<&mut Node>,
    handle: NodeId,
    parent: Option<NodeId>,
    ctx: &mut Context,
) -> Fallible<HashMap<NodeId, Node>> {
    let mut transaction = HashMap::new();
    let node = nodes.get(handle).unwrap();

    for child in &node.children {
        if let Some(&child) = child.as_ref() {
            transaction.extend(generic(_lazy, nodes, child, Some(handle), ctx)?);
        }
    }
    
    let this = ctx.universes[&node.id()];
    let child = match node.kind {
        Kind::Function => {
            let handle = node.children[1].as_ref().unwrap();
            if ctx.generic.contains_key(handle) {
                None
            } else {
                Some(ctx.universes[handle])
            }
        }
        _ => None,
    };

    if let Some(child) = child {
        if child > this {
            let mut variables = SmallVec::new();
            let params = node.children[0].unwrap();
            let params = nodes.get(params).unwrap();
            match params.kind {
                Kind::Tuple => {
                    for param in &params.children {
                        let param = nodes.get(param.unwrap()).unwrap();
                        match param.kind {
                            Kind::Declaration => {
                                let mut node_type = Node::new(Kind::Nil, Handle::nil(), iter::empty());
                                node_type.type_of = Some(*primitive::TYPE);
                                node_type.payload = Some(Payload::Type(*primitive::NODE));
                                    
                                let typ = param.children[1].unwrap();
                                transaction.insert(typ, node_type);
                            }
                            _ => {}
                        }
                    }
                }
                _ => {}
            }
            node.visit(Visit::Postorder, nodes, |_, node, parent| {
                match node.kind {
                    Kind::Nil => match node.payload.as_ref().unwrap() {
                        Payload::Identifier(ident) if ctx.res_rev.contains_key(ident) => {
                            if let Some(parent) = parent {
                                if matches!(parent.kind, Kind::Declaration) {
                                    if Some(node.id()) == parent.children[0] {
                                        return VisitResult::Recurse;
                                    }
                                }
                                let universe = ctx.universes[&parent.id()];
                                ctx.universes.insert(node.id(), universe);
                            }
                            variables.push(*ident);
                        }
                        _ => {}
                    }
                    _ => {}
                }
                VisitResult::Recurse
            });
            let child = node.children[1].unwrap();
            ctx.generic.insert(child, variables);
            if let Some(parent) = parent {
                ctx.generic_defs.insert(parent);
            }
        }
    }

    Ok(transaction)
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
            generic: HashMap::new(),
            generic_call: HashMap::new(),
            generic_defs: HashSet::new(),
            bindings,
            rev,
            res_rev: HashMap::new(),
        };

        root.visit(Visit::Postorder, &nodes, |_res, node, _| {
            ctx.universes.insert(node.id(), node.universe.min(0));
            VisitResult::Recurse
        });

        types(_lazy, &nodes, root_node.0, None, &mut ctx)?;
        non_nil(_lazy, &nodes, root_node.0, None, &mut ctx)?;
        reverse_dependencies(_lazy, &nodes, root_node.0, None, &mut ctx)?;
        non_nil(_lazy, &nodes, root_node.0, None, &mut ctx)?;
        
        let transaction = generic(_lazy, &mut nodes, root_node.0, None, &mut ctx)?;

        for (handle, node) in transaction {
            nodes.get_mut(handle).unwrap().clone_from(node);
        }
        
        non_nil(_lazy, &nodes, root_node.0, None, &mut ctx)?;
        dependencies(_lazy, &nodes, root_node.0, None, &mut ctx)?;

        for (handle, gen) in &ctx.generic_call {
            nodes.get_mut::<Node>(*handle).unwrap().generic_call = Some(*gen);
        }
        
        non_nil(_lazy, &nodes, root_node.0, None, &mut ctx)?;

        for (handle, universe) in ctx.universes {
            nodes.get_mut::<Node>(handle).unwrap().universe = universe;
        }

        for (handle, variables) in ctx.generic {
            nodes.get_mut::<Node>(handle).unwrap().generic = Some(variables);
        }
    }
    Ok(None)
}
