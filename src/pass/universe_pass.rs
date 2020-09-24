use either::Either;
use failure::Fallible;
use hashbrown::{HashMap, HashSet};
use smallvec::SmallVec;

use crate::assets::{Handle, LazyUpdate, Resources};
use crate::ast::{Kind, Node, NodeId, RootNode};
use crate::values::Payload;

#[derive(Debug, Clone)]
struct UniversePrototype {
    dependencies: SmallVec<[NodeId; 4]>,
    inner: Either<i32, i32>,
}

struct Binding {
    is_param: bool,
    node: NodeId,
}

struct Context {
    lowest: i32,
    universes: HashMap<NodeId, UniversePrototype>,
    bindings: Vec<HashMap<Handle<String>, Binding>>,
    params: HashSet<NodeId>,
}

fn visit_bind(
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
                    visit_bind(_lazy, nodes, child, Some(handle), ctx)?;
                }
            }
        }
        Kind::Binding | Kind::Declaration => {
            let ident = nodes.get(node.children[0].unwrap()).unwrap();
            let ident = match ident.payload.unwrap() {
                Payload::Identifier(ident) => ident,
                _ => todo!(),
            };
            ctx.bindings.last_mut().unwrap().insert(ident, Binding { is_param: true, node: node.id() });
        }
        _ => {}
    }

    Ok(())
}

fn visit(
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
        visit_bind(_lazy, nodes, child, Some(handle), ctx)?;
        for child in node.children.iter().rev() {
            if let Some(&child) = child.as_ref() {
                visit(_lazy, nodes, child, Some(handle), ctx)?;
            }
        }
    } else {
        for child in &node.children {
            if let Some(&child) = child.as_ref() {
                let lowest = if matches!(node.kind, Kind::Block) {
                    Some(ctx.lowest)
                } else {
                    None
                };
                visit(_lazy, nodes, child, Some(handle), ctx)?;
                if let Some(lowest) = lowest {
                    ctx.lowest = lowest;
                }
            }
        }
    }

    // both of those have the type in position 1, so this is fine
    if matches!(node.kind, Kind::Binding | Kind::Declaration) {
        let ident = nodes.get(node.children[0].unwrap()).unwrap();
        let ident = match ident.payload.unwrap() {
            Payload::Identifier(ident) => ident,
            _ => todo!(),
        };
        ctx.bindings.last_mut().unwrap().insert(ident, Binding { is_param: false, node: node.id() });
        if let Some(handle) = node.children[1] {
            let u = ctx.universes.get(&handle).cloned().unwrap();
            ctx.lowest += -1;
            match u.inner {
                Either::Left(_) => {}
                Either::Right(_) => {
                    ctx.universes.get_mut(&handle).unwrap().inner = Either::Left(ctx.lowest);
                }
            }
        }
    }
    
    let mut dependencies = SmallVec::new();
    if let Some(payload) = node.payload {
        match payload {
            Payload::Identifier(ident) => {
                for bindings in ctx.bindings.iter().rev() {
                    if let Some(Binding { is_param, node: binding }) = bindings.get(&ident) {
                        if *is_param {
                            ctx.params.insert(node.id());
                        }
                        dependencies.push(*binding);
                        break;
                    }
                }
            }
            _ => {}
        }
    }
    let mut highest = ctx.lowest;
    for child in &node.children {
        if let Some(child) = child {
            highest = highest.max(ctx.universes[child].inner.into_inner());
            dependencies.extend(ctx.universes[child].dependencies.iter().copied());
        }
    }
    ctx.universes.insert(
        node.id(),
        UniversePrototype {
            dependencies,
            inner: Either::Right(highest),
        },
    );
    
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
        let lowest = 0;
        // map of NodeId to Either<i32, i32>, where i32 represents the universe
        // Either::Left represents a universe that may be changed to a lower value
        // Either::Right represents a universe that may be changed to a higher value
        let universes = HashMap::new();
        // a map of ad-hoc bindings of identifiers to nodes
        // required for dependencies
        let bindings = vec![HashMap::new()];
        let params = HashSet::new();

        let mut ctx = Context {
            lowest,
            universes,
            bindings,
            params,
        };

        visit(_lazy, &nodes, root_node.0, None, &mut ctx)?;

        // PERF: clone is bad
        let u = ctx.universes.clone();

        for (handle, _universe) in &u {
            let node = nodes.get(*handle).unwrap();
            match node.kind {
                Kind::Nil | Kind::Binding | Kind::Declaration => {}
                Kind::Function => {
                    let mut lowest: Option<i32> = None;
                    for child in &node.children {
                        if let Some(child) = child {
                            lowest = lowest.map(|l| l.min(ctx.universes[child].inner.into_inner()));
                        }
                    }
                    ctx.universes.get_mut(handle).unwrap().inner = Either::Left(lowest.unwrap_or(ctx.lowest));
                }
                _ => {
                    let mut highest: Option<i32> = None;
                    for child in &node.children {
                        if let Some(child) = child {
                            highest = highest.map(|h| h.max(ctx.universes[child].inner.into_inner()));
                        }
                    }
                    ctx.universes.get_mut(handle).unwrap().inner = Either::Right(highest.unwrap_or(ctx.lowest));
                }
            }
        }

        for (handle, _universe) in &u {
            let node = nodes.get(*handle).unwrap();
            if matches!(node.kind, Kind::Binding) {
                let u = ctx.universes.get(&node.children[2].unwrap()).unwrap().inner;
                ctx.universes.get_mut(&node.children[0].unwrap()).unwrap().inner = u;
                ctx.universes.get_mut(handle).unwrap().inner = u;
            }
        }
        
        for (handle, universe) in &u {
            let level = universe.inner.into_inner();
            let mut lower: Option<i32> = None;
            for &dep in &universe.dependencies {
                match ctx.universes.get_mut(&dep) {
                    Some(u) => {
                        match u.inner {
                            Either::Right(l) if level > l => u.inner = Either::Left(level),
                            Either::Right(_) => {}
                            Either::Left(l) if level < l => {
                                lower = lower.map(|lower| lower.min(l)).or(Some(l));
                                u.inner = Either::Left(level);
                            }
                            Either::Left(l) => {
                                lower = lower.map(|lower| lower.min(l)).or(Some(l));
                            }
                        }
                    }
                    None => panic!("dependency not found"),
                }
            }
            if let Some(lower) = lower {
                ctx.universes.get_mut(handle).unwrap().inner = Either::Left(lower);
            }
        }

        for (handle, universe) in ctx.universes {
            let level = universe.inner.into_inner();
            nodes.get_mut::<Node>(handle).unwrap().universe = level;
        }

        for handle in ctx.params {
            nodes.get_mut::<Node>(handle).unwrap().is_param = true;
        }
    }
    Ok(None)
}
