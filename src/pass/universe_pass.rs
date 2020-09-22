use either::Either;
use failure::Fallible;
use hashbrown::HashMap;
use smallvec::SmallVec;

use crate::assets::{LazyUpdate, Resources};
use crate::ast::{Kind, Node, NodeId, RootNode, Visit, VisitResult};
use crate::values::Payload;

#[derive(Debug, Clone)]
struct UniversePrototype {
    dependencies: SmallVec<[NodeId; 4]>,
    inner: Either<i32, i32>,
}

pub fn universe_update(
    _lazy: &mut LazyUpdate,
    roots: Resources<&RootNode>,
    strings: Resources<&String>,
    mut nodes: Resources<&mut Node>,
) -> Fallible<Option<&'static str>> {
    for (_, root_node) in roots.iter::<RootNode>() {
        let root = nodes.get::<Node>(root_node.0).unwrap();
        let mut universe = 0;
        // map of NodeId to Either<i32, i32>, where i32 represents the universe
        // Either::Left represents a universe that may be changed to a lower value
        // Either::Right represents a universe that may be changed to a lower left or higher value right
        let mut universes = HashMap::<NodeId, UniversePrototype>::new();

        root.visit(Visit::Postorder, &nodes, |_res, node| {
            let result = match node.kind {
                Kind::Application
                | Kind::Argument
                | Kind::Array
                | Kind::Block
                | Kind::Dotted
                | Kind::Function
                | Kind::Index
                | Kind::Nil
                | Kind::Tuple
                | Kind::With => {
                    let mut dependencies = SmallVec::new();
                    if let Some(payload) = node.payload {
                        match payload {
                            Payload::Identifier(ident) => {
                                let ident = strings.get::<String>(ident).unwrap();
                                let node = NodeId::from_hash(ident.as_bytes());
                                if nodes.get(node).is_some() {
                                    dependencies.push(node);
                                }
                            }
                            _ => {}
                        }
                    }
                    for child in &node.children {
                        if let Some(child) = child {
                            dependencies.extend(universes[child].dependencies.iter().copied());
                        }
                    }
                    universes.insert(
                        node.id(),
                        UniversePrototype {
                            dependencies,
                            inner: Either::Right(universe),
                        },
                    );
                    VisitResult::Recurse
                }
                // both of those have the type in position 1, so this is fine
                Kind::Binding | Kind::Declaration => {
                    // TODO: rule 2
                    if let Some(handle) = node.children[1] {
                        let u = universes.get(&handle).cloned().unwrap();
                        match u.inner {
                            Either::Left(_) => {}
                            Either::Right(_) => {
                                universes.get_mut(&handle).unwrap().inner = Either::Left(universe);
                            }
                        }
                        universe += 1;
                        for (_, v) in &mut universes {
                            match &mut v.inner {
                                Either::Left(_) => {}
                                Either::Right(v) => *v = universe,
                            }
                        }
                    }
                    let mut dependencies = SmallVec::new();
                    for child in &node.children {
                        if let Some(child) = child {
                            dependencies.extend(universes[child].dependencies.iter().copied());
                        }
                    }
                    universes.insert(
                        node.id(),
                        UniversePrototype {
                            dependencies,
                            inner: Either::Right(universe),
                        },
                    );
                    VisitResult::Recurse
                }
            };
            result
        });

        // PERF: clone is bad
        let u = universes.clone();

        for (_, universe) in u {
            let level = universe.inner.into_inner();
            for dep in universe.dependencies {
                match universes.get_mut(&dep) {
                    Some(u) => {
                        match u.inner {
                            Either::Right(l) if level == l => {}
                            Either::Right(_) => u.inner = Either::Left(level),
                            Either::Left(l) if level <= l => u.inner = Either::Left(level),
                            Either::Left(_) => { /* ignore */ }
                        }
                    }
                    None => panic!("dependency not found"),
                }
            }
        }

        for (handle, universe) in universes {
            let level = universe.inner.into_inner();
            nodes.get_mut::<Node>(handle).unwrap().universe = level;
        }
    }
    Ok(None)
}
