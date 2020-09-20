use either::Either;
use failure::Fallible;
use hashbrown::HashMap;

use crate::assets::{LazyUpdate, Resources};
use crate::ast::{Kind, Node, RootNode, Visit, VisitResult};

pub fn universe_update(
    _lazy: &mut LazyUpdate,
    roots: Resources<&RootNode>,
    mut nodes: Resources<&mut Node>,
) -> Fallible<Option<&'static str>> {
    for (_, root_node) in roots.iter::<RootNode>() {
        let root = nodes.get::<Node>(root_node.0).unwrap();
        let mut universe = 0;
        // map of NodeId to Either<i32, i32>, where i32 represents the universe
        // Either::Left represents a fixed universe,
        // Either::Right represents a universe that may be changed to a higher value
        let mut universes = HashMap::new();

        root.visit(Visit::Postorder, &nodes, |res, node| {
            match node.kind {
                Kind::Application
                | Kind::Argument
                | Kind::Array
                | Kind::Block
                | Kind::Dotted
                | Kind::Function
                | Kind::Index
                | Kind::Nil
                | Kind::Tuple => {
                    universes.insert(node.id(), Either::Right(universe));

                    for &child in &node.children {
                        if let Some(handle) = child {
                            let child = res.get(handle).unwrap();
                            child.visit(Visit::Postorder, res, |_res, node| {
                                let handle = node.id();
                                let u = universes
                                    .get(&handle)
                                    .copied()
                                    .unwrap_or_else(|| Either::Right(universe));
                                match u {
                                    Either::Left(_) => {}
                                    Either::Right(_) => {
                                        universes.insert(handle, Either::Left(universe));
                                    }
                                }
                                VisitResult::Recurse
                            });
                        }
                    }

                    VisitResult::Recurse
                }
                // both of those have the type in position 1, so this is fine
                Kind::Binding | Kind::Declaration => {
                    // TODO: rule 2
                    if let Some(handle) = node.children[1] {
                        let u = universes
                            .get(&handle)
                            .copied()
                            .unwrap_or_else(|| Either::Right(universe));
                        match u {
                            Either::Left(_) => {}
                            Either::Right(_) => {
                                universes.insert(handle, Either::Left(universe));
                            }
                        }
                        universe += 1;
                        universes.insert(node.id(), Either::Right(universe));
                        VisitResult::Repeat(node.id())
                    } else {
                        VisitResult::Recurse
                    }
                }
            }
        });

        for (handle, universe) in universes {
            nodes.get_mut::<Node>(handle).unwrap().universe = universe.into_inner();
        }
    }
    Ok(None)
}
