use std::iter;

use failure::Fallible;
use hashbrown::{HashMap, HashSet};
use smallvec::SmallVec;

use crate::assets::{Handle, LazyUpdate, Resources};
use crate::ast::{Kind, Node, RootNode, VisitResult, Which};
use crate::builder::BuilderMacro;
use crate::types::primitive;
use crate::universe::Universe;
use crate::values::Payload;

pub fn universe_update(
    _lazy: &mut LazyUpdate,
    roots: Resources<&RootNode>,
    mut nodes: Resources<&mut Node>,
    strings: Resources<&String>,
    builders: Resources<&BuilderMacro>,
) -> Fallible<Option<&'static str>> {
    for (_, root_node) in roots.iter::<RootNode>() {
        let root = nodes.get(root_node.0).unwrap();
        let mut universes: HashMap<_, Universe> = HashMap::new();
        let mut clones = HashSet::new();
        let mut generic = HashMap::new();
        let mut bindings: Vec<HashMap<_, Universe>> = vec![HashMap::new()];

        root.visit_twice(&nodes, |res, node, _, which| {
            match which {
                Which::Before => match node.kind {
                    Kind::Block | Kind::Function => {
                        bindings.push(HashMap::new());
                    }
                    _ => {}
                },
                Which::After => {
                    let mut universe = Universe::Terminal(-(node.amps as i32));
                    match node.kind {
                        Kind::Nil => match node.payload.unwrap() {
                            Payload::Identifier(ident) if !node.alternative => {
                                for bindings in bindings.iter().rev() {
                                    if let Some(u) = bindings.get(&ident) {
                                        universe = u.clone();
                                        break;
                                    }
                                }
                            }
                            _ => {}
                        },
                        // all of these have the type in '1 position
                        Kind::Global | Kind::Binding | Kind::Declaration => {
                            if let Some(binding) = &node.children[1] {
                                universe = universes[binding].clone();
                                universes.insert(*binding, &universe - 1);
                                universes.insert(node.children[0].unwrap(), universe.clone());
                                if universe < 0 {
                                    clones.insert(*binding);
                                }
                            } else if let Some(binding) =
                                node.children.get(2).map(Option::as_ref).flatten()
                            {
                                universe = universes[binding].clone();
                            }
                            // XXX: why does this not work for declarations?
                            if matches!(node.kind, Kind::Global | Kind::Binding) {
                                let ident = node.children[0].unwrap();
                                let ident = res.get(ident).unwrap();
                                let ident = match ident.payload.unwrap() {
                                    Payload::Identifier(ident) => ident,
                                    _ => panic!("not a valid pattern (yet)!"),
                                };
                                universe = match universe {
                                    u @ Universe::Terminal(..) => u,
                                    u @ Universe::MappingStar(..) => u,
                                    Universe::Mapping(params, result) => {
                                        Universe::MappingStar(0, params, result)
                                    }
                                };
                                bindings.last_mut().unwrap().insert(ident, universe.clone());
                            }
                        }
                        Kind::Block | Kind::Tuple | Kind::Array => {
                            for elem in &node.children {
                                if let Some(elem) = elem {
                                    universe = universe.max(&universes[elem]);
                                }
                            }
                        }
                        Kind::Function => {
                            let params = node.children[0].unwrap();
                            let params = res.get(params).unwrap();
                            let mut gen = SmallVec::new();
                            let mut uparams = Vec::new();
                            for child in &params.children {
                                let child = child.unwrap();
                                let child = res.get(child).unwrap();
                                uparams.push(universes[&child.id()].clone());
                                if universes[&child.id()] < 0 {
                                    let ident = child.children[0].unwrap();
                                    let ident = res.get(ident).unwrap();
                                    let ident = match ident.payload.unwrap() {
                                        Payload::Identifier(ident) => ident,
                                        _ => panic!("not an identifier"),
                                    };
                                    gen.push(ident);
                                }
                            }
                            let body = node.children[1].unwrap();
                            universe =
                                Universe::Mapping(uparams, Box::new(universes[&body].clone()));
                            if gen.len() > 0 {
                                generic.insert(node.children[1].unwrap(), gen);
                            }
                        }
                        Kind::With => {
                            let func = &node.children[0].unwrap();
                            universe = universe.min(&universes[func]);
                        }
                        Kind::Application => {
                            let func = &node.children[0].unwrap();
                            let func = res.get(*func).unwrap();
                            if func.alternative {
                                match func.payload.unwrap() {
                                    Payload::Identifier(ident) => {
                                        let alternative = strings.get(ident).unwrap();
                                        let handle = Handle::from_hash(alternative.as_bytes());
                                        if let Some(builder) = builders.get::<BuilderMacro>(handle)
                                        {
                                            let u = builder
                                                .universal(&node, &nodes, &mut universes)
                                                .transpose()
                                                .unwrap_or_else(|err| {
                                                    panic!(
                                                        "i don't know how to handle errors yet: {}",
                                                        err
                                                    )
                                                });
                                            if let Some(u) = u {
                                                universe = u;
                                            }
                                        } else {
                                            panic!(
                                                "not an alternative: `${}`",
                                                alternative.as_str()
                                            );
                                        }
                                    }
                                    Payload::Struct => {
                                        let fields = node.children[1].unwrap();
                                        let fields = res.get(fields).unwrap();
                                        let mut ufields = Vec::new();
                                        for child in &fields.children {
                                            let child = child.unwrap();
                                            let child = res.get(child).unwrap();
                                            ufields.push(universes[&child.id()].clone());
                                        }
                                        let body = Universe::Terminal(0);
                                        universe = Universe::Mapping(ufields, Box::new(body));
                                    }
                                    Payload::Enum
                                    | Payload::Union
                                    | Payload::Tagged
                                    | Payload::Class => universe = Universe::Terminal(0),
                                    _ => todo!(),
                                }
                            } else {
                                let mapping = &universes[&func.id()];
                                match mapping {
                                    Universe::Terminal(_) => {
                                        panic!("application of something not a mapping");
                                    }
                                    Universe::MappingStar(_, _, result)
                                    | Universe::Mapping(_, result) => {
                                        universe = result.as_ref().clone();
                                        let mapping = mapping.value_unstar();
                                        if mapping < universe.value() {
                                            universe = match universe {
                                                Universe::Terminal(_) => universe,
                                                Universe::Mapping(params, result) => {
                                                    Universe::MappingStar(mapping, params, result)
                                                }
                                                Universe::MappingStar(_, params, result) => {
                                                    Universe::MappingStar(mapping, params, result)
                                                }
                                            };
                                        }
                                    }
                                }
                            }
                        }
                        Kind::Assign | Kind::Argument => {
                            // TODO: assert that both children have the same universe
                            for elem in &node.children {
                                if let Some(elem) = elem {
                                    universe = universe.min(&universes[elem]);
                                }
                            }
                        }
                        Kind::Dotted => {
                            let elem = node.children[0].as_ref().unwrap();
                            universe = universe.min(&universes[elem]);
                        }
                        Kind::Index => todo!(),
                    }
                    universes.insert(node.id(), universe);
                }
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

        for handle in clones {
            let mut node = Node::new(Kind::Nil, Handle::nil(), iter::empty());
            node.type_of = Some(*primitive::TYPE);
            node.payload = Some(Payload::Type(*primitive::NODE));
            nodes.get_mut(handle).unwrap().clone_from(node);
        }

        for (handle, universe) in universes {
            nodes.get_mut::<Node>(handle).unwrap().universe = universe;
        }

        for (handle, variables) in generic {
            nodes.get_mut::<Node>(handle).unwrap().generic = Some(variables);
        }
    }
    Ok(None)
}
