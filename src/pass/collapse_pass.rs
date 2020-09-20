use failure::Fallible;
use hashbrown::HashMap;

use crate::assets::{Handle, LazyUpdate, Resources};
use crate::ast::{Node, RootNode, Visit, VisitResult};
use crate::dispatch::*;
use crate::error::MultiError;

use crate::types::{NamedType, TypeOrPlaceholder};
use crate::values::Payload;

pub fn collapse_update(
    _lazy: &mut LazyUpdate,
    roots: Resources<&RootNode>,
    dispatch: Resources<&Dispatcher>,
    named_types: Resources<&NamedType>,
    mut nodes: Resources<&mut Node>,
) -> Fallible<Option<&'static str>> {
    let errors = Vec::new();
    for (_, root_node) in roots.iter::<RootNode>() {
        let root = nodes.get::<Node>(root_node.0).unwrap();
        let mut payloads = HashMap::new();
        let mut types = HashMap::new();
        root.visit(Visit::Postorder, &nodes, |_res, node| {
            if let Some(payload) = node.payload {
                match payload {
                    Payload::Type(typ) => {
                        match typ.concrete {
                            TypeOrPlaceholder::Type(_) => {}
                            TypeOrPlaceholder::Placeholder(_) => {}
                            TypeOrPlaceholder::Dispatch(scope, name) => {
                                let id = Handle::from_name(scope, &name.as_u128().to_le_bytes());
                                let dispatcher = dispatch.get::<Dispatcher>(id).unwrap();
                                let query =
                                    Query::new(Name(scope, name), IsFunction::Maybe, None, None);
                                let results = dispatcher.query(&query, &named_types);
                                let t = match results.len() {
                                    // TODO: traverse parents
                                    0 => todo!(),
                                    1 => results[0].result_type(),
                                    _ => todo!(),
                                };
                                payloads.insert(node.id(), Payload::Type(t));
                            }
                            TypeOrPlaceholder::Typeof(id) => {
                                payloads.insert(
                                    node.id(),
                                    Payload::Type(nodes.get(id).unwrap().type_of.unwrap()),
                                );
                            }
                        }
                    }
                    _ => {}
                }
            }
            if let Some(typ) = node.type_of {
                match typ.concrete {
                    TypeOrPlaceholder::Type(_) => {}
                    TypeOrPlaceholder::Placeholder(_) => {}
                    TypeOrPlaceholder::Dispatch(scope, name) => {
                        let id = Handle::from_name(scope, &name.as_u128().to_le_bytes());
                        let dispatcher = dispatch.get::<Dispatcher>(id).unwrap();
                        let query = Query::new(Name(scope, name), IsFunction::Maybe, None, None);
                        let results = dispatcher.query(&query, &named_types);
                        let t = match results.len() {
                            // TODO: traverse parents
                            0 => todo!(),
                            1 => results[0].result_type(),
                            _ => todo!(),
                        };
                        types.insert(node.id(), t);
                    }
                    TypeOrPlaceholder::Typeof(id) => {
                        types.insert(node.id(), nodes.get(id).unwrap().type_of.unwrap());
                    }
                }
            }
            VisitResult::Recurse
        });
        for (handle, payload) in payloads {
            nodes.get_mut::<Node>(handle).unwrap().payload = Some(payload);
        }
        for (handle, typ) in types {
            nodes.get_mut::<Node>(handle).unwrap().type_of = Some(typ);
        }
    }
    if errors.is_empty() {
        Ok(None)
    } else {
        Err(From::from(MultiError::from(errors)))
    }
}
