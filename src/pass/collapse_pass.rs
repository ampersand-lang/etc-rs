use failure::Fallible;
use hashbrown::HashMap;

use crate::assets::{Handle, LazyUpdate, Resources};
use crate::ast::{Node, RootNode, Visit, VisitResult};
use crate::dispatch::*;
use crate::error::MultiError;
use crate::scope::{Scope, ScopeId};

use crate::types::{NamedType, TypeId, TypeOrPlaceholder};
use crate::values::Payload;

pub fn collapse(
    typ: TypeId,
    scopes: &Resources<&Scope>,
    strings: &Resources<&String>,
    dispatch: &Resources<&Dispatcher>,
    named_types: &Resources<&NamedType>,
    nodes: &Resources<&mut Node>,
) -> Fallible<(TypeId, Option<ScopeId>)> {
    match typ.concrete {
        TypeOrPlaceholder::Type(_) => Ok((typ, None)),
        TypeOrPlaceholder::Placeholder(_) => Ok((typ, None)),
        TypeOrPlaceholder::Dispatch(scope, name) => {
            let string = strings.get(name).unwrap();
            let mut iter = Some(scope);
            while let Some(scope) = iter {
                let id = Handle::from_name(scope, string.as_bytes());
                let dispatcher = if let Some(d) = dispatch.get::<Dispatcher>(id) {
                    d
                } else {
                    let scope = scopes.get(scope);
                    if let Some(scope) = scope {
                        iter = Some(scope.parent());
                    } else {
                        iter = None;
                    }
                    continue;
                };
                let query =
                    Query::new(Name(scope, name), IsFunction::Maybe, None, None);
                let results = dispatcher.query(&query, &named_types);
                let t = match results.len() {
                    0 => {
                        let scope = scopes.get(scope);
                        if let Some(scope) = scope {
                            iter = Some(scope.parent());
                        } else {
                            iter = None;
                        }
                        continue;
                    }
                    1 => results[0].result_type(),
                    _ => todo!(),
                };
                return Ok((t, Some(scope)));
            }
            // TODO: a proper error type
            Err(failure::err_msg(format!("binding not found: {}", string.as_str())))
        }
        TypeOrPlaceholder::Typeof(id) => {
            collapse(nodes.get(id).unwrap().type_of.unwrap(), scopes, strings, dispatch, named_types, nodes)
        }
    }
}

pub fn collapse_update(
    _lazy: &mut LazyUpdate,
    roots: Resources<&RootNode>,
    scopes: Resources<&Scope>,
    strings: Resources<&String>,
    dispatch: Resources<&Dispatcher>,
    named_types: Resources<&NamedType>,
    mut nodes: Resources<&mut Node>,
) -> Fallible<Option<&'static str>> {
    let mut errors = Vec::new();
    for (_, root_node) in roots.iter::<RootNode>() {
        let root = nodes.get::<Node>(root_node.0).unwrap();
        let mut new_scopes = HashMap::new();
        let mut payloads = HashMap::new();
        let mut types = HashMap::new();
        root.visit(Visit::Postorder, &nodes, |_res, node, _| {
            if let Some(typ) = node.type_of {
                let (t, s) = match collapse(typ, &scopes, &strings, &dispatch, &named_types, &nodes) {
                    Ok(tuple) => tuple,
                    Err(err) => {
                        errors.push(err);
                        return VisitResult::Recurse;
                    }
                };
                types.insert(node.id(), t);
                if let Some(s) = s {
                    new_scopes.insert(node.id(), s);
                }
            }
            VisitResult::Recurse
        });
        for (handle, typ) in types {
            nodes.get_mut::<Node>(handle).unwrap().type_of = Some(typ);
        }
        let root = nodes.get::<Node>(root_node.0).unwrap();
        root.visit(Visit::Postorder, &nodes, |_res, node, _| {
            if let Some(payload) = node.payload {
                match payload {
                    Payload::Type(typ) => {
                        let (t, s) = match collapse(typ, &scopes, &strings, &dispatch, &named_types, &nodes) {
                            Ok(tuple) => tuple,
                            Err(err) => {
                                errors.push(err);
                                return VisitResult::Recurse;
                            }
                        };
                        payloads.insert(node.id(), Payload::Type(t));
                        if let Some(s) = s {
                            new_scopes.insert(node.id(), s);
                        }
                    }
                    _ => {}
                }
            }
            VisitResult::Recurse
        });
        //for (handle, scope) in new_scopes {
        //    nodes.get_mut::<Node>(handle).unwrap().scope = Some(scope);
        //}
        for (handle, payload) in payloads {
            nodes.get_mut::<Node>(handle).unwrap().payload = Some(payload);
        }
    }
    if errors.is_empty() {
        Ok(None)
    } else {
        Err(From::from(MultiError::from(errors)))
    }
}
