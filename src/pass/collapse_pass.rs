use failure::Fallible;
use hashbrown::{HashMap, HashSet};

use crate::assets::{AssetBundle, Handle, LazyUpdate, Resources};
use crate::ast::{Kind, Node, RootNode, Visit, VisitResult};
use crate::dispatch::*;
use crate::error::MultiError;
use crate::scope::{Scope, ScopeId};
use crate::types::{NamedType, NonConcrete, TypeId};
use crate::values::Payload;

pub fn collapse<A: AssetBundle, B: AssetBundle>(
    typ: TypeId,
    scopes: &Resources<&Scope>,
    strings: &Resources<&String>,
    dispatch: &Resources<B>,
    named_types: &Resources<A>,
    nodes: &Resources<&mut Node>,
) -> Fallible<(TypeId, Option<ScopeId>)> {
    match typ.concrete {
        NonConcrete::Type(_) => Ok((typ, None)),
        NonConcrete::Dispatch(scope, name) => {
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
                let query = Query::new(Name(scope, name), IsFunction::Maybe, None, None);
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
                let (t, _) = collapse(t, scopes, strings, dispatch, named_types, nodes)?;
                return Ok((t, Some(scope)));
            }
            // TODO: a proper error type
            Err(failure::err_msg(format!(
                "binding not found: {}",
                string.as_str()
            )))
        }
        NonConcrete::Typeof(id) => collapse(
            nodes.get(id).unwrap().type_of.unwrap(),
            scopes,
            strings,
            dispatch,
            named_types,
            nodes,
        ),
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

        let mut skip = HashSet::new();

        root.visit(Visit::Preorder, &nodes, |res, node, _| match node.kind {
            Kind::Application => {
                let func = node.children[0].unwrap();
                let func = res.get(func).unwrap();
                if !func.alternative {
                    return VisitResult::Recurse;
                }
                match func.kind {
                    Kind::Nil => match func.payload.unwrap() {
                        Payload::Identifier(string) => {
                            match strings.get::<String>(string).unwrap().as_str() {
                                "quasiquote" => {
                                    node.visit(Visit::Postorder, res, |_, node, _| {
                                        skip.insert(node.id());
                                        VisitResult::Recurse
                                    });
                                    skip.remove(&node.id());
                                    skip.remove(&func.id());
                                    VisitResult::Continue
                                }
                                _ => VisitResult::Recurse,
                            }
                        }
                        _ => VisitResult::Recurse,
                    },
                    _ => VisitResult::Recurse,
                }
            }
            _ => VisitResult::Recurse,
        });

        root.visit(Visit::Postorder, &nodes, |_res, node, _| {
            if skip.contains(&node.id()) {
                return VisitResult::Continue;
            }

            if let Some(typ) = node.type_of {
                let (t, s) = match collapse(typ, &scopes, &strings, &dispatch, &named_types, &nodes)
                {
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
            if skip.contains(&node.id()) {
                return VisitResult::Continue;
            }

            if let Some(payload) = node.payload {
                match payload {
                    Payload::Type(typ) => {
                        let (t, s) =
                            match collapse(typ, &scopes, &strings, &dispatch, &named_types, &nodes)
                            {
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
