use std::mem;

use failure::Fallible;
use hashbrown::{HashMap, HashSet};

use crate::assets::{Handle, LazyUpdate, Resources, Static};
use crate::ast::{Kind, Node, NodeId, RootNode, Visit, VisitResult};
use crate::builder::BuilderMacro;
use crate::dispatch::*;
use crate::error::MultiError;
use crate::lexer::Location;
use crate::lir::{context::ExecutionContext, target::Target};
use crate::pass::infer;
use crate::scope::{Scope, ScopeId};
use crate::types::{NamedType, NonConcrete, TypeId};
use crate::values::Payload;

pub fn collapse(
    typ: TypeId,
    root: &Node,
    node: &Node,
    target: &Target,
    roots: &Resources<&RootNode>,
    scopes: &Resources<&Scope>,
    contexts: &Resources<&ExecutionContext>,
    dispatch: &mut Resources<&mut Dispatcher>,
    named_types: &mut Resources<&mut NamedType>,
    strings: &Resources<&String>,
    builders: &Resources<&BuilderMacro>,
    locations: &Resources<&Location>,
    nodes: &Resources<&mut Node>,
    types: &mut HashMap<NodeId, TypeId>,
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
                mem::drop(results);
                let (t, _) = collapse(
                    t,
                    root,
                    node,
                    target,
                    roots,
                    scopes,
                    contexts,
                    dispatch,
                    named_types,
                    strings,
                    builders,
                    locations,
                    nodes,
                    types,
                )?;
                return Ok((t, Some(scope)));
            }
            // TODO: a proper error type
            Err(failure::err_msg(format!(
                "binding not found: {}",
                string.as_str()
            )))
        }
        NonConcrete::Typeof(id) => {
            if let Some(t) = nodes.get(id).unwrap().type_of {
                collapse(
                    t,
                    root,
                    node,
                    target,
                    roots,
                    scopes,
                    contexts,
                    dispatch,
                    named_types,
                    strings,
                    builders,
                    locations,
                    nodes,
                    types,
                )
            } else {
                infer(
                    root,
                    &nodes.get(id).unwrap(),
                    target,
                    roots,
                    scopes,
                    contexts,
                    dispatch,
                    named_types,
                    strings,
                    builders,
                    locations,
                    nodes,
                    types,
                )?;
                collapse(
                    types[&id],
                    root,
                    node,
                    target,
                    roots,
                    scopes,
                    contexts,
                    dispatch,
                    named_types,
                    strings,
                    builders,
                    locations,
                    nodes,
                    types,
                )
            }
        }
    }
}

pub fn collapse_update(
    _lazy: &mut LazyUpdate,
    target: &Static<Target>,
    roots: Resources<&RootNode>,
    scopes: Resources<&Scope>,
    contexts: Resources<&ExecutionContext>,
    mut dispatch: Resources<&mut Dispatcher>,
    mut named_types: Resources<&mut NamedType>,
    strings: Resources<&String>,
    builders: Resources<&BuilderMacro>,
    locations: Resources<&Location>,
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
                let (t, s) = match collapse(
                    typ,
                    &root,
                    node,
                    &target,
                    &roots,
                    &scopes,
                    &contexts,
                    &mut dispatch,
                    &mut named_types,
                    &strings,
                    &builders,
                    &locations,
                    &nodes,
                    &mut types,
                ) {
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
        for (handle, typ) in types.drain() {
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
                        let (t, s) = match collapse(
                            typ,
                            &root,
                            node,
                            &target,
                            &roots,
                            &scopes,
                            &contexts,
                            &mut dispatch,
                            &mut named_types,
                            &strings,
                            &builders,
                            &locations,
                            &nodes,
                            &mut types,
                        ) {
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
        for (handle, typ) in types {
            nodes.get_mut::<Node>(handle).unwrap().type_of = Some(typ);
        }
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
