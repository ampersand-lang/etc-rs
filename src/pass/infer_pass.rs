use std::iter;

use failure::{Fail, Fallible};
use hashbrown::{HashMap, HashSet};
use smallvec::smallvec;

use crate::assets::{Handle, LazyUpdate, Resources};
use crate::ast::{Kind, Node, RootNode, Visit, VisitResult};
use crate::dispatch::*;
use crate::error::MultiError;
use crate::lexer::Location;
use crate::types::{builtin, primitive, NamedType, Type, TypeGroup, TypeId, TypeOrPlaceholder};
use crate::values::Payload;

#[derive(Debug, Fail)]
pub enum InferError {
    #[fail(
        display = "type error at {}: expected {:?}, but got {:?}",
        location, expected, got
    )]
    TypeError {
        location: Location,
        expected: TypeId,
        got: TypeId,
    },
    #[fail(display = "type at {} is not a function: {:?}", location, typ)]
    NotAFunction { location: Location, typ: TypeId },
    #[fail(display = "no matching definition found at {}", location)]
    NoDefinitions { location: Location },
    #[fail(display = "multiple matching definition found at {}", location)]
    MultipleDefinitions { location: Location },
}

pub fn infer_update(
    lazy: &mut LazyUpdate,
    roots: Resources<&RootNode>,
    mut dispatch: Resources<&mut Dispatcher>,
    named_types: Resources<&NamedType>,
    strings: Resources<&String>,
    locations: Resources<&Location>,
    mut nodes: Resources<&mut Node>,
) -> Fallible<Option<&'static str>> {
    let mut errors = Vec::new();
    for (_, root_node) in roots.iter::<RootNode>() {
        let root = nodes.get::<Node>(root_node.0).unwrap();
        let mut skip = HashSet::new();
        let mut types = HashMap::new();
        root.visit(Visit::Preorder, &nodes, |res, node| match node.kind {
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
                                    node.visit(Visit::Postorder, res, |_, node| {
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
        root.visit(Visit::Postorder, &nodes, |_res, node| {
            if skip.contains(&node.id()) {
                return VisitResult::Recurse;
            }
            match node.kind {
                Kind::Nil => {
                    let typ = match node.payload.unwrap() {
                        Payload::Unit => primitive::UNIT.clone(),
                        Payload::Integer(_) => primitive::SINT.clone(),
                        Payload::Float(_) => primitive::FLOAT.clone(),
                        Payload::String(_) => {
                            let pointee = primitive::S8.clone();
                            let handle = Handle::new();
                            let t = NamedType {
                                name: None,
                                t: Type::Pointer(pointee),
                            };
                            lazy.insert(handle, t);
                            TypeId {
                                group: TypeGroup::Pointer,
                                concrete: TypeOrPlaceholder::Type(handle),
                            }
                        }
                        Payload::Identifier(string) if node.alternative => {
                            match strings.get::<String>(string).unwrap().as_str() {
                                "declare" => builtin::DECLARE.clone(),
                                "ptr" => builtin::PTR.clone(),
                                "fn" => builtin::FN.clone(),
                                "format-ast" => builtin::FORMAT_AST.clone(),
                                "new-node" => builtin::NEW_NODE.clone(),
                                "quasiquote" => builtin::QUASIQUOTE.clone(),
                                ident => panic!("not a builtin identifier: `${}`", ident),
                            }
                        }
                        Payload::Identifier(string) => TypeId {
                            group: TypeGroup::None,
                            concrete: TypeOrPlaceholder::Dispatch(node.scope.unwrap(), string),
                        },
                        Payload::Type(_) => primitive::TYPE.clone(),
                        Payload::Function(_) => todo!(),
                    };
                    types.insert(node.id(), typ);
                }
                Kind::Block => {
                    let typ = node.children.last().and_then(|last| {
                        last.and_then(|last| {
                            nodes
                                .get::<Node>(last)
                                .and_then(|last| last.type_of.clone())
                                .or_else(|| types.get(&last).copied())
                        })
                    });
                    if let Some(typ) = typ {
                        types.insert(node.id(), typ);
                    }
                }
                Kind::Function => {
                    let param_tuple = &*nodes.get::<Node>(node.children[0].unwrap()).unwrap();
                    let param_types = match param_tuple.kind {
                        Kind::Nil => {
                            match param_tuple.payload.unwrap() {
                                Payload::Identifier(_) => smallvec![TypeId::new_placeholder()],
                                // other patterns
                                _ => todo!(),
                            }
                        }
                        Kind::Tuple => {
                            param_tuple
                                .children
                                .iter()
                                .map(|param| {
                                    let param = &*nodes.get::<Node>(param.unwrap()).unwrap();
                                    match param.kind {
                                        Kind::Nil => {
                                            match param.payload.unwrap() {
                                                Payload::Identifier(_) => TypeId::new_placeholder(),
                                                // other patterns
                                                _ => todo!(),
                                            }
                                        }
                                        // other patterns
                                        _ => todo!(),
                                    }
                                })
                                .collect()
                        }
                        _ => panic!("did the validation stage run?"),
                    };
                    let result_type = node.children.get(1).and_then(|last| {
                        last.and_then(|last| nodes.get::<Node>(last).unwrap().type_of.clone())
                    });
                    if let Some(result_type) = result_type {
                        let named = Handle::new();
                        lazy.insert(
                            named,
                            NamedType {
                                name: None,
                                t: Type::Function {
                                    result_type,
                                    param_types,
                                },
                            },
                        );
                        let typ = TypeId {
                            group: TypeGroup::Function,
                            concrete: TypeOrPlaceholder::Type(named),
                        };
                        types.insert(node.id(), typ);
                    }
                }
                Kind::Application => {
                    let func = nodes.get::<Node>(node.children[0].unwrap()).unwrap();
                    let typ = func
                        .type_of
                        .or_else(|| types.get(&node.children[0].unwrap()).copied())
                        .unwrap();
                    let typ = match typ.concrete {
                        TypeOrPlaceholder::Type(handle) => {
                            match &named_types.get::<NamedType>(handle).unwrap().t {
                                Type::Function {
                                    result_type,
                                    param_types,
                                } => Some((result_type.clone(), param_types.clone())),
                                _ => {
                                    errors.push(From::from(InferError::NotAFunction {
                                        location: locations
                                            .get::<Location>(func.location)
                                            .unwrap()
                                            .as_ref()
                                            .clone(),
                                        typ,
                                    }));
                                    return VisitResult::Recurse;
                                }
                            }
                        }
                        TypeOrPlaceholder::Dispatch(scope, handle) => {
                            let ident = strings.get(handle).unwrap();
                            let name = Name(scope, handle);
                            let args = node.children[1..]
                                .iter()
                                .map(|param| {
                                    nodes.get::<Node>(param.unwrap()).unwrap().type_of.unwrap()
                                })
                                .collect();
                            let dispatch = dispatch
                                .get::<Dispatcher>(DispatchId::from_name(
                                    name.0,
                                    ident.as_bytes(),
                                ))
                                .unwrap();
                            let query = Query::new(name, IsFunction::Yes, Some(args), None);
                            let results = dispatch.query(&query, &named_types);
                            match results.len() {
                                // TODO: traverse parents
                                0 => todo!(),
                                1 => Some((
                                    results[0].result_type(),
                                    results[0].arg_types().unwrap().iter().copied().collect(),
                                )),
                                _ => {
                                    errors.push(From::from(InferError::MultipleDefinitions {
                                        location: locations
                                            .get::<Location>(func.location)
                                            .unwrap()
                                            .as_ref()
                                            .clone(),
                                    }));
                                    return VisitResult::Recurse;
                                }
                            }
                        }
                        _ => todo!(),
                    };
                    if let Some((result_type, param_types)) = typ {
                        for (idx, t) in param_types.into_iter().enumerate() {
                            if t.group == TypeGroup::Node {
                                let handle = node.children[1 + idx].unwrap();
                                types.insert(handle, t);
                            }
                        }
                        types.insert(node.id(), result_type);
                    }
                }
                Kind::Binding => {
                    let ident = nodes.get::<Node>(node.children[0].unwrap()).unwrap();
                    let ident = match ident.kind {
                        Kind::Nil => {
                            match ident.payload.unwrap() {
                                Payload::Identifier(ident) => ident,
                                // TODO: other bindings
                                _ => todo!(),
                            }
                        }
                        // TODO: other bindings
                        _ => todo!(),
                    };
                    let name = strings.get(ident).unwrap();
                    let scope = node.scope.unwrap();
                    let handle = DispatchId::from_name(scope, name.as_bytes());
                    let binding = nodes
                        .get::<Node>(node.children[2].unwrap())
                        .and_then(|node| node.type_of)
                        .or_else(|| types.get(&node.children[2].unwrap()).copied())
                        .unwrap();
                    let (result_type, param_types) = match binding.concrete {
                        TypeOrPlaceholder::Type(t) => {
                            match named_types.get::<NamedType>(t).unwrap().t {
                                Type::Function {
                                    result_type,
                                    ref param_types,
                                } => (result_type, Some(param_types.clone())),
                                _ => (binding, None),
                            }
                        }
                        _ => (binding, None),
                    };
                    let d = if let Some(mut d) = dispatch.remove(handle) {
                        if let Some(param_types) = param_types {
                            d.push(Definition::new_function(
                                node.universe,
                                param_types,
                                result_type,
                            ));
                        } else {
                            d.push(Definition::new_variable(node.universe, result_type));
                        }
                        d
                    } else {
                        let name = Name(scope, ident);
                        if let Some(param_types) = param_types {
                            Dispatcher::with_definitions(
                                name,
                                iter::once(Definition::new_function(
                                    node.universe,
                                    param_types,
                                    result_type,
                                )),
                            )
                        } else {
                            Dispatcher::with_definitions(
                                name,
                                iter::once(Definition::new_variable(node.universe, result_type)),
                            )
                        }
                    };
                    types.insert(node.id(), binding);
                    lazy.insert(handle, d);
                }
                Kind::Tuple => {
                    if node.alternative {
                        types.insert(node.id(), *primitive::TYPE);
                        return VisitResult::Recurse;
                    }
                    let fields = node
                        .children
                        .iter()
                        .map(|param| nodes.get::<Node>(param.unwrap()).unwrap().type_of.unwrap())
                        .collect();
                    let named = Handle::new();
                    lazy.insert(
                        named,
                        NamedType {
                            name: None,
                            t: Type::Struct { fields },
                        },
                    );
                    let typ = TypeId {
                        group: TypeGroup::Struct,
                        concrete: TypeOrPlaceholder::Type(named),
                    };
                    types.insert(node.id(), typ);
                }
                Kind::Argument => todo!(),
                Kind::Declaration => todo!(),
                Kind::Array => todo!(),
                Kind::Index => todo!(),
                Kind::Dotted => todo!(),
            }

            VisitResult::Recurse
        });
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
