use std::iter;

use failure::{Fail, Fallible};
use hashbrown::HashMap;
use smallvec::SmallVec;

use crate::assets::{Handle, LazyUpdate, Resources};
use crate::ast::{Kind, Node, RootNode, VisitResult, Which};
use crate::dispatch::*;
use crate::error::MultiError;
use crate::lexer::Location;
use crate::lir::context::ExecutionContext;
use crate::scope::Scope;
use crate::types::{builtin, primitive, NamedType, NonConcrete, Type, TypeGroup, TypeId};
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
    _lazy: &mut LazyUpdate,
    roots: Resources<&RootNode>,
    scopes: Resources<&Scope>,
    contexts: Resources<&ExecutionContext>,
    mut dispatch: Resources<&mut Dispatcher>,
    mut named_types: Resources<&mut NamedType>,
    strings: Resources<&String>,
    locations: Resources<&Location>,
    mut nodes: Resources<&mut Node>,
) -> Fallible<Option<&'static str>> {
    let mut errors = Vec::new();
    for (_, root_node) in roots.iter::<RootNode>() {
        let root = nodes.get::<Node>(root_node.0).unwrap();
        let mut types = HashMap::new();

        root.visit_twice(&nodes, |_res, node, _, which| {
            if matches!(which, Which::Before) {
                match node.kind {
                    Kind::Application => {
                        let func = nodes.get::<Node>(node.children[0].unwrap()).unwrap();
                        match func.payload {
                            Some(Payload::Identifier(string)) if func.alternative => {
                                match strings.get::<String>(string).unwrap().as_str() {
                                    "replace" => return VisitResult::Continue,
                                    _ => {}
                                }
                            }
                            _ => {}
                        }
                    }
                    _ => {}
                }
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
                            named_types.insert(handle, t);
                            TypeId {
                                group: TypeGroup::Pointer,
                                concrete: NonConcrete::Type(handle),
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
                        Payload::Identifier(string) => {
                            println!(
                                "{:?}: {:?}",
                                strings.get(string).unwrap().as_str(),
                                node.scope.unwrap(),
                            );
                            TypeId {
                                group: TypeGroup::None,
                                concrete: NonConcrete::Dispatch(node.scope.unwrap(), string),
                            }
                        }
                        Payload::Type(_) => primitive::TYPE.clone(),
                        Payload::Function(id) => {
                            let ctx = contexts.get(root.thread.unwrap()).unwrap();
                            let funk = ctx.function(id);
                            let param_types = funk.param_types().iter().copied().collect();
                            let result_type = funk.result_type();
                            let named = Handle::new();
                            named_types.insert(
                                named,
                                NamedType {
                                    name: None,
                                    t: Type::Function {
                                        result_type,
                                        param_types,
                                    },
                                },
                            );
                            TypeId {
                                group: TypeGroup::Function,
                                concrete: NonConcrete::Type(named),
                            }
                        }
                    };
                    types.insert(node.id(), typ);
                }
                Kind::Block => {
                    let last = node.children.last().map(Option::as_ref).flatten();
                    if let Some(&last) = last {
                        let last = nodes.get::<Node>(last).unwrap();
                        let typ = types
                            .get(&last.id())
                            .copied()
                            .or_else(|| last.type_of)
                            .unwrap();
                        types.insert(node.id(), typ);
                    } else {
                        types.insert(node.id(), *primitive::UNIT);
                    }
                }
                Kind::Function => {
                    let mut none = false;
                    let param_tuple = &*nodes.get::<Node>(node.children[0].unwrap()).unwrap();
                    let param_types = match param_tuple.kind {
                        Kind::Tuple => {
                            let iter = param_tuple.children.iter().map(|param| {
                                let param = &*nodes.get::<Node>(param.unwrap()).unwrap();
                                match param.kind {
                                    Kind::Declaration => {
                                        param.type_of.or_else(|| types.get(&param.id()).copied())
                                    }
                                    // other patterns
                                    _ => todo!(),
                                }
                            });
                            let mut params = SmallVec::new();
                            for param in iter {
                                match param {
                                    Some(param) => params.push(param),
                                    None => none = true,
                                }
                            }
                            params
                        }
                        _ => panic!("did the validation stage run?"),
                    };
                    if !none {
                        let body = node.children[1].unwrap();
                        let body = nodes.get(body).unwrap();
                        let result_type = types
                            .get(&body.id())
                            .copied()
                            .or_else(|| body.type_of)
                            .unwrap();
                        let named = Handle::new();
                        named_types.insert(
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
                            concrete: NonConcrete::Type(named),
                        };
                        types.insert(node.id(), typ);
                    }
                }
                Kind::Application => {
                    let func = nodes.get::<Node>(node.children[0].unwrap()).unwrap();
                    let typ = types
                        .get(&func.id())
                        .copied()
                        .or_else(|| func.unquote_type)
                        .or_else(|| func.type_of);
                    let args = node.children[1..].to_vec();
                    let typ = typ.and_then(|typ| match typ.concrete {
                        NonConcrete::Type(handle) => {
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
                                    return None;
                                }
                            }
                        }
                        NonConcrete::Dispatch(scope, handle) => {
                            let args: SmallVec<_> = args
                                .iter()
                                .map(|param| {
                                    let id = param.unwrap();
                                    nodes
                                        .get::<Node>(id)
                                        .unwrap()
                                        .type_of
                                        .or_else(|| types.get(&id).copied())
                                        .unwrap()
                                })
                                .collect();
                            let ident = strings.get(handle).unwrap();
                            let mut iter = Some(scope);
                            let mut result = None;
                            while let Some(scope) = iter {
                                let name = Name(scope, handle);
                                let dispatch = dispatch.get::<Dispatcher>(DispatchId::from_name(
                                    name.0,
                                    ident.as_bytes(),
                                ));
                                let dispatch = if let Some(d) = dispatch {
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
                                    Query::new(name, IsFunction::Yes, Some(args.clone()), None);
                                let results = dispatch.query(&query, &named_types);
                                match results.len() {
                                    0 => {
                                        let scope = scopes.get(scope);
                                        if let Some(scope) = scope {
                                            iter = Some(scope.parent());
                                        } else {
                                            iter = None;
                                        }
                                        continue;
                                    }
                                    1 => {
                                        result = Some((
                                            results[0].result_type(),
                                            results[0]
                                                .arg_types()
                                                .unwrap()
                                                .iter()
                                                .copied()
                                                .collect(),
                                        ));
                                        break;
                                    }
                                    _ => {
                                        errors.push(From::from(InferError::MultipleDefinitions {
                                            location: locations
                                                .get::<Location>(func.location)
                                                .unwrap()
                                                .as_ref()
                                                .clone(),
                                        }));
                                        return None;
                                    }
                                }
                            }
                            result
                        }
                        _ => todo!(),
                    });
                    if let Some((result_type, param_types)) = typ {
                        if !func.alternative && args.len() < param_types.len() {
                            todo!()
                        } else {
                            let named = Handle::new();
                            named_types.insert(
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
                                concrete: NonConcrete::Type(named),
                            };
                            types.insert(func.id(), typ);
                            types.insert(node.id(), result_type);
                        }
                    }
                }
                Kind::Binding | Kind::Global => {
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
                    let binding = nodes.get::<Node>(node.children[2].unwrap()).unwrap();
                    let binding = types
                        .get(&binding.id())
                        .copied()
                        .or_else(|| binding.unquote_type)
                        .or_else(|| binding.type_of);
                    if let Some(binding) = binding {
                        let (result_type, param_types) = match binding.concrete {
                            NonConcrete::Type(t) => {
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
                                    iter::once(Definition::new_variable(
                                        node.universe,
                                        result_type,
                                    )),
                                )
                            }
                        };
                        types.insert(node.id(), binding);
                        dispatch.insert(handle, d);
                    }
                }
                Kind::Tuple => {
                    if node.alternative {
                        types.insert(node.id(), *primitive::TYPE);
                        return VisitResult::Recurse;
                    }
                    let iter = node.children.iter().map(|param| {
                        nodes
                            .get::<Node>(param.unwrap())
                            .unwrap()
                            .type_of
                            .or_else(|| types.get(&param.unwrap()).copied())
                    });
                    let mut none = false;
                    let mut fields = SmallVec::new();
                    for field in iter {
                        match field {
                            Some(field) => fields.push(field),
                            None => none = true,
                        }
                    }
                    if !none {
                        let named = Handle::new();
                        named_types.insert(
                            named,
                            NamedType {
                                name: None,
                                t: Type::Struct { fields },
                            },
                        );
                        let typ = TypeId {
                            group: TypeGroup::Struct,
                            concrete: NonConcrete::Type(named),
                        };
                        types.insert(node.id(), typ);
                    }
                }
                Kind::Argument => todo!(),
                Kind::Declaration => {
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
                    let type_of = nodes
                        .get::<Node>(node.children[1].unwrap())
                        .unwrap()
                        .payload;
                    if let Some(type_of) = type_of {
                        let type_of = match type_of {
                            Payload::Type(t) => t,
                            Payload::Identifier(string) => TypeId {
                                group: TypeGroup::None,
                                concrete: NonConcrete::Dispatch(node.scope.unwrap(), string),
                            },
                            _ => todo!(),
                        };

                        let d = if let Some(mut d) = dispatch.remove(handle) {
                            d.push(Definition::new_variable(node.universe, type_of));
                            d
                        } else {
                            let name = Name(scope, ident);
                            Dispatcher::with_definitions(
                                name,
                                iter::once(Definition::new_variable(node.universe, type_of)),
                            )
                        };
                        types.insert(node.id(), type_of);
                        dispatch.insert(handle, d);
                    }
                }
                Kind::Array => todo!(),
                Kind::Index => todo!(),
                Kind::Dotted => todo!(),
                Kind::With => todo!(),
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
