use std::iter;

use failure::{Fail, Fallible};
use hashbrown::HashMap;
use smallvec::smallvec;

use crate::error::MultiError;
use crate::assets::{Handle, LazyUpdate, Resources};
use crate::ast::{Kind, Node, RootNode, Visit};
use crate::dispatch::*;
use crate::scope::Scope;
use crate::types::{primitive, NamedType, Type, TypeGroup, TypeId, TypeOrPlaceholder};
use crate::values::Payload;
use crate::lexer::Location;

#[derive(Debug, Fail)]
pub enum InferError {
    #[fail(display = "type error at {}: expected {:?}, but got {:?}", location, expected, got)]
    TypeError {
        location: Location,
        expected: TypeId,
        got: TypeId,
    },
    #[fail(display = "type at {} is not a function: {:?}", location, typ)]
    NotAFunction {
        location: Location,
        typ: TypeId,
    },
    #[fail(display = "no matching definition found at {}", location)]
    NoDefinitions {
        location: Location,
    },
    #[fail(display = "multiple matching definition found at {}", location)]
    MultipleDefinitions {
        location: Location,
    },
}

pub fn infer_update(
    lazy: &mut LazyUpdate,
    roots: Resources<&RootNode>,
    _scopes: Resources<&Scope>,
    mut dispatch: Resources<&mut Dispatcher>,
    named_types: Resources<&NamedType>,
    locations: Resources<&Location>,
    mut nodes: Resources<&mut Node>,
) -> Fallible<()> {
    let mut errors = Vec::new();
    for (_, root_node) in roots.iter::<RootNode>() {
        let root = nodes.get::<Node>(root_node.0).unwrap();
        let mut types = HashMap::new();
        root.visit(Visit::Postorder, &nodes, |_, node| {
            if let Some(node) = node {
                match node.kind {
                    Kind::Nil => {
                        let typ = match node.payload.unwrap() {
                            Payload::Unit => primitive::UNIT.clone(),
                            Payload::Integer(_) => primitive::SINT.clone(),
                            Payload::Float(_) => primitive::FLOAT.clone(),
                            Payload::String(_) => todo!(),
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
                                    .or_else(|| Some(types[&last]))
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
                                                    Payload::Identifier(_) => {
                                                        TypeId::new_placeholder()
                                                    }
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
                        if let Some(typ) = func.type_of {
                            let result_type = match typ.concrete {
                                TypeOrPlaceholder::Type(handle) => {
                                    match &named_types.get::<NamedType>(handle).unwrap().t {
                                        Type::Function { result_type, .. } => {
                                            Some(result_type.clone())
                                        }
                                        _ => {
                                            errors.push(From::from(InferError::NotAFunction {
                                                location: locations.get::<Location>(func.location).unwrap().as_ref().clone(),
                                                typ,
                                            }));
                                            return;
                                        }
                                    }
                                }
                                TypeOrPlaceholder::Dispatch(scope, handle) => {
                                    let name = Name(scope, handle);
                                    let args = node.children[1..]
                                        .iter()
                                        .map(|param| {
                                            nodes
                                                .get::<Node>(param.unwrap())
                                                .unwrap()
                                                .type_of
                                                .unwrap()
                                        })
                                        .collect();
                                    let dispatch =
                                        dispatch.get::<Dispatcher>(DispatchId::from_name(
                                            name.0,
                                            &name.1.as_u128().to_le_bytes(),
                                        )).unwrap();
                                    let query =
                                        Query::new(name, IsFunction::Yes, Some(args), None);
                                    let results = dispatch.query(&query, &named_types);
                                    match results.len() {
                                        // TODO: traverse parents
                                        0 => todo!(),
                                        1 => Some(results[0].result_type()),
                                        _ => {
                                            errors.push(From::from(InferError::MultipleDefinitions {
                                                location: locations.get::<Location>(func.location).unwrap().as_ref().clone(),
                                            }));
                                            return;
                                        }
                                    }
                                }
                                _ => todo!(),
                            };
                            if let Some(typ) = result_type {
                                types.insert(node.id(), typ);
                            }
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
                        let scope = node.scope.unwrap();
                        let handle = DispatchId::from_name(scope, &ident.as_u128().to_le_bytes());
                        let binding = nodes
                            .get::<Node>(node.children[2].unwrap())
                            .unwrap()
                            .type_of
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
                                d.push(Definition::new_function(param_types, result_type));
                            } else {
                                d.push(Definition::new_variable(result_type));
                            }
                            d
                        } else {
                            let name = Name(scope, ident);
                            if let Some(param_types) = param_types {
                                Dispatcher::with_definitions(
                                    name,
                                    iter::once(Definition::new_function(param_types, result_type)),
                                )
                            } else {
                                Dispatcher::with_definitions(
                                    name,
                                    iter::once(Definition::new_variable(result_type)),
                                )
                            }
                        };
                        lazy.insert(handle, d);
                    }
                    Kind::Tuple => {
                        let fields = node
                            .children
                            .iter()
                            .map(|param| {
                                nodes.get::<Node>(param.unwrap()).unwrap().type_of.unwrap()
                            })
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
                            group: TypeGroup::Function,
                            concrete: TypeOrPlaceholder::Type(named),
                        };
                        types.insert(node.id(), typ);
                    }
                    Kind::Declaration => todo!(),
                    Kind::Array => todo!(),
                    Kind::Index => todo!(),
                    Kind::Dotted => todo!(),
                }
            }
        });
        for (handle, typ) in types {
            nodes.get_mut::<Node>(handle).unwrap().type_of = Some(typ);
        }
    }
    if errors.is_empty() {
        Ok(())
    } else {
        Err(From::from(MultiError::from(errors)))
    }
}
