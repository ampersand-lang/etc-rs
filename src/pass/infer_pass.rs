use std::iter;

use failure::Fallible;
use hashbrown::HashMap;
use smallvec::smallvec;

use crate::assets::{Handle, LazyUpdate, Resources};
use crate::ast::{Kind, Node, RootNode, Visit};
use crate::dispatch::*;
use crate::scope::Scope;
use crate::types::{primitive, NamedType, Type, TypeGroup, TypeId, TypeOrPlaceholder};
use crate::values::Value;

pub fn infer_update(
    lazy: &mut LazyUpdate,
    roots: Resources<&RootNode>,
    payloads: Resources<&Value>,
    scopes: Resources<&Scope>,
    mut dispatch: Resources<&mut Dispatcher>,
    named_types: Resources<&NamedType>,
    strings: Resources<&String>,
    mut nodes: Resources<&mut Node>,
) -> Fallible<()> {
    for (_, root_node) in roots.iter::<RootNode>() {
        let root = nodes.get::<Node>(root_node.0).unwrap();
        let mut types = HashMap::new();
        root.visit(Visit::Postorder, &nodes, |_, node| {
            if let Some(node) = node {
                match node.kind {
                    Kind::Nil => {
                        let typ = match &*payloads.get(node.payload.unwrap()).unwrap() {
                            Value::Unit => primitive::UNIT.clone(),
                            Value::Int(_) => primitive::SINT.clone(),
                            Value::Float(_) => primitive::FLOAT.clone(),
                            Value::String(_) => todo!(),
                            Value::Identifier(string) => {
                                let handle = Handle::new();
                                lazy.insert(handle, string.clone());
                                TypeId {
                                    group: TypeGroup::None,
                                    concrete: TypeOrPlaceholder::Dispatch(
                                        node.scope.unwrap(),
                                        handle,
                                    ),
                                }
                            }
                            Value::Type(_) => primitive::TYPE.clone(),
                            Value::Function(_) => todo!(),
                        };
                        types.insert(node.id(), typ);
                    }
                    Kind::Block => {
                        let typ = node.children.last().and_then(|last| {
                            last.and_then(|last| nodes.get::<Node>(last).unwrap().type_of.clone())
                        });
                        if let Some(typ) = typ {
                            types.insert(node.id(), typ);
                        }
                    }
                    Kind::Function => {
                        let param_tuple = &*nodes.get::<Node>(node.children[0].unwrap()).unwrap();
                        let param_types = match param_tuple.kind {
                            Kind::Nil => {
                                match &*payloads.get::<Value>(param_tuple.payload.unwrap()).unwrap()
                                {
                                    Value::Identifier(_) => smallvec![TypeId::new_placeholder()],
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
                                                match &*payloads
                                                    .get::<Value>(param.payload.unwrap())
                                                    .unwrap()
                                                {
                                                    Value::Identifier(_) => {
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
                            // TODO: error
                            _ => todo!(),
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
                                        // TODO: error
                                        _ => todo!(),
                                    }
                                }
                                TypeOrPlaceholder::Dispatch(scope, handle) => {
                                    let name = Name(
                                        scope,
                                        strings
                                            .get::<String>(handle)
                                            .unwrap()
                                            .as_ref()
                                            .clone()
                                            .into(),
                                    );
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
                                    let dispatch = dispatch.get::<Dispatcher>(
                                        DispatchId::from_name(name.0, &name.1.as_ref()),
                                    );
                                    if let Some(dispatch) = dispatch {
                                        let query =
                                            Query::new(name, IsFunction::Yes, Some(args), None);
                                        let results = dispatch.query(&query);
                                        match results.len() {
                                            // TODO: traverse parents
                                            0 => todo!(),
                                            1 => Some(results[0].result_type()),
                                            // TODO: error
                                            _ => todo!(),
                                        }
                                    } else {
                                        panic!("error");
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
                                match &*payloads.get::<Value>(ident.payload.unwrap()).unwrap() {
                                    Value::Identifier(ident) => ident.clone(),
                                    // TODO: other bindings
                                    _ => todo!(),
                                }
                            }
                            // TODO: other bindings
                            _ => todo!(),
                        };
                        let scope = node.scope.unwrap();
                        let handle = DispatchId::from_name(scope, &ident);
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
                            let name = Name(scope, ident.into());
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
                }
            }
        });
        for (handle, typ) in types {
            nodes.get_mut::<Node>(handle).unwrap().type_of = Some(typ);
        }
    }
    Ok(())
}
