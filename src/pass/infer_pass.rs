use std::iter;

use failure::{Error, Fail, Fallible};
use hashbrown::HashMap;

use crate::assets::{Handle, LazyUpdate, Resources, Static};
use crate::ast::{Kind, Node, NodeId, RootNode};
use crate::builder::BuilderMacro;
use crate::dispatch::*;
use crate::error::MultiError;
use crate::lexer::Location;
use crate::lir::{context::ExecutionContext, target::Target};
use crate::scope::Scope;
use crate::types::{
    primitive, Assoc, AssocOptional, NamedType, NonConcrete, Type, TypeGroup, TypeId,
};
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

pub fn infer(
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
    errors: &mut Vec<Error>,
) -> Fallible<()> {
    let mut err = None;
    match node.kind {
        Kind::Nil => {
            let typ = match node.payload.unwrap() {
                Payload::Unit => primitive::UNIT.clone(),
                Payload::Bool(_) => primitive::BOOL.clone(),
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
                Payload::Identifier(_string) if node.alternative => {
                    // placeholder
                    *primitive::UNIT_BUILDER
                }
                Payload::Identifier(string) => {
                    if let Some(id) = node.definition {
                        infer_recursively(
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
                            errors,
                        );
                    }
                    TypeId {
                        group: TypeGroup::None,
                        concrete: NonConcrete::Dispatch(node.scope.unwrap(), string),
                    }
                }
                Payload::Type(_) => primitive::TYPE.clone(),
                Payload::Function(id) => {
                    let ctx = contexts.get(root.thread.unwrap()).unwrap();
                    let funk = ctx.function((id.offset + id.idx) as usize);
                    let mut param_types = Assoc::new();
                    for (i, t) in funk.param_types().iter().enumerate() {
                        param_types.insert(format!("{}", i), *t);
                    }
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
                Payload::Struct
                | Payload::Enum
                | Payload::Union
                | Payload::Tagged
                | Payload::Class => *primitive::METATYPE,
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
                                let name = param.children[0].unwrap();
                                let name = nodes.get(name).unwrap();
                                let ident = match name.payload.unwrap() {
                                    Payload::Identifier(ident) => ident,
                                    _ => todo!(),
                                };
                                let ident = strings.get(ident).unwrap();
                                let ident = ident.to_string();
                                let param =
                                    param.type_of.or_else(|| types.get(&param.id()).copied());
                                (ident, param)
                            }
                            // other patterns
                            _ => todo!(),
                        }
                    });
                    let mut params = Assoc::new();
                    for (name, param) in iter {
                        match param {
                            Some(param) => params.insert(name, param),
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
            let typ = if func.alternative {
                match func.payload {
                    Some(Payload::Identifier(ident)) => {
                        let alternative = strings.get(ident).unwrap();
                        let handle = Handle::from_hash(alternative.as_bytes());
                        if let Some(builder) = builders.get::<BuilderMacro>(handle) {
                            builder
                                .infer(
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
                                .transpose()
                                .unwrap_or_else(|err| {
                                    panic!("i don't know how to handle errors yet: {}", err)
                                })
                        } else {
                            panic!("not an alternative: `${}`", alternative.as_str());
                        }
                    }
                    Some(Payload::Struct) => {
                        let mut fields = Assoc::new();
                        if let Some(structure) = node.children[1] {
                            let structure = nodes.get(structure).unwrap();
                            for &field in &structure.children {
                                if let Some(field) = field {
                                    let field = nodes.get(field).unwrap();
                                    let name = field.children[0].unwrap();
                                    let name = nodes.get(name).unwrap();
                                    let name = match name.payload.unwrap() {
                                        Payload::Identifier(ident) => {
                                            strings.get(ident).unwrap().to_string()
                                        }
                                        _ => todo!(),
                                    };
                                    let typ = *types.get(&field.id()).unwrap();
                                    fields.insert(name, typ);
                                }
                            }
                        }
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

                        let cons = Handle::new();
                        named_types.insert(
                            cons,
                            NamedType {
                                name: None,
                                t: Type::Constructor(typ),
                            },
                        );
                        let cons = TypeId {
                            group: TypeGroup::Constructor,
                            concrete: NonConcrete::Type(cons),
                        };

                        types.insert(node.id(), cons);
                        return Ok(());
                    }
                    Some(Payload::Enum)
                    | Some(Payload::Union)
                    | Some(Payload::Tagged)
                    | Some(Payload::Class) => todo!(),
                    _ => None,
                }
            } else {
                None
            };
            if let Some(typ) = typ {
                types.insert(node.id(), typ);
            } else {
                let typ = types
                    .get(&func.id())
                    .copied()
                    .or_else(|| func.unquote_type)
                    .or_else(|| func.type_of);
                let args = node.children[1..].to_vec();
                let fn_types = typ.and_then(|typ| match typ.concrete {
                    NonConcrete::Type(handle) => {
                        match &named_types.get::<NamedType>(handle).unwrap().t {
                            Type::Constructor(_) => Some((typ, None)),
                            Type::Function {
                                result_type,
                                param_types,
                            } => Some((result_type.clone(), Some(param_types.clone()))),
                            _ => {
                                err = Some(From::from(InferError::NotAFunction {
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
                        let args: AssocOptional<_> = args
                            .iter()
                            .map(|param| {
                                let id = param.unwrap();
                                let t = nodes
                                    .get::<Node>(id)
                                    .unwrap()
                                    .type_of
                                    .or_else(|| types.get(&id).copied())
                                    .unwrap();
                                let name: Option<String> = None;
                                // TODO: named args
                                (name, t)
                            })
                            .collect();
                        let ident = strings.get(handle).unwrap();
                        let mut iter = Some(scope);
                        let mut result = None;
                        while let Some(scope) = iter {
                            let name = Name(scope, handle);
                            let dispatch = dispatch
                                .get::<Dispatcher>(DispatchId::from_name(name.0, ident.as_bytes()));
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
                            let query = Query::new(name, IsFunction::Yes, Some(args.clone()), None);
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
                                        results[0].arg_types().cloned(),
                                    ));
                                    break;
                                }
                                _ => {
                                    err = Some(From::from(InferError::MultipleDefinitions {
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
                if let Some((result_type, Some(param_types))) = fn_types {
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
                } else if let Some((typ, None)) = fn_types {
                    match typ.concrete {
                        NonConcrete::Type(handle) => {
                            match named_types.get::<NamedType>(handle).unwrap().t {
                                Type::Constructor(t) => {
                                    types.insert(func.id(), typ);
                                    types.insert(node.id(), t);
                                }
                                _ => todo!(),
                            }
                        }
                        NonConcrete::Dispatch(scope, handle) => {
                            let args: AssocOptional<_> = args
                                .iter()
                                .map(|param| {
                                    let id = param.unwrap();
                                    let t = nodes
                                        .get::<Node>(id)
                                        .unwrap()
                                        .type_of
                                        .or_else(|| types.get(&id).copied())
                                        .unwrap();
                                    let name: Option<String> = None;
                                    // TODO: named args
                                    (name, t)
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
                                        result = Some(results[0].result_type());
                                        break;
                                    }
                                    _ => {
                                        err = Some(From::from(InferError::MultipleDefinitions {
                                            location: locations
                                                .get::<Location>(func.location)
                                                .unwrap()
                                                .as_ref()
                                                .clone(),
                                        }));
                                        result = None;
                                        break;
                                    }
                                }
                            }
                            if let Some(t) = result {
                                match t.concrete {
                                    NonConcrete::Type(t) => {
                                        let t = named_types.get(t).unwrap();
                                        match t.t {
                                            Type::Constructor(t) => {
                                                types.insert(node.id(), t);
                                            }
                                            _ => todo!(),
                                        }
                                    }
                                    _ => todo!(),
                                }
                                types.insert(func.id(), t);
                            } else {
                                err = Some(From::from(InferError::NoDefinitions {
                                    location: locations
                                        .get::<Location>(func.location)
                                        .unwrap()
                                        .as_ref()
                                        .clone(),
                                }));
                            }
                        }
                        _ => todo!(),
                    }
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
                let (result_type, param_types, cons) = match binding.concrete {
                    NonConcrete::Type(t) => match named_types.get::<NamedType>(t).unwrap().t {
                        Type::Function {
                            result_type,
                            ref param_types,
                        } => (result_type, Some(param_types.clone()), false),
                        Type::Constructor(_) => (binding, None, true),
                        _ => (binding, None, false),
                    },
                    _ => (binding, None, false),
                };
                let d = if let Some(mut d) = dispatch.remove(handle) {
                    if let Some(param_types) = param_types {
                        d.push(Definition::new_function(
                            node.universe.value(),
                            param_types,
                            result_type,
                        ));
                    } else if cons {
                        d.push(Definition::new_constructor(
                            node.universe.value(),
                            result_type,
                        ));
                    } else {
                        d.push(Definition::new_variable(node.universe.value(), result_type));
                    }
                    d
                } else {
                    let name = Name(scope, ident);
                    if let Some(param_types) = param_types {
                        Dispatcher::with_definitions(
                            name,
                            iter::once(Definition::new_function(
                                node.universe.value(),
                                param_types,
                                result_type,
                            )),
                        )
                    } else if cons {
                        Dispatcher::with_definitions(
                            name,
                            iter::once(Definition::new_constructor(
                                node.universe.value(),
                                result_type,
                            )),
                        )
                    } else {
                        Dispatcher::with_definitions(
                            name,
                            iter::once(Definition::new_variable(
                                node.universe.value(),
                                result_type,
                            )),
                        )
                    }
                };
                types.insert(node.id(), binding);
                dispatch.insert(handle, d);
            }
        }
        Kind::Assign => {
            let value = nodes.get::<Node>(node.children[1].unwrap()).unwrap();
            let value = types
                .get(&value.id())
                .copied()
                .or_else(|| value.unquote_type)
                .or_else(|| value.type_of);
            if let Some(value) = value {
                types.insert(node.id(), value);
            }
        }
        Kind::Tuple => {
            if node.alternative {
                types.insert(node.id(), *primitive::TYPE);
                return Ok(());
            }
            let iter = node.children.iter().map(|param| {
                nodes
                    .get::<Node>(param.unwrap())
                    .unwrap()
                    .type_of
                    .or_else(|| types.get(&param.unwrap()).copied())
            });
            let mut none = false;
            let mut fields = Assoc::new();
            for (i, field) in iter.enumerate() {
                match field {
                    Some(field) => fields.insert(format!("{}", i), field),
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
        Kind::Argument => {
            let value = node.children[1].unwrap();
            let typ = types.get(&value);
            if let Some(&typ) = typ {
                types.insert(node.id(), typ);
            }
        }
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
                    d.push(Definition::new_variable(node.universe.value(), type_of));
                    d
                } else {
                    let name = Name(scope, ident);
                    Dispatcher::with_definitions(
                        name,
                        iter::once(Definition::new_variable(node.universe.value(), type_of)),
                    )
                };
                types.insert(node.id(), type_of);
                dispatch.insert(handle, d);
            }
        }
        Kind::Dotted => {
            let ident = nodes.get::<Node>(node.children[1].unwrap()).unwrap();
            let ident = match ident.kind {
                Kind::Nil => match ident.payload.unwrap() {
                    Payload::Identifier(ident) => ident,
                    _ => todo!(),
                },
                _ => todo!(),
            };
            let name = strings.get(ident).unwrap();

            let value = node.children[0].unwrap();
            let typ = types.get(&value);
            if let Some(&typ) = typ {
                let field = match typ.concrete {
                    NonConcrete::Type(t) => match named_types.get::<NamedType>(t).unwrap().t {
                        Type::Struct { ref fields } => Some(*fields.get(name.as_str()).unwrap()),
                        _ => todo!(),
                    },
                    NonConcrete::Dispatch(scope, handle) => {
                        let ident = strings.get(handle).unwrap();
                        let mut iter = Some(scope);
                        let mut result = None;
                        while let Some(scope) = iter {
                            let name = Name(scope, handle);
                            let dispatch = dispatch
                                .get::<Dispatcher>(DispatchId::from_name(name.0, ident.as_bytes()));
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
                            let query = Query::new(name, IsFunction::Maybe, None, None);
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
                                    result = Some(results[0].result_type());
                                    break;
                                }
                                _ => {
                                    err = Some(From::from(InferError::MultipleDefinitions {
                                        location: locations
                                            .get::<Location>(node.location)
                                            .unwrap()
                                            .as_ref()
                                            .clone(),
                                    }));
                                    result = None;
                                    break;
                                }
                            }
                        }
                        if let Some(t) = result {
                            match t.concrete {
                                NonConcrete::Type(t) => {
                                    match named_types.get::<NamedType>(t).unwrap().t {
                                        Type::Struct { ref fields } => {
                                            Some(*fields.get(name.as_str()).unwrap())
                                        }
                                        _ => todo!(),
                                    }
                                }
                                _ => todo!(),
                            }
                        } else {
                            err = Some(From::from(InferError::NoDefinitions {
                                location: locations
                                    .get::<Location>(node.location)
                                    .unwrap()
                                    .as_ref()
                                    .clone(),
                            }));
                            None
                        }
                    }
                    _ => todo!(),
                };

                if let Some(field) = field {
                    types.insert(node.id(), field);
                }
            }
        }
        Kind::Array => todo!(),
        Kind::Index => todo!(),
        Kind::With => todo!(),
    }
    if let Some(e) = err {
        Err(e)
    } else {
        Ok(())
    }
}

pub fn infer_recursively(
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
    errors: &mut Vec<Error>,
) {
    if types.contains_key(&node.id()) {
        return;
    }

    match node.kind {
        Kind::Application => {
            let func = nodes.get::<Node>(node.children[0].unwrap()).unwrap();
            match func.payload {
                Some(Payload::Identifier(string)) if func.alternative => {
                    match strings.get::<String>(string).unwrap().as_str() {
                        "quasiquote" => return,
                        _ => {}
                    }
                }
                _ => {}
            }
        }
        _ => {}
    }

    match node.kind {
        Kind::Nil => {}
        Kind::Application => {
            let func = node.children[0].unwrap();
            let func = nodes.get(func).unwrap();
            let mut was_struct = false;
            if func.alternative {
                match func.payload.unwrap() {
                    Payload::Struct => {
                        was_struct = true;
                        let body = node.children[1].unwrap();
                        let body = nodes.get(body).unwrap();
                        for &decl in &body.children {
                            let decl = nodes.get(decl.unwrap()).unwrap();
                            let child = decl.children[1];
                            if let Some(child) = child {
                                let node = nodes.get(child).unwrap();
                                infer_recursively(
                                    root,
                                    &node,
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
                                    errors,
                                );
                            }
                            let type_of = nodes
                                .get::<Node>(decl.children[1].unwrap())
                                .unwrap()
                                .payload;
                            if let Some(type_of) = type_of {
                                let type_of = match type_of {
                                    Payload::Type(t) => t,
                                    Payload::Identifier(string) => TypeId {
                                        group: TypeGroup::None,
                                        concrete: NonConcrete::Dispatch(
                                            node.scope.unwrap(),
                                            string,
                                        ),
                                    },
                                    _ => todo!(),
                                };
                                types.insert(decl.id(), type_of);
                            }
                        }
                    }
                    _ => {}
                }
            }

            if !was_struct {
                for &child in &node.children {
                    if let Some(child) = child {
                        let node = nodes.get(child).unwrap();
                        infer_recursively(
                            root,
                            &node,
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
                            errors,
                        );
                    }
                }
            }
        }
        Kind::Block
        | Kind::Tuple
        | Kind::Index
        | Kind::Array
        | Kind::With
        | Kind::Assign
        | Kind::Function => {
            for &child in &node.children {
                if let Some(child) = child {
                    let node = nodes.get(child).unwrap();
                    infer_recursively(
                        root,
                        &node,
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
                        errors,
                    );
                }
            }
        }
        Kind::Declaration | Kind::Argument | Kind::Global | Kind::Binding => {
            for &child in &node.children[1..] {
                if let Some(child) = child {
                    let node = nodes.get(child).unwrap();
                    infer_recursively(
                        root,
                        &node,
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
                        errors,
                    );
                }
            }
        }
        Kind::Dotted => {
            for &child in &node.children[..1] {
                if let Some(child) = child {
                    let node = nodes.get(child).unwrap();
                    infer_recursively(
                        root,
                        &node,
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
                        errors,
                    );
                }
            }
        }
    }

    let result = infer(
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
        errors,
    );

    if let Err(e) = result {
        errors.push(e);
    }
}

pub fn infer_update(
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

        let mut types = HashMap::new();

        infer_recursively(
            &root,
            &root,
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
            &mut errors,
        );

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
