use either::Either;
use failure::Fallible;
use hashbrown::HashMap;
use smallvec::SmallVec;

use crate::assets::{Handle, Resources};
use crate::ast::{Node, NodeId};
use crate::dispatch::Dispatcher;
use crate::lir::{
    builder::{FunctionBuilder, ValueBuilder},
    compile::Compile as _,
    foreign,
    target::Target,
    Bytes, Elems, TypedValue, Value, Variants,
};
use crate::pass::collapse;
use crate::scope::Scope;
use crate::types::{primitive, NamedType, NonConcrete, TypeId};
use crate::values::Payload;

pub type Infer = Box<
    dyn Fn(
            &Node,
            &Resources<&mut Node>,
            &mut Resources<&mut NamedType>,
            &Resources<&String>,
            &Resources<&Scope>,
            &Resources<&mut Dispatcher>,
            &mut HashMap<NodeId, TypeId>,
            &Target,
        ) -> Fallible<TypeId>
        + Send
        + Sync
        + 'static,
>;

pub type Compile = Box<
    dyn for<'a> Fn(
            &Node,
            &mut Resources<(
                &mut Node,
                &Scope,
                &Payload,
                &String,
                &mut TypedValue,
                &mut Elems,
                &mut Variants,
                &mut Bytes,
            )>,
            &Resources<&BuilderMacro>,
            ValueBuilder<'a>,
        ) -> Fallible<(TypedValue, FunctionBuilder<'a>)>
        + Send
        + Sync
        + 'static,
>;

pub struct BuilderMacro {
    name: &'static str,
    infer: Option<Infer>,
    compile: Option<Compile>,
    require_intrinsic: bool,
    require_runtime: bool,
}

impl BuilderMacro {
    pub fn new(name: &'static str) -> Self {
        Self {
            name,
            infer: None,
            compile: None,
            require_intrinsic: false,
            require_runtime: false,
        }
    }

    pub fn id(&self) -> Handle<BuilderMacro> {
        Handle::from_hash(self.name.as_bytes())
    }

    pub fn infer(
        &self,
        node: &Node,
        nodes: &Resources<&mut Node>,
        named_types: &mut Resources<&mut NamedType>,
        strings: &Resources<&String>,
        scopes: &Resources<&Scope>,
        dispatch: &Resources<&mut Dispatcher>,
        types: &mut HashMap<NodeId, TypeId>,
        target: &Target,
    ) -> Option<Fallible<TypeId>> {
        self.infer.as_ref().map(|f| {
            f(
                node,
                nodes,
                named_types,
                strings,
                scopes,
                dispatch,
                types,
                target,
            )
        })
    }

    pub fn compile<'a>(
        &self,
        node: &Node,
        res: &mut Resources<(
            &mut Node,
            &Scope,
            &Payload,
            &String,
            &mut TypedValue,
            &mut Elems,
            &mut Variants,
            &mut Bytes,
        )>,
        builders: &Resources<&BuilderMacro>,
        builder: ValueBuilder<'a>,
    ) -> Either<Fallible<(TypedValue, FunctionBuilder<'a>)>, ValueBuilder<'a>> {
        match &self.compile {
            Some(f) => Either::Left(f(node, res, builders, builder)),
            None => Either::Right(builder),
        }
    }

    pub fn with_infer(mut self, infer: Infer) -> Self {
        self.infer = Some(infer);
        self
    }

    pub fn with_compile(mut self, compile: Compile) -> Self {
        self.compile = Some(compile);
        self
    }

    pub fn require_intrinsic(mut self, require: bool) -> Self {
        self.require_intrinsic = require;
        self
    }

    pub fn require_runtime(mut self, require: bool) -> Self {
        self.require_runtime = require;
        self
    }

    pub fn build_new_node() -> Self {
        Self::new("new-node")
            .with_infer(Box::new(
                |_node, _nodes, _named_types, _string, _scopes, _dispatch, _types, _target| {
                    Ok(*primitive::NODE)
                },
            ))
            .with_compile(Box::new(|node, res, builders, mut builder| {
                let mut args = SmallVec::new();
                for expr in &node.children[1..] {
                    if let Some(expr) = expr {
                        let (v, f) = Node::compile(*expr, res, builders, builder)?;
                        builder = ValueBuilder(f);
                        args.push(v);
                    }
                }
                let mut result = TypedValue::new(*primitive::UNIT, Value::Unit);
                builder.0 = builder.0.build_newnode(&mut result, args);
                Ok((result, builder.0))
            }))
    }

    pub fn build_format_ast() -> Self {
        Self::new("format-ast")
            .with_infer(Box::new(
                |_node, _nodes, _named_types, _string, _scopes, _dispatch, _types, _target| {
                    Ok(*primitive::NODE)
                },
            ))
            .with_compile(Box::new(|node, res, builders, mut builder| {
                let mut args = SmallVec::new();
                let func =
                    TypedValue::new(*primitive::NODE_BUILDER, Value::Ffi(*foreign::FORMAT_AST));
                args.push(func);
                for expr in &node.children[1..] {
                    if let Some(expr) = expr {
                        let (v, f) = Node::compile(*expr, res, builders, builder)?;
                        builder = ValueBuilder(f);
                        args.push(v);
                    }
                }
                let mut result = TypedValue::new(*primitive::UNIT, Value::Unit);
                builder.0 = builder.0.build_call(&mut result, args);
                Ok((result, builder.0))
            }))
    }

    pub fn build_quasiquote() -> Self {
        Self::new("quasiquote")
            .with_infer(Box::new(
                |_node, _nodes, _named_types, _string, _scopes, _dispatch, _types, _target| {
                    Ok(*primitive::NODE)
                },
            ))
            .with_compile(Box::new(|node, _res, _builders, builder| {
                let node =
                    TypedValue::new(*primitive::NODE, Value::Node(node.children[1].unwrap()));
                Ok((node, builder.0))
            }))
    }

    pub fn build_compile() -> Self {
        Self::new("compile")
            .with_infer(Box::new(
                |_node, _nodes, _named_types, _string, _scopes, _dispatch, _types, _target| {
                    Ok(*primitive::UNIT)
                },
            ))
            .with_compile(Box::new(|node, res, builders, mut builder| {
                let mut args = SmallVec::new();
                let func = TypedValue::new(*primitive::UNIT_BUILDER, Value::Ffi(*foreign::COMPILE));
                args.push(func);
                for expr in &node.children[1..] {
                    if let Some(expr) = expr {
                        let (v, f) = Node::compile(*expr, res, builders, builder)?;
                        builder = ValueBuilder(f);
                        args.push(v);
                    }
                }
                let mut result = TypedValue::new(*primitive::UNIT, Value::Unit);
                builder.0 = builder.0.build_call(&mut result, args);
                Ok((result, builder.0))
            }))
    }

    pub fn build_builtin_add() -> Self {
        Self::new("+")
            .with_infer(Box::new(
                |node, nodes, named_types, strings, scopes, dispatch, types, target| {
                    if node.children.len() > 3 {
                        todo!();
                    }
                    let arg0 = nodes.get(node.children[1].unwrap()).unwrap();
                    let arg1 = nodes.get(node.children[2].unwrap()).unwrap();
                    let (t, _) = collapse(
                        types[&arg0.id()],
                        scopes,
                        strings,
                        dispatch,
                        named_types,
                        nodes,
                    )?;
                    let (u, _) = collapse(
                        types[&arg1.id()],
                        scopes,
                        strings,
                        dispatch,
                        named_types,
                        nodes,
                    )?;
                    match (t.concrete, u.concrete) {
                        (NonConcrete::Type(t), NonConcrete::Type(u)) => {
                            let t = &named_types.get(t).unwrap().t;
                            let u = &named_types.get(u).unwrap().t;
                            if !(t.is_signed() && u.is_signed()
                                || t.is_unsigned() && u.is_unsigned())
                            {
                                todo!()
                            }
                        }
                        _ => todo!(),
                    }
                    if t.size_of(named_types, target).unwrap()
                        > u.size_of(named_types, target).unwrap()
                    {
                        Ok(t)
                    } else {
                        Ok(u)
                    }
                },
            ))
            .with_compile(Box::new(|node, res, builders, mut builder| {
                let mut args = SmallVec::<[_; 2]>::new();
                for expr in &node.children[1..] {
                    if let Some(expr) = expr {
                        let (v, f) = Node::compile(*expr, res, builders, builder)?;
                        builder = ValueBuilder(f);
                        args.push(v);
                    }
                }
                let mut result = TypedValue::new(*primitive::UNIT, Value::Unit);
                builder.0 =
                    builder
                        .0
                        .build_add(&mut result, node.type_of.unwrap(), args[0], args[1]);
                Ok((result, builder.0))
            }))
    }

    pub fn build_builtin_sub() -> Self {
        Self::new("-")
            .with_infer(Box::new(
                |node, nodes, named_types, strings, scopes, dispatch, types, target| {
                    if node.children.len() > 3 {
                        todo!();
                    }
                    let arg0 = nodes.get(node.children[1].unwrap()).unwrap();
                    let arg1 = nodes.get(node.children[2].unwrap()).unwrap();
                    let (t, _) = collapse(
                        types[&arg0.id()],
                        scopes,
                        strings,
                        dispatch,
                        named_types,
                        nodes,
                    )?;
                    let (u, _) = collapse(
                        types[&arg1.id()],
                        scopes,
                        strings,
                        dispatch,
                        named_types,
                        nodes,
                    )?;
                    match (t.concrete, u.concrete) {
                        (NonConcrete::Type(t), NonConcrete::Type(u)) => {
                            let t = &named_types.get(t).unwrap().t;
                            let u = &named_types.get(u).unwrap().t;
                            if !(t.is_signed() && u.is_signed()
                                || t.is_unsigned() && u.is_unsigned())
                            {
                                todo!()
                            }
                        }
                        _ => todo!(),
                    }
                    if t.size_of(named_types, target).unwrap()
                        > u.size_of(named_types, target).unwrap()
                    {
                        Ok(t)
                    } else {
                        Ok(u)
                    }
                },
            ))
            .with_compile(Box::new(|node, res, builders, mut builder| {
                let mut args = SmallVec::<[_; 2]>::new();
                for expr in &node.children[1..] {
                    if let Some(expr) = expr {
                        let (v, f) = Node::compile(*expr, res, builders, builder)?;
                        builder = ValueBuilder(f);
                        args.push(v);
                    }
                }
                let mut result = TypedValue::new(*primitive::UNIT, Value::Unit);
                builder.0 =
                    builder
                        .0
                        .build_sub(&mut result, node.type_of.unwrap(), args[0], args[1]);
                Ok((result, builder.0))
            }))
    }

    pub fn build_builtin_mul() -> Self {
        Self::new("*")
            .with_infer(Box::new(
                |node, nodes, named_types, strings, scopes, dispatch, types, target| {
                    if node.children.len() > 3 {
                        todo!();
                    }
                    let arg0 = nodes.get(node.children[1].unwrap()).unwrap();
                    let arg1 = nodes.get(node.children[2].unwrap()).unwrap();
                    let (t, _) = collapse(
                        types[&arg0.id()],
                        scopes,
                        strings,
                        dispatch,
                        named_types,
                        nodes,
                    )?;
                    let (u, _) = collapse(
                        types[&arg1.id()],
                        scopes,
                        strings,
                        dispatch,
                        named_types,
                        nodes,
                    )?;
                    match (t.concrete, u.concrete) {
                        (NonConcrete::Type(t), NonConcrete::Type(u)) => {
                            let t = &named_types.get(t).unwrap().t;
                            let u = &named_types.get(u).unwrap().t;
                            if !(t.is_signed() && u.is_signed()
                                || t.is_unsigned() && u.is_unsigned())
                            {
                                todo!()
                            }
                        }
                        _ => todo!(),
                    }
                    if t.size_of(named_types, target).unwrap()
                        > u.size_of(named_types, target).unwrap()
                    {
                        Ok(t)
                    } else {
                        Ok(u)
                    }
                },
            ))
            .with_compile(Box::new(|node, res, builders, mut builder| {
                let mut args = SmallVec::<[_; 2]>::new();
                for expr in &node.children[1..] {
                    if let Some(expr) = expr {
                        let (v, f) = Node::compile(*expr, res, builders, builder)?;
                        builder = ValueBuilder(f);
                        args.push(v);
                    }
                }
                let mut result = TypedValue::new(*primitive::UNIT, Value::Unit);
                builder.0 =
                    builder
                        .0
                        .build_mul(&mut result, node.type_of.unwrap(), args[0], args[1]);
                Ok((result, builder.0))
            }))
    }

    pub fn build_builtin_div() -> Self {
        Self::new("/")
            .with_infer(Box::new(
                |node, nodes, named_types, strings, scopes, dispatch, types, target| {
                    if node.children.len() > 3 {
                        todo!();
                    }
                    let arg0 = nodes.get(node.children[1].unwrap()).unwrap();
                    let arg1 = nodes.get(node.children[2].unwrap()).unwrap();
                    let (t, _) = collapse(
                        types[&arg0.id()],
                        scopes,
                        strings,
                        dispatch,
                        named_types,
                        nodes,
                    )?;
                    let (u, _) = collapse(
                        types[&arg1.id()],
                        scopes,
                        strings,
                        dispatch,
                        named_types,
                        nodes,
                    )?;
                    match (t.concrete, u.concrete) {
                        (NonConcrete::Type(t), NonConcrete::Type(u)) => {
                            let t = &named_types.get(t).unwrap().t;
                            let u = &named_types.get(u).unwrap().t;
                            if !(t.is_signed() && u.is_signed()
                                || t.is_unsigned() && u.is_unsigned())
                            {
                                todo!()
                            }
                        }
                        _ => todo!(),
                    }
                    if t.size_of(named_types, target).unwrap()
                        > u.size_of(named_types, target).unwrap()
                    {
                        Ok(t)
                    } else {
                        Ok(u)
                    }
                },
            ))
            .with_compile(Box::new(|node, res, builders, mut builder| {
                let mut args = SmallVec::<[_; 2]>::new();
                for expr in &node.children[1..] {
                    if let Some(expr) = expr {
                        let (v, f) = Node::compile(*expr, res, builders, builder)?;
                        builder = ValueBuilder(f);
                        args.push(v);
                    }
                }
                let mut result = TypedValue::new(*primitive::UNIT, Value::Unit);
                builder.0 =
                    builder
                        .0
                        .build_div(&mut result, node.type_of.unwrap(), args[0], args[1]);
                Ok((result, builder.0))
            }))
    }

    pub fn build_builtin_rem() -> Self {
        Self::new("%")
            .with_infer(Box::new(
                |node, nodes, named_types, strings, scopes, dispatch, types, target| {
                    if node.children.len() > 3 {
                        todo!();
                    }
                    let arg0 = nodes.get(node.children[1].unwrap()).unwrap();
                    let arg1 = nodes.get(node.children[2].unwrap()).unwrap();
                    let (t, _) = collapse(
                        types[&arg0.id()],
                        scopes,
                        strings,
                        dispatch,
                        named_types,
                        nodes,
                    )?;
                    let (u, _) = collapse(
                        types[&arg1.id()],
                        scopes,
                        strings,
                        dispatch,
                        named_types,
                        nodes,
                    )?;
                    match (t.concrete, u.concrete) {
                        (NonConcrete::Type(t), NonConcrete::Type(u)) => {
                            let t = &named_types.get(t).unwrap().t;
                            let u = &named_types.get(u).unwrap().t;
                            if !(t.is_signed() && u.is_signed()
                                || t.is_unsigned() && u.is_unsigned())
                            {
                                todo!()
                            }
                        }
                        _ => todo!(),
                    }
                    if t.size_of(named_types, target).unwrap()
                        > u.size_of(named_types, target).unwrap()
                    {
                        Ok(t)
                    } else {
                        Ok(u)
                    }
                },
            ))
            .with_compile(Box::new(|node, res, builders, mut builder| {
                let mut args = SmallVec::<[_; 2]>::new();
                for expr in &node.children[1..] {
                    if let Some(expr) = expr {
                        let (v, f) = Node::compile(*expr, res, builders, builder)?;
                        builder = ValueBuilder(f);
                        args.push(v);
                    }
                }
                let mut result = TypedValue::new(*primitive::UNIT, Value::Unit);
                builder.0 =
                    builder
                        .0
                        .build_rem(&mut result, node.type_of.unwrap(), args[0], args[1]);
                Ok((result, builder.0))
            }))
    }
}

pub fn init(mut res: Resources<&mut BuilderMacro>) {
    let builders = vec![
        BuilderMacro::build_new_node(),
        BuilderMacro::build_format_ast(),
        BuilderMacro::build_quasiquote(),
        BuilderMacro::build_compile(),
        BuilderMacro::build_builtin_add(),
        BuilderMacro::build_builtin_sub(),
        BuilderMacro::build_builtin_mul(),
        BuilderMacro::build_builtin_div(),
        BuilderMacro::build_builtin_rem(),
    ];
    for builder in builders {
        res.insert(builder.id(), builder);
    }
}
