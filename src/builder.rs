use either::Either;
use failure::Fallible;
use hashbrown::HashMap;
use smallvec::{smallvec, SmallVec};

use crate::assets::{Handle, Resources};
use crate::ast::{Node, NodeId};
use crate::dispatch::Dispatcher;
use crate::lir::{
    builder::{FunctionBuilder, ValueBuilder},
    compile::Compile as _,
    foreign,
    target::Target,
    BasicBlock, Bytes, Elems, RegisterConstraint, TypedValue, Value, Variants, ICMP_EQ, ICMP_GE,
    ICMP_GT, ICMP_LE, ICMP_LT, ICMP_NE,
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
                        let (v, f) = Node::compile(*expr, None, res, builders, builder)?;
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
                        let (v, f) = Node::compile(*expr, None, res, builders, builder)?;
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
                        let (v, f) = Node::compile(*expr, None, res, builders, builder)?;
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
                        let (v, f) = Node::compile(*expr, None, res, builders, builder)?;
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
                        let (v, f) = Node::compile(*expr, None, res, builders, builder)?;
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
                        let (v, f) = Node::compile(*expr, None, res, builders, builder)?;
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
                        let (v, f) = Node::compile(*expr, None, res, builders, builder)?;
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
                        let (v, f) = Node::compile(*expr, None, res, builders, builder)?;
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

    pub fn build_builtin_bit_and() -> Self {
        Self::new("&")
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
                                || t.is_unsigned() && u.is_unsigned()
                                || t.is_bool() && u.is_bool())
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
                        let (v, f) = Node::compile(*expr, None, res, builders, builder)?;
                        builder = ValueBuilder(f);
                        args.push(v);
                    }
                }
                let mut result = TypedValue::new(*primitive::UNIT, Value::Unit);
                builder.0 =
                    builder
                        .0
                        .build_bit_and(&mut result, node.type_of.unwrap(), args[0], args[1]);
                Ok((result, builder.0))
            }))
    }

    pub fn build_builtin_bit_or() -> Self {
        Self::new("|")
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
                                || t.is_unsigned() && u.is_unsigned()
                                || t.is_bool() && u.is_bool())
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
                        let (v, f) = Node::compile(*expr, None, res, builders, builder)?;
                        builder = ValueBuilder(f);
                        args.push(v);
                    }
                }
                let mut result = TypedValue::new(*primitive::UNIT, Value::Unit);
                builder.0 =
                    builder
                        .0
                        .build_bit_or(&mut result, node.type_of.unwrap(), args[0], args[1]);
                Ok((result, builder.0))
            }))
    }

    pub fn build_builtin_bit_xor() -> Self {
        Self::new("^")
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
                                || t.is_unsigned() && u.is_unsigned()
                                || t.is_bool() && u.is_bool())
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
                        let (v, f) = Node::compile(*expr, None, res, builders, builder)?;
                        builder = ValueBuilder(f);
                        args.push(v);
                    }
                }
                let mut result = TypedValue::new(*primitive::UNIT, Value::Unit);
                builder.0 =
                    builder
                        .0
                        .build_bit_xor(&mut result, node.type_of.unwrap(), args[0], args[1]);
                Ok((result, builder.0))
            }))
    }

    pub fn build_builtin_eq() -> Self {
        Self::new("==")
            .with_infer(Box::new(
                |_node, _nodes, _named_types, _strings, _scopes, _dispatch, _types, _target| {
                    Ok(*primitive::BOOL)
                },
            ))
            .with_compile(Box::new(|node, res, builders, mut builder| {
                let mut args = SmallVec::<[_; 2]>::new();
                for expr in &node.children[1..] {
                    if let Some(expr) = expr {
                        let (v, f) = Node::compile(*expr, None, res, builders, builder)?;
                        builder = ValueBuilder(f);
                        args.push(v);
                    }
                }
                let mut result = TypedValue::new(*primitive::UNIT, Value::Unit);
                builder.0 = builder.0.build_icmp(&mut result, ICMP_EQ, args[0], args[1]);
                Ok((result, builder.0))
            }))
    }

    pub fn build_builtin_ne() -> Self {
        Self::new("!=")
            .with_infer(Box::new(
                |_node, _nodes, _named_types, _strings, _scopes, _dispatch, _types, _target| {
                    Ok(*primitive::BOOL)
                },
            ))
            .with_compile(Box::new(|node, res, builders, mut builder| {
                let mut args = SmallVec::<[_; 2]>::new();
                for expr in &node.children[1..] {
                    if let Some(expr) = expr {
                        let (v, f) = Node::compile(*expr, None, res, builders, builder)?;
                        builder = ValueBuilder(f);
                        args.push(v);
                    }
                }
                let mut result = TypedValue::new(*primitive::UNIT, Value::Unit);
                builder.0 = builder.0.build_icmp(&mut result, ICMP_NE, args[0], args[1]);
                Ok((result, builder.0))
            }))
    }

    pub fn build_builtin_lt() -> Self {
        Self::new("<")
            .with_infer(Box::new(
                |_node, _nodes, _named_types, _strings, _scopes, _dispatch, _types, _target| {
                    Ok(*primitive::BOOL)
                },
            ))
            .with_compile(Box::new(|node, res, builders, mut builder| {
                let mut args = SmallVec::<[_; 2]>::new();
                for expr in &node.children[1..] {
                    if let Some(expr) = expr {
                        let (v, f) = Node::compile(*expr, None, res, builders, builder)?;
                        builder = ValueBuilder(f);
                        args.push(v);
                    }
                }
                let mut result = TypedValue::new(*primitive::UNIT, Value::Unit);
                builder.0 = builder.0.build_icmp(&mut result, ICMP_LT, args[0], args[1]);
                Ok((result, builder.0))
            }))
    }

    pub fn build_builtin_gt() -> Self {
        Self::new(">")
            .with_infer(Box::new(
                |_node, _nodes, _named_types, _strings, _scopes, _dispatch, _types, _target| {
                    Ok(*primitive::BOOL)
                },
            ))
            .with_compile(Box::new(|node, res, builders, mut builder| {
                let mut args = SmallVec::<[_; 2]>::new();
                for expr in &node.children[1..] {
                    if let Some(expr) = expr {
                        let (v, f) = Node::compile(*expr, None, res, builders, builder)?;
                        builder = ValueBuilder(f);
                        args.push(v);
                    }
                }
                let mut result = TypedValue::new(*primitive::UNIT, Value::Unit);
                builder.0 = builder.0.build_icmp(&mut result, ICMP_GT, args[0], args[1]);
                Ok((result, builder.0))
            }))
    }

    pub fn build_builtin_le() -> Self {
        Self::new("<=")
            .with_infer(Box::new(
                |_node, _nodes, _named_types, _strings, _scopes, _dispatch, _types, _target| {
                    Ok(*primitive::BOOL)
                },
            ))
            .with_compile(Box::new(|node, res, builders, mut builder| {
                let mut args = SmallVec::<[_; 2]>::new();
                for expr in &node.children[1..] {
                    if let Some(expr) = expr {
                        let (v, f) = Node::compile(*expr, None, res, builders, builder)?;
                        builder = ValueBuilder(f);
                        args.push(v);
                    }
                }
                let mut result = TypedValue::new(*primitive::UNIT, Value::Unit);
                builder.0 = builder.0.build_icmp(&mut result, ICMP_LE, args[0], args[1]);
                Ok((result, builder.0))
            }))
    }

    pub fn build_builtin_ge() -> Self {
        Self::new(">=")
            .with_infer(Box::new(
                |_node, _nodes, _named_types, _strings, _scopes, _dispatch, _types, _target| {
                    Ok(*primitive::BOOL)
                },
            ))
            .with_compile(Box::new(|node, res, builders, mut builder| {
                let mut args = SmallVec::<[_; 2]>::new();
                for expr in &node.children[1..] {
                    if let Some(expr) = expr {
                        let (v, f) = Node::compile(*expr, None, res, builders, builder)?;
                        builder = ValueBuilder(f);
                        args.push(v);
                    }
                }
                let mut result = TypedValue::new(*primitive::UNIT, Value::Unit);
                builder.0 = builder.0.build_icmp(&mut result, ICMP_GE, args[0], args[1]);
                Ok((result, builder.0))
            }))
    }

    pub fn build_builtin_if() -> Self {
        Self::new("if")
            .with_infer(Box::new(
                |node, nodes, named_types, strings, scopes, dispatch, types, _target| {
                    if !matches!(node.children.len(), 3..=4) {
                        todo!();
                    }
                    let arg0 = nodes.get(node.children[2].unwrap()).unwrap();
                    let (t, _) = collapse(
                        types[&arg0.id()],
                        scopes,
                        strings,
                        dispatch,
                        named_types,
                        nodes,
                    )?;
                    if node.children.len() == 3 {
                        let arg1 = nodes.get(node.children[2].unwrap()).unwrap();
                        let (_u, _) = collapse(
                            types[&arg1.id()],
                            scopes,
                            strings,
                            dispatch,
                            named_types,
                            nodes,
                        )?;
                    }
                    // TODO: compare types
                    Ok(t)
                },
            ))
            .with_compile(Box::new(|node, res, builders, mut builder| {
                let expr = node.children[1].unwrap();
                let (v, mut f) = Node::compile(expr, None, res, builders, builder)?;

                let mut bb0 = BasicBlock::new(0);
                let mut bb1 = BasicBlock::new(0);
                f = f.add_basic_block(&mut bb0).add_basic_block(&mut bb1);

                builder = ValueBuilder(f);
                builder.0 = builder.0.build_cond_br(v, bb0, bb1);

                builder.0 = builder.0.set_basic_block_as_current(bb0);
                let then = node.children[2].unwrap();
                let (value, f) = Node::compile(then, None, res, builders, builder)?;
                builder = ValueBuilder(f);

                let mut res0 = TypedValue::new(*primitive::UNIT, Value::Unit);
                builder.0 = builder.0.build_copy(
                    &mut res0,
                    value.typ,
                    RegisterConstraint::PhiRegister,
                    value,
                );

                builder.0 = builder.0.set_basic_block_as_current(bb1);
                let or_else = node.children[3].unwrap();
                let (value, f) = Node::compile(or_else, None, res, builders, builder)?;
                builder = ValueBuilder(f);

                let mut res1 = TypedValue::new(*primitive::UNIT, Value::Unit);
                builder.0 = builder.0.build_copy(
                    &mut res1,
                    value.typ,
                    RegisterConstraint::PhiRegister,
                    value,
                );

                let mut bb_fin = BasicBlock::new(0);
                let mut result = TypedValue::new(*primitive::UNIT, Value::Unit);
                builder.0 = builder
                    .0
                    .add_basic_block(&mut bb_fin)
                    .set_basic_block_as_current(bb0)
                    .build_br(bb_fin)
                    .set_basic_block_as_current(bb1)
                    .build_br(bb_fin)
                    .set_basic_block_as_current(bb_fin)
                    .build_phi(&mut result, res1.typ, smallvec![(res0, bb0), (res1, bb1)]);

                Ok((result, builder.0))
            }))
    }

    pub fn build_builtin_while() -> Self {
        Self::new("while")
            .with_infer(Box::new(
                |_node, _nodes, _named_types, _strings, _scopes, _dispatch, _types, _target| {
                    Ok(*primitive::UNIT)
                },
            ))
            .with_compile(Box::new(|node, res, builders, mut builder| {
                let mut bb0 = BasicBlock::new(0);
                let mut bb1 = BasicBlock::new(0);
                let mut bb_fin = BasicBlock::new(0);
                builder.0 = builder
                    .0
                    .add_basic_block(&mut bb0)
                    .add_basic_block(&mut bb1)
                    .add_basic_block(&mut bb_fin)
                    .build_br(bb0);

                builder.0 = builder.0.set_basic_block_as_current(bb0);
                let expr = node.children[1].unwrap();
                let (v, mut f) = Node::compile(expr, None, res, builders, builder)?;

                f = f.build_cond_br(v, bb1, bb_fin);
                builder = ValueBuilder(f);

                builder.0 = builder.0.set_basic_block_as_current(bb1);
                let then = node.children[2].unwrap();
                let (_, f) = Node::compile(then, None, res, builders, builder)?;
                builder = ValueBuilder(f);

                builder.0 = builder
                    .0
                    .set_basic_block_as_current(bb1)
                    .build_br(bb0)
                    .set_basic_block_as_current(bb_fin);

                let result = TypedValue::new(*primitive::UNIT, Value::Unit);
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
        BuilderMacro::build_builtin_bit_and(),
        BuilderMacro::build_builtin_bit_or(),
        BuilderMacro::build_builtin_bit_xor(),
        BuilderMacro::build_builtin_eq(),
        BuilderMacro::build_builtin_ne(),
        BuilderMacro::build_builtin_lt(),
        BuilderMacro::build_builtin_gt(),
        BuilderMacro::build_builtin_le(),
        BuilderMacro::build_builtin_ge(),
        BuilderMacro::build_builtin_if(),
        BuilderMacro::build_builtin_while(),
    ];
    for builder in builders {
        res.insert(builder.id(), builder);
    }
}
