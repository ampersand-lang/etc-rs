use either::Either;
use failure::Fallible;
use smallvec::SmallVec;

use crate::assets::{Handle, Resources};
use crate::ast::Node;
use crate::lir::{
    builder::{FunctionBuilder, ValueBuilder},
    compile::Compile as _,
    foreign, Bytes, Elems, TypedValue, Value, Variants,
};
use crate::scope::Scope;
use crate::types::{primitive, NamedType, TypeId};
use crate::values::Payload;

pub type Infer = Box<
    dyn Fn(
            &Node,
            &Resources<&mut Node>,
            &mut Resources<&mut NamedType>,
            &Resources<&String>,
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
    ) -> Option<Fallible<TypeId>> {
        self.infer
            .as_ref()
            .map(|f| f(node, nodes, named_types, strings))
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
            .with_infer(Box::new(|_node, _nodes, _named_types, _string| {
                Ok(*primitive::NODE)
            }))
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
            .with_infer(Box::new(|_node, _nodes, _named_types, _string| {
                Ok(*primitive::NODE)
            }))
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
            .with_infer(Box::new(|_node, _nodes, _named_types, _string| {
                Ok(*primitive::NODE)
            }))
            .with_compile(Box::new(|node, _res, _builders, builder| {
                let node =
                    TypedValue::new(*primitive::NODE, Value::Node(node.children[1].unwrap()));
                Ok((node, builder.0))
            }))
    }

    pub fn build_compile() -> Self {
        Self::new("compile")
            .with_infer(Box::new(|_node, _nodes, _named_types, _string| {
                Ok(*primitive::UNIT)
            }))
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
}

pub fn init(mut res: Resources<&mut BuilderMacro>) {
    let builder = BuilderMacro::build_new_node();
    res.insert(builder.id(), builder);

    let builder = BuilderMacro::build_format_ast();
    res.insert(builder.id(), builder);

    let builder = BuilderMacro::build_quasiquote();
    res.insert(builder.id(), builder);

    let builder = BuilderMacro::build_compile();
    res.insert(builder.id(), builder);
}
