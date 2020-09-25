use failure::Fallible;
use hashbrown::HashMap;
use smallvec::SmallVec;

use crate::assets::{Asset, Handle, Resources};
use crate::ast::{Kind, Node};
use crate::lir::builder::*;
use crate::lir::context::{ExecutionContext, VirtualAddress};

use crate::scope::Scope;
use crate::types::{primitive, NamedType};
use crate::values::Payload;

use super::*;

pub trait Compile<B>: Asset + Sized {
    type Output;

    fn compile(
        handle: Handle<Self>,
        res: &mut Resources<(
            &mut Self,
            &Scope,
            &Payload,
            &String,
            &mut TypedValue,
            &mut Elems,
            &mut Variants,
            &mut Bytes,
        )>,
        builder: B,
    ) -> Fallible<Self::Output>;
}

impl<'a> Compile<Builder<'a>> for Node {
    type Output = (Resources<&'a mut NamedType>, ExecutionContext);

    fn compile(
        handle: Handle<Self>,
        res: &mut Resources<(
            &mut Self,
            &Scope,
            &Payload,
            &String,
            &mut TypedValue,
            &mut Elems,
            &mut Variants,
            &mut Bytes,
        )>,
        builder: Builder<'a>,
    ) -> Fallible<Self::Output> {
        let _root = res.get::<Node>(handle).unwrap();
        // TODO: remove this primitive
        let mut start = BasicBlock::new(0);
        let f = builder
            .function("main")
            .add_basic_block(&mut start)
            .result(*primitive::S32);
        let (main, mut b): (FuncId, Builder<'a>) = Node::compile(handle, res, f)?;
        b.ctx.main = main;
        Ok(b.build())
    }
}

impl<'a> Compile<FunctionBuilder<'a>> for Node {
    type Output = (FuncId, Builder<'a>);

    fn compile(
        handle: Handle<Self>,
        res: &mut Resources<(
            &mut Self,
            &Scope,
            &Payload,
            &String,
            &mut TypedValue,
            &mut Elems,
            &mut Variants,
            &mut Bytes,
        )>,
        builder: FunctionBuilder<'a>,
    ) -> Fallible<Self::Output> {
        let (v, f) = Node::compile(handle, res, ValueBuilder(builder))?;
        let mut id = 0;
        let b = f.build_return(v).build(&mut id);
        Ok((id, b))
    }
}

impl<'a> Compile<ValueBuilder<'a>> for Node {
    type Output = (TypedValue, FunctionBuilder<'a>);

    fn compile(
        handle: Handle<Self>,
        res: &mut Resources<(
            &mut Self,
            &Scope,
            &Payload,
            &String,
            &mut TypedValue,
            &mut Elems,
            &mut Variants,
            &mut Bytes,
        )>,
        mut builder: ValueBuilder<'a>,
    ) -> Fallible<Self::Output> {
        // PERF: can we avoid this clone?
        let this = res.get::<Node>(handle).unwrap().as_ref().clone();
        let value = match this.kind {
            Kind::Nil => match this.payload.unwrap() {
                Payload::Unit => (
                    TypedValue::new(this.type_of.unwrap(), Value::Unit),
                    builder.0,
                ),
                Payload::Integer(i) => (
                    TypedValue::new(this.type_of.unwrap(), Value::Uint(i)),
                    builder.0,
                ),
                Payload::Float(f) => (
                    TypedValue::new(this.type_of.unwrap(), Value::Float(f)),
                    builder.0,
                ),
                Payload::Type(t) => (
                    TypedValue::new(this.type_of.unwrap(), Value::Type(t)),
                    builder.0,
                ),
                Payload::String(string) => {
                    let string = res.get::<String>(string).unwrap();
                    let mut addr = VirtualAddress(0);
                    builder.0.builder = builder.0.builder.add_global(&mut addr, string.as_str());
                    (
                        TypedValue::new(this.type_of.unwrap(), Value::Address(addr)),
                        builder.0,
                    )
                }
                Payload::Identifier(ident) if this.alternative => {
                    match res.get::<String>(ident).unwrap().as_str() {
                        "ptr" => (
                            TypedValue::new(this.type_of.unwrap(), Value::Ffi(*foreign::PTR)),
                            builder.0,
                        ),
                        "fn" => (
                            TypedValue::new(this.type_of.unwrap(), Value::Ffi(*foreign::FN)),
                            builder.0,
                        ),
                        "format-ast" => (
                            TypedValue::new(
                                this.type_of.unwrap(),
                                Value::Ffi(*foreign::FORMAT_AST),
                            ),
                            builder.0,
                        ),
                        _ => todo!(),
                    }
                }
                Payload::Identifier(ident) => {
                    let name = res.get::<String>(ident).unwrap();
                    let mut addr = None;
                    let mut iter = Some(this.scope.unwrap());
                    while let Some(scope) = iter {
                        let handle = Handle::from_name(scope, name.as_bytes());
                        if let Some(val) = res.get::<TypedValue>(handle).map(|v| *v) {
                            addr = Some(val);
                            break;
                        }
                        let scope = res.get(scope);
                        if let Some(scope) = scope {
                            iter = Some(scope.parent());
                        } else {
                            iter = None;
                        }
                    }
                    let addr = addr.expect(&format!("binding not found: {}", name.as_str()));
                    let mut value = TypedValue::new(*primitive::UNIT, Value::Unit);
                    match addr.val {
                        Value::Address(_) | Value::Register(_) => {
                            builder.0 = builder.0.build_load(&mut value, addr);
                        }
                        _ => value = addr,
                    }
                    (value, builder.0)
                }
                Payload::Function(id) => (
                    TypedValue::new(this.type_of.unwrap(), Value::Function(id)),
                    builder.0,
                ),
            },
            Kind::Block => {
                let mut result = TypedValue::new(*primitive::UNIT, Value::Unit);
                for expr in &this.children {
                    if let Some(expr) = expr {
                        let (v, f) = Node::compile(*expr, res, builder)?;
                        builder = ValueBuilder(f);
                        result = v;
                    } else {
                        result = TypedValue::new(*primitive::UNIT, Value::Unit);
                    }
                }
                (result, builder.0)
            }
            Kind::Function => {
                let mut param_types = SmallVec::<[_; 6]>::new();
                let params = this.children[0].unwrap();
                let params = res.get(params).unwrap();
                let mut transaction = HashMap::new();
                match params.kind {
                    Kind::Tuple => {
                        for (idx, &param) in params.children.iter().enumerate() {
                            let param = &*res.get::<Node>(param.unwrap()).unwrap();
                            match param.kind {
                                Kind::Declaration => {
                                    param_types.push(param.type_of.unwrap());
                                    let v = Value::Arg(idx as _);

                                    let ident =
                                        res.get::<Node>(param.children[0].unwrap()).unwrap();
                                    let ident = match ident.payload.unwrap() {
                                        Payload::Identifier(ident) => ident,
                                        // TODO: other bindings
                                        _ => todo!(),
                                    };
                                    let name = res.get(ident).unwrap();

                                    let scope = param.scope.unwrap();
                                    let handle = Handle::from_name(scope, name.as_bytes());
                                    transaction
                                        .insert(handle, TypedValue::new(param.type_of.unwrap(), v));
                                }
                                // other patterns
                                _ => todo!(),
                            }
                        }
                    }
                    _ => todo!(),
                }

                for (k, v) in transaction {
                    res.insert(k, v);
                }

                let handle = this.children[1].unwrap();
                let body = res.get::<Node>(handle).unwrap();
                let mut name = String::new();
                for _ in 0..16 {
                    name.push((b'a' + rand::random::<u8>() % 26) as char);
                }
                let mut start = BasicBlock::new(0);
                let mut f = builder
                    .0
                    .builder
                    .function(name)
                    .add_basic_block(&mut start)
                    .result(body.type_of.unwrap());
                for param in param_types {
                    let mut v = TypedValue::new(*primitive::UNIT, Value::Unit);
                    f = f.parameter(&mut v, param);
                }
                let (id, b): (FuncId, Builder<'a>) = Node::compile(handle, res, f)?;
                builder.0.builder = b;
                let result = TypedValue::new(this.type_of.unwrap(), Value::Function(id));
                (result, builder.0)
            }
            Kind::Application => {
                let func = this.children[0].unwrap();
                // PERF: clone is bad
                let func = res.get::<Node>(func).unwrap().as_ref().clone();
                // NOTE: special forms
                let done = match func.kind {
                    Kind::Nil => match func.payload.unwrap() {
                        Payload::Identifier(ident) if func.alternative => {
                            match res.get::<String>(ident).unwrap().as_str() {
                                "quasiquote" => Some(TypedValue::new(
                                    *primitive::NODE,
                                    Value::Node(this.children[1].unwrap()),
                                )),
                                "new-node" => {
                                    let mut args = SmallVec::new();
                                    for expr in &this.children[1..] {
                                        if let Some(expr) = expr {
                                            let (v, f) = Node::compile(*expr, res, builder)?;
                                            builder = ValueBuilder(f);
                                            args.push(v);
                                        }
                                    }
                                    let mut result = TypedValue::new(*primitive::UNIT, Value::Unit);
                                    builder.0 = builder.0.build_newnode(&mut result, args);
                                    Some(result)
                                }
                                _ => None,
                            }
                        }
                        _ => None,
                    },
                    _ => None,
                };
                if let Some(done) = done {
                    (done, builder.0)
                } else {
                    let mut args = SmallVec::new();
                    for expr in &this.children {
                        if let Some(expr) = expr {
                            let (v, f) = Node::compile(*expr, res, builder)?;
                            builder = ValueBuilder(f);
                            args.push(v);
                        }
                    }
                    let mut result = TypedValue::new(*primitive::UNIT, Value::Unit);
                    let b = builder.0.build_call(&mut result, args);
                    (result, b)
                }
            }
            Kind::Binding => {
                let v = if let Some(expr) = this.children[2] {
                    let (v, f) = Node::compile(expr, res, builder)?;
                    builder = ValueBuilder(f);
                    v
                } else {
                    TypedValue::new(*primitive::UNIT, Value::Unit)
                };

                let mut addr = TypedValue::new(*primitive::UNIT, Value::Unit);
                let builder = builder
                    .0
                    .build_alloca(&mut addr, v.typ, 1)
                    .build_store(addr, v);

                let ident = res.get::<Node>(this.children[0].unwrap()).unwrap();
                let ident = match ident.payload.unwrap() {
                    Payload::Identifier(ident) => ident,
                    // TODO: other bindings
                    _ => todo!(),
                };
                let name = res.get(ident).unwrap();

                let scope = this.scope.unwrap();
                let handle = Handle::from_name(scope, name.as_bytes());
                res.insert::<TypedValue>(handle, addr);
                (v, builder)
            }
            Kind::Tuple => {
                let mut result = Elems::new();
                for expr in &this.children {
                    let (v, f) = Node::compile(expr.unwrap(), res, builder)?;
                    builder = ValueBuilder(f);
                    result.push(v);
                }
                let handle = Handle::new();
                res.insert::<Elems>(handle, result);
                (
                    TypedValue::new(this.type_of.unwrap(), Value::Struct(handle)),
                    builder.0,
                )
            }
            Kind::Argument => todo!(),
            Kind::Declaration => Node::compile(this.children[0].unwrap(), res, builder)?,
            Kind::Array => {
                let mut result = Elems::new();
                for expr in &this.children {
                    let (v, f) = Node::compile(expr.unwrap(), res, builder)?;
                    builder = ValueBuilder(f);
                    result.push(v);
                }
                let handle = Handle::new();
                res.insert::<Elems>(handle, result);
                (
                    TypedValue::new(this.type_of.unwrap(), Value::Array(handle)),
                    builder.0,
                )
            }
            Kind::Index => todo!(),
            Kind::Dotted => todo!(),
            Kind::With => todo!(),
        };
        Ok(value)
    }
}
