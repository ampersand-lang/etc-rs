use failure::Fallible;
use smallvec::SmallVec;

use crate::assets::{Asset, Handle, Resources};
use crate::ast::{Kind, Node};
use crate::lir::builder::*;
use crate::lir::context::ExecutionContext;
use crate::lir::{GlobId, Value};
use crate::types::NamedType;
use crate::values::Payload;

pub trait Compile<B>: Asset + Sized {
    type Output;

    fn compile(
        handle: Handle<Self>,
        res: &mut Resources<(&mut Self, &Payload, &mut Value)>,
        builder: B,
    ) -> Fallible<Self::Output>;
}

impl<'a> Compile<Builder<'a>> for Node {
    type Output = (Resources<&'a NamedType>, ExecutionContext);

    fn compile(
        handle: Handle<Self>,
        res: &mut Resources<(&mut Self, &Payload, &mut Value)>,
        builder: Builder<'a>,
    ) -> Fallible<Self::Output> {
        let root = res.get::<Node>(handle).unwrap();
        let f = builder.function().result(root.type_of.unwrap());
        let (_, b): (GlobId, Builder<'a>) = Node::compile(handle, res, f)?;
        Ok(b.build())
    }
}

impl<'a> Compile<FunctionBuilder<'a>> for Node {
    type Output = (GlobId, Builder<'a>);

    fn compile(
        handle: Handle<Self>,
        res: &mut Resources<(&mut Self, &Payload, &mut Value)>,
        builder: FunctionBuilder<'a>,
    ) -> Fallible<Self::Output> {
        let (v, f) = Node::compile(handle, res, ValueBuilder(builder))?;
        let mut id = 0;
        let b = f.build_return(v).build(&mut id);
        Ok((id, b))
    }
}

impl<'a> Compile<ValueBuilder<'a>> for Node {
    type Output = (Value, FunctionBuilder<'a>);

    fn compile(
        handle: Handle<Self>,
        res: &mut Resources<(&mut Self, &Payload, &mut Value)>,
        mut builder: ValueBuilder<'a>,
    ) -> Fallible<Self::Output> {
        // PERF: can we avoid this clone?
        let this = res.get::<Node>(handle).unwrap().as_ref().clone();
        let value = match this.kind {
            Kind::Nil => match this.payload.unwrap() {
                Payload::Integer(i) => (Value::Uint(i), builder.0),
                Payload::Type(t) => (Value::Type(t), builder.0),
                Payload::Identifier(ident) => {
                    let handle =
                        Handle::from_name(this.scope.unwrap(), &ident.as_u128().to_le_bytes());
                    let value = match *res.get::<Value>(handle).expect("binding not found") {
                        Value::Address(addr) => Value::Unref(addr),
                        ref x => x.clone(),
                    };
                    (value, builder.0)
                }
                Payload::Function(id) => (Value::Global(id), builder.0),
                _ => todo!(),
            },
            Kind::Block => {
                let mut result = Value::Unit;
                for expr in &this.children {
                    if let Some(expr) = expr {
                        let (v, f) = Node::compile(*expr, res, builder)?;
                        builder = ValueBuilder(f);
                        result = v;
                    } else {
                        result = Value::Unit;
                    }
                }
                (result, builder.0)
            }
            Kind::Function => {
                let handle = this.children[1].unwrap();
                let body = res.get::<Node>(this.children[1].unwrap()).unwrap();
                let f = builder.0.builder.function().result(body.type_of.unwrap());
                let (id, b): (GlobId, Builder<'a>) = Node::compile(handle, res, f)?;
                builder.0.builder = b;
                let result = Value::Global(id);
                (result, builder.0)
            }
            Kind::Application => {
                let mut args = SmallVec::new();
                for expr in &this.children {
                    if let Some(expr) = expr {
                        let (v, f) = Node::compile(*expr, res, builder)?;
                        builder = ValueBuilder(f);
                        args.push(v);
                    }
                }
                let mut result = Value::Unit;
                let b = builder
                    .0
                    .build_call(&mut result, this.type_of.unwrap(), args);
                (result, b)
            }
            Kind::Binding => {
                let v = if let Some(expr) = this.children[2] {
                    let (v, f) = Node::compile(expr, res, builder)?;
                    builder = ValueBuilder(f);
                    v
                } else {
                    Value::Unit
                };

                let mut addr = Value::Unit;
                let builder = builder
                    .0
                    .build_alloca(&mut addr, this.type_of.unwrap(), 1)
                    .build_store(this.type_of.unwrap(), addr, v);

                let ident = res.get::<Node>(this.children[0].unwrap()).unwrap();
                let ident = match ident.payload.unwrap() {
                    Payload::Identifier(ident) => ident,
                    // TODO: other bindings
                    _ => todo!(),
                };

                let scope = this.scope.unwrap();
                let handle = Handle::from_name(scope, &ident.as_u128().to_le_bytes());
                res.insert::<Value>(handle, addr);
                (Value::Unit, builder)
            }
            Kind::Tuple => todo!(),
            Kind::Declaration => todo!(),
            Kind::Array => todo!(),
            Kind::TupleType => todo!(),
            Kind::Index => todo!(),
            Kind::Dotted => todo!(),
        };
        Ok(value)
    }
}
