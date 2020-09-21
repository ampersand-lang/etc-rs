use failure::Fallible;
use smallvec::SmallVec;

use crate::assets::{Asset, Handle, Resources};
use crate::ast::{Kind, Node};
use crate::lir::builder::*;
use crate::lir::context::{ExecutionContext, VirtualAddress};

use crate::types::{primitive, NamedType};
use crate::values::Payload;

use super::*;

pub trait Compile<B>: Asset + Sized {
    type Output;

    fn compile(
        handle: Handle<Self>,
        res: &mut Resources<(
            &mut Self,
            &Payload,
            &String,
            &mut Value,
            &mut Elems,
            &mut Fields,
            &mut Variants,
            &mut Bytes,
        )>,
        builder: B,
    ) -> Fallible<Self::Output>;
}

impl<'a> Compile<Builder<'a>> for Node {
    type Output = (Resources<&'a NamedType>, ExecutionContext);

    fn compile(
        handle: Handle<Self>,
        res: &mut Resources<(
            &mut Self,
            &Payload,
            &String,
            &mut Value,
            &mut Elems,
            &mut Fields,
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
            &Payload,
            &String,
            &mut Value,
            &mut Elems,
            &mut Fields,
            &mut Variants,
            &mut Bytes,
        )>,
        builder: FunctionBuilder<'a>,
    ) -> Fallible<Self::Output> {
        let this = res.get::<Node>(handle).unwrap();
        let t = this.type_of.unwrap();
        let (v, f) = Node::compile(handle, res, ValueBuilder(builder))?;
        let mut id = 0;
        let b = f.build_return(t, v).build(&mut id);
        Ok((id, b))
    }
}

impl<'a> Compile<ValueBuilder<'a>> for Node {
    type Output = (Value, FunctionBuilder<'a>);

    fn compile(
        handle: Handle<Self>,
        res: &mut Resources<(
            &mut Self,
            &Payload,
            &String,
            &mut Value,
            &mut Elems,
            &mut Fields,
            &mut Variants,
            &mut Bytes,
        )>,
        mut builder: ValueBuilder<'a>,
    ) -> Fallible<Self::Output> {
        // PERF: can we avoid this clone?
        let this = res.get::<Node>(handle).unwrap().as_ref().clone();
        let value = match this.kind {
            Kind::Nil => match this.payload.unwrap() {
                Payload::Unit => (Value::Unit, builder.0),
                Payload::Integer(i) => (Value::Uint(i), builder.0),
                Payload::Float(f) => (Value::Float(f), builder.0),
                Payload::Type(t) => (Value::Type(t), builder.0),
                Payload::String(string) => {
                    let string = res.get::<String>(string).unwrap();
                    let mut addr = VirtualAddress(0);
                    builder.0.builder = builder.0.builder.add_global(&mut addr, string.as_str());
                    (Value::Address(addr), builder.0)
                }
                Payload::Identifier(ident) if this.alternative => {
                    match res.get::<String>(ident).unwrap().as_str() {
                        "ptr" => (Value::Ffi(*foreign::PTR), builder.0),
                        "fn" => (Value::Ffi(*foreign::FN), builder.0),
                        "format-ast" => (Value::Ffi(*foreign::FORMAT_AST), builder.0),
                        _ => todo!(),
                    }
                }
                Payload::Identifier(ident) => {
                    let name = res.get::<String>(ident).unwrap();
                    let handle = Handle::from_name(this.scope.unwrap(), name.as_bytes());
                    let addr = *res
                        .get::<Value>(handle)
                        .expect(&format!("binding not found: {}", name.as_str()));
                    let mut value = Value::Unit;
                    match addr {
                        Value::Address(_) | Value::Register(_) => {
                            builder.0 =
                                builder
                                    .0
                                    .build_load(&mut value, this.type_of.unwrap(), addr);
                        }
                        _ => value = addr,
                    }
                    (value, builder.0)
                }
                Payload::Function(id) => (Value::Function(id), builder.0),
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
                let mut name = String::new();
                for _ in 0..16 {
                    name.push((b'a' + rand::random::<u8>() % 26) as char);
                }
                let mut start = BasicBlock::new(0);
                let f = builder
                    .0
                    .builder
                    .function(name)
                    .add_basic_block(&mut start)
                    .result(body.type_of.unwrap());
                let (id, b): (FuncId, Builder<'a>) = Node::compile(handle, res, f)?;
                builder.0.builder = b;
                let result = Value::Function(id);
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
                                "quasiquote" => Some(Value::Node(this.children[1].unwrap())),
                                "new-node" => {
                                    let mut args = SmallVec::new();
                                    for expr in &this.children[1..] {
                                        if let Some(expr) = expr {
                                            let (v, f) = Node::compile(*expr, res, builder)?;
                                            builder = ValueBuilder(f);
                                            args.push(v);
                                        }
                                    }
                                    let mut result = Value::Unit;
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
                    let mut result = Value::Unit;
                    let b = builder
                        .0
                        .build_call(&mut result, this.type_of.unwrap(), args);
                    (result, b)
                }
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
                let name = res.get(ident).unwrap();

                let scope = this.scope.unwrap();
                let handle = Handle::from_name(scope, name.as_bytes());
                res.insert::<Value>(handle, addr);
                (v, builder)
            }
            Kind::Tuple => {
                let mut result = Fields::new();
                for expr in &this.children {
                    let (v, f) = Node::compile(expr.unwrap(), res, builder)?;
                    builder = ValueBuilder(f);
                    let expr = res.get::<Node>(expr.unwrap()).unwrap();
                    result.push(TypedValue {
                        val: v,
                        typ: expr.type_of.unwrap(),
                    });
                }
                let handle = Handle::new();
                res.insert::<Fields>(handle, result);
                (Value::Struct(handle), builder.0)
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
                (Value::Array(handle), builder.0)
            }
            Kind::Index => todo!(),
            Kind::Dotted => todo!(),
        };
        Ok(value)
    }
}
