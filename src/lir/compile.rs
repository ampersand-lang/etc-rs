use either::Either;
use failure::Fallible;
use hashbrown::HashMap;
use smallvec::SmallVec;

use crate::assets::{Asset, Handle, Resources};
use crate::ast::{self, Kind, Node};
use crate::builder::BuilderMacro;
use crate::lir::builder::*;
use crate::lir::context::{ExecutionContext, VirtualAddress};

use crate::scope::Scope;
use crate::types::{primitive, NamedType, NonConcrete, Type};
use crate::values::Payload;

use super::*;

pub trait Compile<B>: Asset + Sized {
    type Output;

    fn compile(
        handle: Handle<Self>,
        name: Option<&str>,
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
        builders: &Resources<&BuilderMacro>,
        builder: B,
    ) -> Fallible<Self::Output>;
}

impl<'a> Compile<Builder<'a>> for Node {
    type Output = (Resources<&'a mut NamedType>, ExecutionContext);

    fn compile(
        handle: Handle<Self>,
        name: Option<&str>,
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
        builders: &Resources<&BuilderMacro>,
        builder: Builder<'a>,
    ) -> Fallible<Self::Output> {
        let _root = res.get::<Node>(handle).unwrap();
        // TODO: remove this primitive
        let mut start = BasicBlock::new(0);
        let f = builder
            .function(name.unwrap())
            .add_basic_block(&mut start)
            .set_basic_block_as_current(start)
            .result(*primitive::S32);
        let (main, mut b): (FuncId, Builder<'a>) = Node::compile(handle, name, res, builders, f)?;
        b.ctx.main = main;
        Ok(b.build())
    }
}

impl<'a> Compile<FunctionBuilder<'a>> for Node {
    type Output = (FuncId, Builder<'a>);

    fn compile(
        handle: Handle<Self>,
        name: Option<&str>,
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
        builders: &Resources<&BuilderMacro>,
        mut builder: FunctionBuilder<'a>,
    ) -> Fallible<Self::Output> {
        let name = name.unwrap();
        let attributes = builder.builder.attributes.take();
        if let Some(attributes) = attributes {
            for attribute in attributes {
                match attribute.kind {
                    ast::AttributeKind::InlineAlways => {
                        builder.builder.inlined.insert(name.to_string());
                        builder = builder.attribute(Attribute {
                            kind: AttributeKind::InlineAlways,
                        })
                    }
                }
            }
        }
        let (v, f) = Node::compile(handle, None, res, builders, ValueBuilder(builder))?;
        let mut id = FuncId::new(0, 0);
        let b = f.build_return(v).build(&mut id);
        Ok((id, b))
    }
}

impl<'a> Compile<ValueBuilder<'a>> for Node {
    type Output = (TypedValue, FunctionBuilder<'a>);

    fn compile(
        handle: Handle<Self>,
        _name: Option<&str>,
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
        builders: &Resources<&BuilderMacro>,
        mut builder: ValueBuilder<'a>,
    ) -> Fallible<Self::Output> {
        // PERF: can we avoid this clone?
        let this = res.get::<Node>(handle).unwrap().as_ref().clone();
        let value = match this.kind {
            // TODO: check the thing the < and > the thing the rfes adn teh defucks the derefs here
            Kind::Nil => match this.payload.unwrap() {
                Payload::Unit => (
                    TypedValue::new(this.type_of.unwrap(), Value::Unit),
                    builder.0,
                ),
                Payload::Bool(p) => (
                    TypedValue::new(this.type_of.unwrap(), Value::Bool(p)),
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
                    let mut bytes = string.as_bytes().to_vec();
                    bytes.push(0);
                    let mut addr = VirtualAddress(0);
                    builder.0.builder = builder.0.builder.add_global(&mut addr, bytes.as_slice());
                    (
                        TypedValue::new(this.type_of.unwrap(), Value::Address(addr)),
                        builder.0,
                    )
                }
                Payload::Identifier(_ident) if this.alternative => {
                    // placeholder value
                    (
                        TypedValue::new(this.type_of.unwrap(), Value::Unit),
                        builder.0,
                    )
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
                    match this.refs {
                        n if n < 0 => {
                            let mut addr = addr;
                            for _ in 0..=-n {
                                match addr.val {
                                    Value::Address(_) | Value::Register(_) => {
                                        builder.0 = builder.0.build_load(&mut value, addr);
                                        addr = value;
                                    }
                                    _ => todo!(),
                                }
                            }
                        }
                        0 => match addr.val {
                            Value::Address(_) | Value::Register(_) => {
                                builder.0 = builder.0.build_load(&mut value, addr);
                            }
                            // FIXME: this is a hack and should not be
                            Value::Type(_) => value = addr,
                            _ => todo!(),
                        },
                        1 => value = addr,
                        _ => todo!(),
                    }
                    (value, builder.0)
                }
                Payload::Function(id) => (
                    TypedValue::new(this.type_of.unwrap(), Value::Function(id)),
                    builder.0,
                ),
                Payload::Struct
                | Payload::Enum
                | Payload::Union
                | Payload::Tagged
                | Payload::Class => unimplemented!(),
            },
            Kind::Block => {
                let mut result = TypedValue::new(*primitive::UNIT, Value::Unit);
                for expr in &this.children {
                    if let Some(expr) = expr {
                        let (v, f) = Node::compile(*expr, None, res, builders, builder)?;
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
                let name = builder.0.builder.name.take().unwrap_or_else(|| {
                    let mut name = String::new();
                    for _ in 0..16 {
                        name.push((b'a' + rand::random::<u8>() % 26) as char);
                    }
                    name
                });
                let mut start = BasicBlock::new(0);
                let mut f = builder
                    .0
                    .builder
                    .function(name.clone())
                    .add_basic_block(&mut start)
                    .set_basic_block_as_current(start)
                    .result(body.type_of.unwrap());
                for param in param_types {
                    let mut v = TypedValue::new(*primitive::UNIT, Value::Unit);
                    f = f.parameter(&mut v, param);
                }
                let (id, b): (FuncId, Builder<'a>) =
                    Node::compile(handle, Some(&name), res, builders, f)?;
                builder.0.builder = b;
                let result = TypedValue::new(this.type_of.unwrap(), Value::Function(id));
                (result, builder.0)
            }
            Kind::Application => {
                let func = this.children[0].unwrap();
                // PERF: clone is bad
                let func = res.get::<Node>(func).unwrap().as_ref().clone();
                // NOTE: special forms
                match func.kind {
                    Kind::Nil if func.alternative => match func.payload.unwrap() {
                        Payload::Identifier(ident) => {
                            let alternative = res.get::<String>(ident).unwrap();
                            let handle = Handle::from_hash(alternative.as_bytes());
                            if let Some(b) = builders.get::<BuilderMacro>(handle) {
                                match b.compile(&this, res, builders, builder) {
                                    Either::Left(res) => return res,
                                    Either::Right(b) => builder = b,
                                }
                            } else {
                                panic!("not an alternative: `${}`", alternative.as_str());
                            }
                        }
                        Payload::Struct => {
                            let result = TypedValue::new(*primitive::UNIT, Value::Unit);
                            return Ok((result, builder.0));
                        }
                        _ => {}
                    },
                    _ => {}
                }
                match func.type_of.unwrap().concrete {
                    NonConcrete::Type(t) => {
                        let typ = builder.0.builder.res.get(t).unwrap();
                        match typ.t {
                            Type::Constructor(t) => {
                                let mut args = SmallVec::new();
                                for expr in &this.children[1..] {
                                    if let Some(expr) = expr {
                                        let (v, f) =
                                            Node::compile(*expr, None, res, builders, builder)?;
                                        builder = ValueBuilder(f);
                                        args.push(v);
                                    }
                                }

                                let elems = Handle::new();
                                res.insert(elems, args);
                                let result = TypedValue::new(t, Value::Struct(elems));
                                return Ok((result, builder.0));
                            }
                            _ => {}
                        }
                    }
                    _ => {}
                }
                let mut args = SmallVec::new();
                for expr in &this.children {
                    if let Some(expr) = expr {
                        let (v, f) = Node::compile(*expr, None, res, builders, builder)?;
                        builder = ValueBuilder(f);
                        args.push(v);
                    }
                }
                match func.kind {
                    Kind::Nil => match func.payload.unwrap() {
                        Payload::Identifier(ident) => {
                            let ident = res.get::<String>(ident).unwrap();
                            if builder.0.builder.inlined.contains(ident.as_str()) {
                                let mut func = None;
                                for f in builder.0.builder.ctx.iter() {
                                    if f.name == ident.as_str() {
                                        // PERF: clone bad
                                        func = Some(f.clone());
                                        break;
                                    }
                                }
                                let mut result = TypedValue::new(*primitive::UNIT, Value::Unit);
                                let func = func.unwrap();
                                args.remove(0);
                                let b = builder.0.inline(&mut result, func, args);
                                return Ok((result, b));
                            }
                        }
                        _ => {}
                    },
                    _ => {}
                }
                let mut result = TypedValue::new(*primitive::UNIT, Value::Unit);
                let b = builder.0.build_call(&mut result, args);
                (result, b)
            }
            // XXX: is this correct?
            Kind::Binding | Kind::Global => {
                if matches!(this.kind, Kind::Global) {
                    let name = this.children[0].unwrap();
                    let name = res.get(name).unwrap();
                    let name = match name.payload.unwrap() {
                        Payload::Identifier(ident) => res.get(ident).unwrap().to_string(),
                        _ => panic!("global is not bound to an identifier"),
                    };
                    builder.0.builder.name = Some(name);
                    builder.0.builder.attributes = Some(this.attributes.clone());
                }
                let v = if let Some(expr) = this.children[2] {
                    let value = res.get(expr).unwrap();
                    let mut eliminate = match value.payload {
                        Some(Payload::Function(_)) => true,
                        _ => false,
                    };
                    eliminate |= match value.kind {
                        Kind::Application => {
                            let func = value.children[0].unwrap();
                            let func = res.get(func).unwrap();
                            match func.payload {
                                Some(Payload::Struct) => true,
                                _ => false,
                            }
                        }
                        _ => false,
                    };
                    if eliminate {
                        return Ok((TypedValue::new(*primitive::UNIT, Value::Unit), builder.0));
                    }

                    let (v, f) = Node::compile(expr, None, res, builders, builder)?;
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
            Kind::Assign => {
                let expr = this.children[0].unwrap();
                let (addr, f) =
                    Node::compile(expr, None, res, builders, PointerBuilder(builder.0))?;
                builder = ValueBuilder(f);

                let expr = this.children[1].unwrap();
                let (v, f) = Node::compile(expr, None, res, builders, builder)?;
                builder = ValueBuilder(f);

                let builder = builder.0.build_store(addr, v);

                (v, builder)
            }
            Kind::Tuple => {
                let mut result = Elems::new();
                for expr in &this.children {
                    let (v, f) = Node::compile(expr.unwrap(), None, res, builders, builder)?;
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
            Kind::Argument => {
                Node::compile(this.children[1].unwrap(), None, res, builders, builder)?
            }
            Kind::Declaration => {
                Node::compile(this.children[0].unwrap(), None, res, builders, builder)?
            }
            Kind::Array => {
                let mut result = Elems::new();
                for expr in &this.children {
                    let (v, f) = Node::compile(expr.unwrap(), None, res, builders, builder)?;
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
            Kind::Dotted => {
                let value = this.children[0].unwrap();
                let value = res.get(value).unwrap();
                let typ = value.type_of.unwrap();
                let b = PointerBuilder(builder.0);
                let (v, f) = Node::compile(value.id(), None, res, builders, b)?;
                builder = ValueBuilder(f);

                let ident = res.get::<Node>(this.children[1].unwrap()).unwrap();
                let ident = match ident.kind {
                    Kind::Nil => match ident.payload.unwrap() {
                        Payload::Identifier(ident) => ident,
                        _ => todo!(),
                    },
                    _ => todo!(),
                };
                let name = res.get(ident).unwrap();

                let index = typ.index_of(&builder.0.builder.res, name.as_str()).unwrap();
                let typ = match typ.concrete {
                    NonConcrete::Type(handle) => {
                        let t = builder.0.builder.res.get::<NamedType>(handle).unwrap();
                        match t.t {
                            Type::Struct { ref fields } => *fields.nth(index).unwrap(),
                            _ => todo!(),
                        }
                    }
                    _ => todo!(),
                };

                let ptr_typ = {
                    let handle = Handle::new();
                    let t = NamedType {
                        name: None,
                        t: Type::Pointer(typ),
                    };
                    builder.0.builder.res.insert(handle, t);
                    TypeId {
                        group: TypeGroup::Pointer,
                        concrete: NonConcrete::Type(handle),
                    }
                };

                let mut value = TypedValue::new(*primitive::UNIT, Value::Unit);
                let mut load = TypedValue::new(*primitive::UNIT, Value::Unit);
                let offset = TypedValue::new(*primitive::UINT, Value::Uint(index as _));
                let index = TypedValue::new(*primitive::UINT, Value::Uint(0));
                let b = builder
                    .0
                    .build_gep(&mut value, ptr_typ, v, index, offset)
                    .build_load(&mut load, value);
                (load, b)
            }
            Kind::Index => todo!(),
            Kind::With => todo!(),
        };
        Ok(value)
    }
}

impl<'a> Compile<PointerBuilder<'a>> for Node {
    type Output = (TypedValue, FunctionBuilder<'a>);

    fn compile(
        handle: Handle<Self>,
        _name: Option<&str>,
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
        builders: &Resources<&BuilderMacro>,
        mut builder: PointerBuilder<'a>,
    ) -> Fallible<Self::Output> {
        // PERF: can we avoid this clone?
        let this = res.get::<Node>(handle).unwrap().as_ref().clone();
        match this.kind {
            Kind::Nil => match this.payload.unwrap() {
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
                    Ok((addr, builder.0))
                }
                _ => panic!("can't assign to something not an identifier"),
            },
            Kind::Dotted => {
                let value = this.children[0].unwrap();
                let value = res.get(value).unwrap();
                let typ = value.type_of.unwrap();
                let b = PointerBuilder(builder.0);
                let (v, f) = Node::compile(value.id(), None, res, builders, b)?;
                builder = PointerBuilder(f);

                let ident = res.get::<Node>(this.children[1].unwrap()).unwrap();
                let ident = match ident.kind {
                    Kind::Nil => match ident.payload.unwrap() {
                        Payload::Identifier(ident) => ident,
                        _ => todo!(),
                    },
                    _ => todo!(),
                };
                let name = res.get(ident).unwrap();

                let index = typ.index_of(&builder.0.builder.res, name.as_str()).unwrap();
                let typ = match typ.concrete {
                    NonConcrete::Type(handle) => {
                        let t = builder.0.builder.res.get::<NamedType>(handle).unwrap();
                        match t.t {
                            Type::Struct { ref fields } => *fields.nth(index).unwrap(),
                            _ => todo!(),
                        }
                    }
                    _ => todo!(),
                };

                let ptr_typ = {
                    let handle = Handle::new();
                    let t = NamedType {
                        name: None,
                        t: Type::Pointer(typ),
                    };
                    builder.0.builder.res.insert(handle, t);
                    TypeId {
                        group: TypeGroup::Pointer,
                        concrete: NonConcrete::Type(handle),
                    }
                };

                let mut value = TypedValue::new(*primitive::UNIT, Value::Unit);
                let offset = TypedValue::new(*primitive::UINT, Value::Uint(index as _));
                let index = TypedValue::new(*primitive::UINT, Value::Uint(0));
                let b = builder.0.build_gep(&mut value, ptr_typ, v, index, offset);
                Ok((value, b))
            }
            _ => panic!("can't assign to something not an identifier"),
        }
    }
}
