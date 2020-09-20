use hashbrown::HashMap;
use smallvec::{smallvec, SmallVec};

use crate::assets::Resources;
use crate::ast::Kind;
use crate::lir::repr::*;
use crate::lir::context::*;
use crate::types::NamedType;

use super::*;

pub struct ValueBuilder<'a>(pub(crate) FunctionBuilder<'a>);

pub struct Builder<'a> {
    pub(crate) res: Resources<&'a NamedType>,
    pub(crate) ctx: ExecutionContext,
}

impl<'a> Builder<'a> {
    pub fn build(self) -> (Resources<&'a NamedType>, ExecutionContext) {
        (self.res, self.ctx)
    }

    pub fn function<S: Into<String>>(self, name: S) -> FunctionBuilder<'a> {
        FunctionBuilder {
            builder: self,
            counter: BindingPrototype::new(0, 0),
            stack_ptr: 0,
            name: name.into(),
            param_types: SmallVec::new(),
            result_type: None,
            body: Vec::new(),
        }
    }

    pub fn add_global<T: Repr + ?Sized>(mut self, out: &mut VirtualAddress, bytes: &T) -> Self {
        let addr = PhysicalAddress(Section::Data, self.ctx.data.len() as _);
        *out = self.ctx.to_virtual(addr);
        self.ctx.data.extend(bytes.to_bytes());
        self
    }
}

pub struct FunctionBuilder<'a> {
    pub(crate) builder: Builder<'a>,
    counter: BindingPrototype,
    stack_ptr: u64,
    name: String,
    param_types: SmallVec<[TypeId; 4]>,
    result_type: Option<TypeId>,
    body: Vec<Ir>,
}

impl<'a> FunctionBuilder<'a> {
    pub fn build(self, idx: &mut FuncId) -> Builder<'a> {
        let mut builder = self.builder;
        *idx = builder.ctx.data.len();
        builder.ctx.text.push(Function {
            name: self.name,
            param_types: self.param_types,
            result_type: self.result_type.expect("no result type given"),
            body: self.body,
        });
        builder
    }

    pub fn parameter(mut self, t: TypeId) -> Self {
        self.param_types.push(t);
        self
    }

    pub fn result(mut self, t: TypeId) -> Self {
        self.result_type = Some(t);
        self
    }

    pub fn build_alloca(mut self, out: &mut Value, t: TypeId, n: u64) -> Self {
        let counter = &mut self.counter;
        let binding = *counter;
        counter.inc();
        self.body.push(Ir {
            binding: Some(binding),
            instr: Instruction::Alloca,
            args: smallvec![Value::Type(t), Value::Uint(n)],
        });
        *out = Value::Register(binding);
        self
    }

    pub fn build_store(mut self, t: TypeId, addr: Value, v: Value) -> Self {
        self.body.push(Ir {
            binding: None,
            instr: Instruction::Store,
            args: smallvec![Value::Type(t), addr, v],
        });
        self
    }

    pub fn build_load(mut self, out: &mut Value, t: TypeId, v: Value) -> Self {
        let counter = &mut self.counter;
        let binding = *counter;
        counter.inc();
        self.body.push(Ir {
            binding: Some(binding),
            instr: Instruction::Load,
            args: smallvec![Value::Type(t), v],
        });
        *out = Value::Register(binding);
        self
    }

    pub fn build_newnode(mut self, out: &mut Value, args: SmallVec<[Value; 4]>) -> Self {
        let counter = &mut self.counter;
        let binding = *counter;
        counter.inc();
        self.body.push(Ir {
            binding: Some(binding),
            instr: Instruction::NewNode,
            args,
        });
        *out = Value::Register(binding);
        self
    }

    pub fn build_call(mut self, out: &mut Value, t: TypeId, mut args: SmallVec<[Value; 4]>) -> Self {
        let counter = &mut self.counter;
        let binding = *counter;
        counter.inc();
        args.insert(0, Value::Type(t));
        self.body.push(Ir {
            binding: Some(binding),
            instr: Instruction::Call,
            args,
        });
        *out = Value::Register(binding);
        self
    }

    pub fn build_return(mut self, t: TypeId, v: Value) -> Self {
        self.body.push(Ir {
            binding: None,
            instr: Instruction::Return,
            args: smallvec![Value::Type(t), v],
        });
        self
    }
}
