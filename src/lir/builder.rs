use hashbrown::HashMap;
use smallvec::{smallvec, SmallVec};

use crate::assets::Resources;
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

    pub fn function(self) -> FunctionBuilder<'a> {
        let f = FunctionBuilder {
            builder: self,
            counter: Vec::new(),
            stack_ptr: 0,
            param_types: SmallVec::new(),
            result_type: None,
            body: Vec::new(),
        };
        f.begin()
    }
}

pub struct FunctionBuilder<'a> {
    pub(crate) builder: Builder<'a>,
    counter: Vec<Binding>,
    stack_ptr: u64,
    param_types: SmallVec<[TypeId; 4]>,
    result_type: Option<TypeId>,
    body: Vec<Ir>,
}

impl<'a> FunctionBuilder<'a> {
    pub fn build(self, idx: &mut GlobId) -> Builder<'a> {
        let mut builder = self.builder;
        *idx = builder.ctx.data.len();
        builder.ctx.data.push(Global::Function(Function {
            param_types: self.param_types,
            result_type: self.result_type.expect("no result type given"),
            body: self.body,
        }));
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

    pub fn begin(mut self) -> Self {
        let parent = self.builder.ctx.bindings.curr.last().copied();
        let next = self.builder.ctx.bindings.list.len();
        self.builder.ctx.bindings.curr.push(next);
        self.builder.ctx.bindings.list.push(Scope {
            parent,
            base_ptr: self.stack_ptr,
            bindings: HashMap::new(),
        });
        self.counter.push(Binding(next, 0));

        self.body.push(Ir {
            binding: None,
            instr: Instruction::Begin,
            args: smallvec![Value::Uint(next as _)],
        });

        self
    }

    pub fn end(mut self) -> Self {
        let scope = self.builder.ctx.bindings.current();
        self.stack_ptr = scope.base_ptr;
        self.builder.ctx.bindings.curr.pop();
        self.counter.pop();

        self.body.push(Ir {
            binding: None,
            instr: Instruction::End,
            args: SmallVec::new(),
        });

        self
    }

    pub fn build_alloca(mut self, out: &mut Value, t: TypeId, n: u64) -> Self {
        let counter = self.counter.last_mut().unwrap();
        let binding = *counter;
        counter.1 += 1;
        self.body.push(Ir {
            binding: Some(binding),
            instr: Instruction::Alloca,
            args: smallvec![Value::Type(t), Value::Uint(n)],
        });
        let addr = self.stack_ptr;
        let addr = PhysicalAddress(Section::Stack, addr as u32);
        self.builder
            .ctx
            .bindings
            .current_mut()
            .bindings
            .insert(binding.1, addr);
        *out = Value::Address(self.builder.ctx.to_virtual(addr));
        self.stack_ptr += t
            .type_info(&self.builder.res, &self.builder.ctx.target)
            .size as u64
            * n;
        self
    }

    pub fn build_store(mut self, t: TypeId, addr: Value, v: Value) -> Self {
        self.body.push(Ir {
            binding: None,
            instr: Instruction::Return,
            args: smallvec![Value::Type(t), addr, v],
        });
        self
    }

    pub fn build_call(mut self, out: &mut Value, t: TypeId, args: SmallVec<[Value; 4]>) -> Self {
        let counter = self.counter.last_mut().unwrap();
        let binding = *counter;
        counter.1 += 1;
        self.body.push(Ir {
            binding: Some(binding),
            instr: Instruction::Call,
            args,
        });
        let addr = self.stack_ptr;
        let addr = PhysicalAddress(Section::Stack, addr as u32);
        self.builder
            .ctx
            .bindings
            .current_mut()
            .bindings
            .insert(binding.1, addr);
        let addr = self.builder.ctx.to_virtual(addr);
        *out = Value::Unref(addr);
        self.stack_ptr += t
            .type_info(&self.builder.res, &self.builder.ctx.target)
            .size as u64;
        self
    }

    pub fn build_return(mut self, v: Value) -> Self {
        self.body.push(Ir {
            binding: None,
            instr: Instruction::Return,
            args: smallvec![v],
        });
        self
    }
}
