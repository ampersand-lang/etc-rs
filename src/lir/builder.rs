use smallvec::{smallvec, SmallVec};
use hashbrown::HashMap;

use crate::lir::context::*;
use crate::assets::Resources;
use crate::types::NamedType;

use super::*;

#[derive(Debug, Clone, Copy)]
pub struct ValueBuilder;

pub struct Builder<'a> {
    pub(crate) res: Resources<&'a NamedType>,
    pub(crate) ctx: ExecutionContext,
}

impl<'a> Builder<'a> {
    pub fn build(self) -> (Resources<&'a NamedType>, ExecutionContext) {
        (self.res, self.ctx)
    }

    pub fn function(self) -> FunctionBuilder<'a> {
        FunctionBuilder {
            builder: self,
            counter: Vec::new(),
            stack_ptr: 0,
            param_types: None,
            result_type: None,
            body: Vec::new(),
        }
    }
}

pub struct FunctionBuilder<'a> {
    builder: Builder<'a>,
    counter: Vec<Binding>,
    stack_ptr: u64,
    param_types: Option<SmallVec<[TypeId; 4]>>,
    result_type: Option<TypeId>,
    body: Vec<Ir>,
}

impl<'a> FunctionBuilder<'a> {
    pub fn build(self) -> Builder<'a> {
        let mut builder = self.builder;
        builder.ctx.data.push(Global::Function(Function {
            param_types: self.param_types.expect("no parameter types given"),
            result_type: self.result_type.expect("no result type given"),
            body: self.body,
        }));
        builder
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
        self.builder.ctx.bindings.current_mut().bindings.insert(binding.1, addr);
        *out = Value::Address(self.builder.ctx.to_virtual(addr));
        self.stack_ptr += t.type_info(&self.builder.res, &self.builder.ctx.target).size as u64 * n;
        self
    }
}
