use hashbrown::HashSet;
use smallvec::{smallvec, SmallVec};

use crate::assets::Resources;
use crate::lir::codegen::Lifetime;
use crate::lir::context::*;
use crate::lir::repr::*;
use crate::types::{primitive, NamedType};

use super::*;

pub struct ValueBuilder<'a>(pub(crate) FunctionBuilder<'a>);

pub struct Builder<'a> {
    pub(crate) res: Resources<&'a mut NamedType>,
    pub(crate) ctx: ExecutionContext,
}

impl<'a> Builder<'a> {
    pub fn build(self) -> (Resources<&'a mut NamedType>, ExecutionContext) {
        (self.res, self.ctx)
    }

    pub fn function<S: Into<String>>(self, name: S) -> FunctionBuilder<'a> {
        FunctionBuilder {
            builder: self,
            counter: BindingPrototype::new(0, 0),
            name: name.into(),
            param_types: SmallVec::new(),
            result_type: None,
            body: Vec::new(),
            blocks: Vec::new(),
            is_ref: HashSet::new(),
            current_block: None,
            block_count: 0,
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
    name: String,
    param_types: SmallVec<[TypeId; 4]>,
    result_type: Option<TypeId>,
    body: Vec<Ir>,
    blocks: Vec<BasicBlockPrototype>,
    is_ref: HashSet<BindingPrototype>,
    current_block: Option<BasicBlock>,
    block_count: u32,
}

impl<'a> FunctionBuilder<'a> {
    pub fn build(mut self, idx: &mut FuncId) -> Builder<'a> {
        let mut builder = self.builder;
        *idx = builder.ctx.text.len();
        for (idx, ir) in self.body.iter_mut().enumerate() {
            ir.life.position = idx as _;
        }
        builder.ctx.text.push(Function {
            name: self.name,
            param_types: self.param_types,
            result_type: self.result_type.expect("no result type given"),
            body: self.body,
            blocks: self.blocks,
            is_ref: self.is_ref,
        });
        builder
    }

    pub fn parameter(mut self, out: &mut TypedValue, t: TypeId) -> Self {
        *out = TypedValue::new(t, Value::Arg(self.param_types.len() as _));
        self.param_types.push(t);
        self
    }

    pub fn result(mut self, t: TypeId) -> Self {
        self.result_type = Some(t);
        self
    }

    pub fn add_basic_block(mut self, out: &mut BasicBlock) -> Self {
        let bb = BasicBlock::new(self.block_count as u32);
        self.current_block = Some(bb);
        *out = bb;
        self.blocks.push(BasicBlockPrototype::new(self.block_count));
        self.block_count += 1;
        self
    }

    pub fn build_alloca(mut self, out: &mut TypedValue, t: TypeId, n: u64) -> Self {
        let counter = &mut self.counter;
        let binding = *counter;
        counter.inc();

        let ptr_t = {
            let handle = Handle::new();
            let t = NamedType {
                name: None,
                t: Type::Pointer(t),
            };
            self.builder.res.insert(handle, t);
            TypeId {
                group: TypeGroup::Pointer,
                concrete: TypeOrPlaceholder::Type(handle),
            }
        };
        
        self.body.push(Ir {
            binding: Some(binding),
            life: Lifetime::empty(self.current_block.expect("no current block started")),
            instr: Instruction::Alloca,
            args: smallvec![TypedValue::new(*primitive::TYPE, Value::Type(t)), TypedValue::new(t, Value::Uint(n))],
            typ: ptr_t,
        });
        *out = TypedValue::new(ptr_t, Value::Register(binding));
        self
    }

    pub fn build_store(mut self, addr: TypedValue, v: TypedValue) -> Self {
        match addr.val {
            Value::Register(r) => {
                self.is_ref.insert(r);
            }
            _ => {}
        }
        self.body.push(Ir {
            binding: None,
            life: Lifetime::empty(self.current_block.expect("no current block started")),
            instr: Instruction::Store,
            args: smallvec![addr, v],
            typ: *primitive::UNIT,
        });
        self
    }

    pub fn build_load(mut self, out: &mut TypedValue, v: TypedValue) -> Self {
        let counter = &mut self.counter;
        let binding = *counter;
        counter.inc();

        let unptr_t = match v.typ.concrete {
            TypeOrPlaceholder::Type(handle) => {
                match self.builder.res.get(handle).unwrap().t {
                    Type::Pointer(t) => t,
                    _ => todo!(),
                }
            }
            _ => todo!(),
        };
        
        self.body.push(Ir {
            binding: Some(binding),
            life: Lifetime::empty(self.current_block.expect("no current block started")),
            instr: Instruction::Load,
            args: smallvec![v],
            typ: unptr_t,
        });
        *out = TypedValue::new(unptr_t, Value::Register(binding));
        self
    }

    pub fn build_newnode(mut self, out: &mut TypedValue, args: SmallVec<[TypedValue; 4]>) -> Self {
        let counter = &mut self.counter;
        let binding = *counter;
        counter.inc();
        self.body.push(Ir {
            binding: Some(binding),
            life: Lifetime::empty(self.current_block.expect("no current block started")),
            instr: Instruction::NewNode,
            args,
            typ: *primitive::NODE,
        });
        *out = TypedValue::new(*primitive::NODE, Value::Register(binding));
        self
    }

    pub fn build_call(
        mut self,
        out: &mut TypedValue,
        args: SmallVec<[TypedValue; 4]>,
    ) -> Self {
        let counter = &mut self.counter;
        let binding = *counter;
        counter.inc();

        let t = match args[0].typ.concrete {
            TypeOrPlaceholder::Type(handle) => {
                match self.builder.res.get(handle).unwrap().t {
                    Type::Function { result_type: t, .. } => t,
                    _ => todo!(),
                }
            }
            _ => todo!(),
        };

        self.body.push(Ir {
            binding: Some(binding),
            life: Lifetime::empty(self.current_block.expect("no current block started")),
            instr: Instruction::Call,
            args,
            typ: t,
        });
        *out = TypedValue::new(t, Value::Register(binding));
        self
    }

    pub fn build_return(mut self, v: TypedValue) -> Self {
        self.body.push(Ir {
            binding: None,
            life: Lifetime::empty(self.current_block.expect("no current block started")),
            instr: Instruction::Return,
            args: smallvec![v],
            typ: *primitive::UNIT,
        });
        self
    }
}
