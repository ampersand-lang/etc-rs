use hashbrown::HashSet;
use smallvec::{smallvec, SmallVec};

use crate::assets::Resources;
use crate::ast;
use crate::lir::context::*;
use crate::lir::repr::*;
use crate::types::{primitive, NamedType};

use super::*;

pub struct ValueBuilder<'a>(pub(crate) FunctionBuilder<'a>);
pub struct PointerBuilder<'a>(pub(crate) FunctionBuilder<'a>);

pub struct Builder<'a> {
    pub(crate) res: Resources<&'a mut NamedType>,
    pub(crate) ctx: ExecutionContext,
    pub(crate) inlined: HashSet<String>,
    pub(crate) name: Option<String>,
    pub(crate) attributes: Option<SmallVec<[ast::Attribute; 4]>>,
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
            attributes: SmallVec::new(),
            param_types: SmallVec::new(),
            result_type: None,
            block_map: HashMap::new(),
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

pub struct BasicBlockBuilder {
    body: Vec<Ir>,
}

pub struct FunctionBuilder<'a> {
    pub(crate) builder: Builder<'a>,
    counter: BindingPrototype,
    name: String,
    attributes: SmallVec<[Attribute; 4]>,
    param_types: SmallVec<[TypeId; 4]>,
    result_type: Option<TypeId>,
    block_map: HashMap<BasicBlock, BasicBlockBuilder>,
    blocks: Vec<BasicBlockPrototype>,
    is_ref: HashSet<BindingPrototype>,
    current_block: Option<BasicBlock>,
    block_count: u32,
}

impl<'a> FunctionBuilder<'a> {
    pub fn build(mut self, idx: &mut FuncId) -> Builder<'a> {
        let mut builder = self.builder;
        idx.idx = builder.ctx.text.len() as u32;
        let mut body = Vec::new();
        let mut ip = 0;
        for block in &mut self.blocks {
            block.start = ip;
            let mut block = self
                .block_map
                .remove(&BasicBlock::new(block.number))
                .unwrap();
            for (idx, ir) in block.body.iter_mut().enumerate() {
                ir.life.position = idx as _;
                ip += 1;
            }
            body.extend(block.body);
        }
        builder.ctx.text.push(Function {
            name: self.name,
            attributes: self.attributes,
            param_types: self.param_types,
            result_type: self.result_type.expect("no result type given"),
            body,
            blocks: self.blocks,
            is_ref: self.is_ref,
        });
        builder
    }

    pub fn inline(
        mut self,
        out: &mut TypedValue,
        other: Function,
        args: SmallVec<[TypedValue; 4]>,
    ) -> Self {
        for (ip, mut ir) in other.body.into_iter().enumerate() {
            if ip > 0 {
                for block in &other.blocks {
                    if block.start == ip {
                        let mut bb = BasicBlock::new(block.number);
                        self = self.add_basic_block(&mut bb).set_basic_block_as_current(bb);
                    }
                }
            }
            for arg in &mut ir.args {
                match arg.val {
                    Value::Arg(idx) => *arg = args[idx as usize].clone(),
                    _ => {}
                }
            }
            match ir.instr {
                Instruction::Return => {
                    *out = ir.args[0];
                    return self;
                }
                _ => {}
            }
            self.block_map
                .get_mut(self.current_block.as_ref().unwrap())
                .unwrap()
                .body
                .push(ir.clone());
        }
        self
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
        *out = bb;
        self.block_map
            .insert(bb, BasicBlockBuilder { body: Vec::new() });
        self.blocks.push(BasicBlockPrototype::new(self.block_count));
        self.block_count += 1;
        self
    }

    pub fn set_basic_block_as_current(mut self, bb: BasicBlock) -> Self {
        self.current_block = Some(bb);
        self
    }

    pub fn attribute(mut self, attr: Attribute) -> Self {
        self.attributes.push(attr);
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
                concrete: NonConcrete::Type(handle),
            }
        };

        self.block_map
            .get_mut(self.current_block.as_ref().unwrap())
            .unwrap()
            .body
            .push(Ir::new(
                self.current_block.expect("no current block started"),
                binding,
                ptr_t,
                Instruction::Alloca,
                smallvec![
                    TypedValue::new(*primitive::TYPE, Value::Type(t)),
                    TypedValue::new(*primitive::UINT, Value::Uint(n))
                ],
            ));
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
        self.block_map
            .get_mut(self.current_block.as_ref().unwrap())
            .unwrap()
            .body
            .push(Ir::new_void(
                self.current_block.expect("no current block started"),
                Instruction::Store,
                smallvec![addr, v],
            ));
        self
    }

    pub fn build_load(mut self, out: &mut TypedValue, v: TypedValue) -> Self {
        let counter = &mut self.counter;
        let binding = *counter;
        counter.inc();

        let unptr_t = match v.typ.concrete {
            NonConcrete::Type(handle) => match self.builder.res.get(handle).unwrap().t {
                Type::Pointer(t) => t,
                _ => todo!(),
            },
            _ => todo!(),
        };

        self.block_map
            .get_mut(self.current_block.as_ref().unwrap())
            .unwrap()
            .body
            .push(Ir::new(
                self.current_block.expect("no current block started"),
                binding,
                unptr_t,
                Instruction::Load,
                smallvec![v],
            ));
        *out = TypedValue::new(unptr_t, Value::Register(binding));
        self
    }

    pub fn build_newnode(mut self, out: &mut TypedValue, args: SmallVec<[TypedValue; 4]>) -> Self {
        let counter = &mut self.counter;
        let binding = *counter;
        counter.inc();
        self.block_map
            .get_mut(self.current_block.as_ref().unwrap())
            .unwrap()
            .body
            .push(Ir::new(
                self.current_block.expect("no current block started"),
                binding,
                *primitive::NODE,
                Instruction::NewNode,
                args,
            ));
        *out = TypedValue::new(*primitive::NODE, Value::Register(binding));
        self
    }

    pub fn build_call(mut self, out: &mut TypedValue, args: SmallVec<[TypedValue; 4]>) -> Self {
        let counter = &mut self.counter;
        let binding = *counter;
        counter.inc();

        let t = match args[0].typ.concrete {
            NonConcrete::Type(handle) => match self.builder.res.get(handle).unwrap().t {
                Type::Function { result_type: t, .. } => t,
                _ => todo!(),
            },
            _ => todo!(),
        };

        self.block_map
            .get_mut(self.current_block.as_ref().unwrap())
            .unwrap()
            .body
            .push(Ir::new(
                self.current_block.expect("no current block started"),
                binding,
                t,
                Instruction::Call,
                args,
            ));
        *out = TypedValue::new(t, Value::Register(binding));
        self
    }

    pub fn build_return(mut self, v: TypedValue) -> Self {
        self.block_map
            .get_mut(self.current_block.as_ref().unwrap())
            .unwrap()
            .body
            .push(Ir::new_void(
                self.current_block.expect("no current block started"),
                Instruction::Return,
                smallvec![v],
            ));
        self
    }

    pub fn build_gep(
        mut self,
        out: &mut TypedValue,
        typ: TypeId,
        ptr: TypedValue,
        index: TypedValue,
        offset: TypedValue,
    ) -> Self {
        let counter = &mut self.counter;
        let binding = *counter;
        counter.inc();

        self.block_map
            .get_mut(self.current_block.as_ref().unwrap())
            .unwrap()
            .body
            .push(Ir::new(
                self.current_block.expect("no current block started"),
                binding,
                typ,
                Instruction::GetElementPtr,
                smallvec![ptr, index, offset],
            ));
        *out = TypedValue::new(typ, Value::Register(binding));
        self
    }

    pub fn build_add(
        mut self,
        out: &mut TypedValue,
        typ: TypeId,
        a: TypedValue,
        b: TypedValue,
    ) -> Self {
        let counter = &mut self.counter;
        let binding = *counter;
        counter.inc();

        self.block_map
            .get_mut(self.current_block.as_ref().unwrap())
            .unwrap()
            .body
            .push(Ir::new(
                self.current_block.expect("no current block started"),
                binding,
                typ,
                Instruction::Add,
                smallvec![a, b],
            ));
        *out = TypedValue::new(typ, Value::Register(binding));
        self
    }

    pub fn build_sub(
        mut self,
        out: &mut TypedValue,
        typ: TypeId,
        a: TypedValue,
        b: TypedValue,
    ) -> Self {
        let counter = &mut self.counter;
        let binding = *counter;
        counter.inc();

        self.block_map
            .get_mut(self.current_block.as_ref().unwrap())
            .unwrap()
            .body
            .push(Ir::new(
                self.current_block.expect("no current block started"),
                binding,
                typ,
                Instruction::Sub,
                smallvec![a, b],
            ));
        *out = TypedValue::new(typ, Value::Register(binding));
        self
    }

    pub fn build_mul(
        mut self,
        out: &mut TypedValue,
        typ: TypeId,
        a: TypedValue,
        b: TypedValue,
    ) -> Self {
        let counter = &mut self.counter;
        let binding = *counter;
        counter.inc();

        self.block_map
            .get_mut(self.current_block.as_ref().unwrap())
            .unwrap()
            .body
            .push(Ir::new(
                self.current_block.expect("no current block started"),
                binding,
                typ,
                Instruction::Mul,
                smallvec![a, b],
            ));
        *out = TypedValue::new(typ, Value::Register(binding));
        self
    }

    pub fn build_div(
        mut self,
        out: &mut TypedValue,
        typ: TypeId,
        a: TypedValue,
        b: TypedValue,
    ) -> Self {
        let counter = &mut self.counter;
        let binding = *counter;
        counter.inc();

        self.block_map
            .get_mut(self.current_block.as_ref().unwrap())
            .unwrap()
            .body
            .push(Ir::new(
                self.current_block.expect("no current block started"),
                binding,
                typ,
                Instruction::Div,
                smallvec![a, b],
            ));
        *out = TypedValue::new(typ, Value::Register(binding));
        self
    }

    pub fn build_rem(
        mut self,
        out: &mut TypedValue,
        typ: TypeId,
        a: TypedValue,
        b: TypedValue,
    ) -> Self {
        let counter = &mut self.counter;
        let binding = *counter;
        counter.inc();

        self.block_map
            .get_mut(self.current_block.as_ref().unwrap())
            .unwrap()
            .body
            .push(Ir::new(
                self.current_block.expect("no current block started"),
                binding,
                typ,
                Instruction::Rem,
                smallvec![a, b],
            ));
        *out = TypedValue::new(typ, Value::Register(binding));
        self
    }

    pub fn build_bit_and(
        mut self,
        out: &mut TypedValue,
        typ: TypeId,
        a: TypedValue,
        b: TypedValue,
    ) -> Self {
        let counter = &mut self.counter;
        let binding = *counter;
        counter.inc();

        self.block_map
            .get_mut(self.current_block.as_ref().unwrap())
            .unwrap()
            .body
            .push(Ir::new(
                self.current_block.expect("no current block started"),
                binding,
                typ,
                Instruction::BitAnd,
                smallvec![a, b],
            ));
        *out = TypedValue::new(typ, Value::Register(binding));
        self
    }

    pub fn build_bit_or(
        mut self,
        out: &mut TypedValue,
        typ: TypeId,
        a: TypedValue,
        b: TypedValue,
    ) -> Self {
        let counter = &mut self.counter;
        let binding = *counter;
        counter.inc();

        self.block_map
            .get_mut(self.current_block.as_ref().unwrap())
            .unwrap()
            .body
            .push(Ir::new(
                self.current_block.expect("no current block started"),
                binding,
                typ,
                Instruction::BitOr,
                smallvec![a, b],
            ));
        *out = TypedValue::new(typ, Value::Register(binding));
        self
    }

    pub fn build_bit_xor(
        mut self,
        out: &mut TypedValue,
        typ: TypeId,
        a: TypedValue,
        b: TypedValue,
    ) -> Self {
        let counter = &mut self.counter;
        let binding = *counter;
        counter.inc();

        self.block_map
            .get_mut(self.current_block.as_ref().unwrap())
            .unwrap()
            .body
            .push(Ir::new(
                self.current_block.expect("no current block started"),
                binding,
                typ,
                Instruction::BitXor,
                smallvec![a, b],
            ));
        *out = TypedValue::new(typ, Value::Register(binding));
        self
    }

    pub fn build_icmp(
        mut self,
        out: &mut TypedValue,
        cmp: u8,
        a: TypedValue,
        b: TypedValue,
    ) -> Self {
        assert!(matches!(cmp, 0..=5), "icmp not in [0; 6)!");
        let counter = &mut self.counter;
        let binding = *counter;
        counter.inc();

        self.block_map
            .get_mut(self.current_block.as_ref().unwrap())
            .unwrap()
            .body
            .push(Ir::new(
                self.current_block.expect("no current block started"),
                binding,
                *primitive::BOOL,
                Instruction::Icmp,
                smallvec![TypedValue::new(*primitive::U8, Value::Uint(cmp as _)), a, b],
            ));
        *out = TypedValue::new(*primitive::BOOL, Value::Register(binding));
        self
    }

    pub fn build_copy(
        mut self,
        out: &mut TypedValue,
        typ: TypeId,
        constraint: RegisterConstraint,
        source: TypedValue,
    ) -> Self {
        let counter = &mut self.counter;
        let binding = *counter;
        counter.inc();

        self.block_map
            .get_mut(self.current_block.as_ref().unwrap())
            .unwrap()
            .body
            .push(
                Ir::new(
                    self.current_block.expect("no current block started"),
                    binding,
                    typ,
                    Instruction::Copy,
                    smallvec![source],
                )
                .with_constraint(constraint),
            );
        *out = TypedValue::new(typ, Value::Register(binding));
        self
    }

    pub fn build_phi(
        mut self,
        out: &mut TypedValue,
        typ: TypeId,
        blocks: SmallVec<[(TypedValue, BasicBlock); 4]>,
    ) -> Self {
        let counter = &mut self.counter;
        let binding = *counter;
        counter.inc();

        let mut args = SmallVec::new();
        for (val, block) in blocks {
            args.push(val);
            args.push(TypedValue::new(*primitive::UINT, Value::Label(block)));
        }

        self.block_map
            .get_mut(self.current_block.as_ref().unwrap())
            .unwrap()
            .body
            .push(Ir::new(
                self.current_block.expect("no current block started"),
                binding,
                typ,
                Instruction::Phi,
                args,
            ));
        *out = TypedValue::new(typ, Value::Register(binding));
        self
    }

    pub fn build_br(mut self, block: BasicBlock) -> Self {
        self.block_map
            .get_mut(self.current_block.as_ref().unwrap())
            .unwrap()
            .body
            .push(Ir::new_void(
                self.current_block.expect("no current block started"),
                Instruction::Br,
                smallvec![TypedValue::new(*primitive::UINT, Value::Label(block))],
            ));
        self
    }

    pub fn build_cond_br(mut self, p: TypedValue, b0: BasicBlock, b1: BasicBlock) -> Self {
        self.block_map
            .get_mut(self.current_block.as_ref().unwrap())
            .unwrap()
            .body
            .push(Ir::new_void(
                self.current_block.expect("no current block started"),
                Instruction::CondBr,
                smallvec![
                    p,
                    TypedValue::new(*primitive::UINT, Value::Label(b0)),
                    TypedValue::new(*primitive::UINT, Value::Label(b1)),
                ],
            ));
        self
    }
}
