use std::convert::{TryFrom, TryInto};
use std::mem;
use std::ops::Range;

use failure::{Fail, Fallible};
use hashbrown::HashMap;
use smallvec::smallvec;

use crate::assets::{LazyUpdate, Resources};
use crate::ast::{self, Node};
use crate::lir::repr::*;
use crate::lir::target::Target;
use crate::types::{NamedType, TypeInfo};

use super::*;
use crate::lir::builder::Builder;

pub mod type_info {
    use lazy_static::lazy_static;

    use crate::ast::NodeId;
    use crate::lir::repr::*;
    use crate::types::{TypeId, TypeInfo};

    use super::VirtualAddress;

    lazy_static! {
        pub static ref TYPE: TypeInfo = TypeId::static_type_info();
        pub static ref POINTER64: TypeInfo = VirtualAddress::static_type_info();
        pub static ref NODE: TypeInfo = NodeId::static_type_info();
    }
}

pub const DEFAULT_STACK_SPACE: usize = 64 * 1024;
pub const DEFAULT_HEAP_SPACE: usize = 1024 * 1024;
pub const DEFAULT_DATA_SPACE: usize = 1024 * 1024;

#[derive(Debug, Fail)]
#[fail(display = "invalid NULL access")]
pub struct NullAccess;

#[derive(Debug, Fail)]
#[fail(display = "access to invalid section")]
pub struct InvalidSectionAccess;

#[derive(Debug, Fail)]
#[fail(display = "type error")]
pub struct TypeError;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Section {
    Data,
    Heap,
    Stack,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct VirtualAddress(pub(crate) u64);

impl Repr for VirtualAddress {
    fn type_info(&self) -> TypeInfo {
        Self::static_type_info()
    }

    fn write_bytes(&self, out: &mut [u8]) {
        out.copy_from_slice(&self.0.to_le_bytes());
    }

    fn copy_from_bytes(&mut self, bytes: &[u8]) {
        if bytes.len() != mem::size_of::<u64>() {
            panic!("attempt to copy from slice of invalid length");
        }
        self.0 = u64::from_le_bytes(bytes.try_into().unwrap());
    }
}

impl ReprExt for VirtualAddress {
    fn static_type_info() -> TypeInfo {
        TypeInfo::new(mem::size_of::<u64>(), mem::align_of::<u64>())
    }

    fn from_bytes(bytes: &[u8]) -> Self {
        if bytes.len() != mem::size_of::<u64>() {
            panic!("attempt to copy from slice of invalid length");
        }
        VirtualAddress(u64::from_le_bytes(bytes.try_into().unwrap()))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct PhysicalAddress(pub(crate) Section, pub(crate) u32);

#[derive(Debug, Clone, Copy)]
pub struct ExecAddress(pub(crate) u32, pub(crate) u32, pub(crate) BindingPrototype);

#[derive(Debug, Clone)]
pub enum Result {
    Continue(u32),
    Return(TypedValue),
}

pub type Reg = SmallVec<[u8; 32]>;

pub struct ContextDisplay<'a> {
    ctx: &'a ExecutionContext,
}

impl<'a> Display for ContextDisplay<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for func in &self.ctx.text {
            writeln!(f, "{}", func)?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct ExecutionContext {
    pub(crate) target: Target,
    pub(crate) main: usize,
    pub(crate) call_stack: Vec<(usize, u64, ExecAddress)>,
    pub(crate) instr_ptr: ExecAddress,
    pub(crate) invocation: u64,
    pub(crate) text: Vec<Function>,
    pub(crate) desc: Vec<Global>,
    pub(crate) data: Vec<u8>,
    pub(crate) heap: Vec<u8>,
    pub(crate) stack: Vec<u8>,
    pub(crate) registers: HashMap<Binding, Reg>,
    pub(crate) arguments: HashMap<Argument, Reg>,
    pub(crate) stack_ptr: usize,
    pub(crate) data_addr: u64,
    pub(crate) heap_addr: u64,
    pub(crate) stack_addr: u64,
}

impl ExecutionContext {
    pub fn new(target: Target) -> Self {
        Self {
            target,
            main: 0,
            call_stack: Vec::new(),
            instr_ptr: ExecAddress(0, 0, BindingPrototype::new(0, 0)),
            invocation: 0,
            text: Vec::new(),
            desc: Vec::new(),
            data: Vec::with_capacity(DEFAULT_DATA_SPACE),
            stack: vec![0; DEFAULT_STACK_SPACE],
            heap: vec![0; DEFAULT_HEAP_SPACE],
            registers: HashMap::new(),
            arguments: HashMap::new(),
            stack_ptr: 0,
            data_addr: 0x100000,
            heap_addr: 0x4000000,
            stack_addr: 0x80000000,
        }
    }

    pub fn builder(res: Resources<&mut NamedType>, target: Target) -> Builder {
        Builder {
            res,
            ctx: Self::new(target),
        }
    }

    pub fn builder_with(ctx: Self, res: Resources<&mut NamedType>) -> Builder {
        Builder { res, ctx }
    }

    pub fn main(&self) -> usize {
        self.main
    }

    pub fn display(&self) -> ContextDisplay<'_> {
        ContextDisplay { ctx: self }
    }

    pub fn iter(&self) -> impl Iterator<Item = &Function> {
        self.text.iter()
    }

    pub fn functions(self) -> impl Iterator<Item = Function> {
        self.text.into_iter()
    }

    pub fn function(&self, id: usize) -> &Function {
        &self.text[id]
    }

    pub fn named_function(&self, name: &str) -> Option<&Function> {
        for func in &self.text {
            if func.name == name {
                return Some(func);
            }
        }
        None
    }

    pub fn named_function_mut(&mut self, name: &str) -> Option<&mut Function> {
        for func in &mut self.text {
            if func.name == name {
                return Some(func);
            }
        }
        None
    }

    pub fn to_physical(&self, addr: VirtualAddress) -> Option<PhysicalAddress> {
        if addr.0 >= self.stack_addr {
            Some(PhysicalAddress(
                Section::Stack,
                (addr.0 - self.stack_addr) as _,
            ))
        } else if addr.0 >= self.heap_addr {
            Some(PhysicalAddress(
                Section::Heap,
                (addr.0 - self.heap_addr) as _,
            ))
        } else if addr.0 >= self.data_addr {
            Some(PhysicalAddress(
                Section::Data,
                (addr.0 - self.data_addr) as _,
            ))
        } else {
            None
        }
    }

    pub fn to_virtual(&self, addr: PhysicalAddress) -> VirtualAddress {
        match addr.0 {
            Section::Data => VirtualAddress(addr.1 as u64 + self.data_addr),
            Section::Heap => VirtualAddress(addr.1 as u64 + self.heap_addr),
            Section::Stack => VirtualAddress(addr.1 as u64 + self.stack_addr),
        }
    }

    pub fn copy_from_virtual_to_virtual(
        &mut self,
        src: VirtualAddress,
        dst: VirtualAddress,
        range: Range<usize>,
    ) -> Fallible<()> {
        let src = self.to_physical(src).ok_or(NullAccess)?;
        let dst = self.to_physical(dst).ok_or(NullAccess)?;
        self.copy_from_physical_to_physical(src, dst, range)
    }

    pub fn copy_from_physical_to_physical(
        &mut self,
        src: PhysicalAddress,
        dst: PhysicalAddress,
        range: Range<usize>,
    ) -> Fallible<()> {
        let src_section = src.0;
        let dst_section = dst.0;
        let src = range.start + src.1 as usize..range.end + src.1 as usize;
        let dst = range.start + dst.1 as usize..range.end + dst.1 as usize;
        match (src_section, dst_section) {
            (Section::Data, Section::Data) => Ok(self.data.copy_within(src, dst.start)),
            (Section::Data, Section::Stack) => Ok(self.stack[dst].copy_from_slice(&self.data[src])),
            (Section::Stack, Section::Data) => Ok(self.data[dst].copy_from_slice(&self.stack[src])),
            (Section::Heap, Section::Data) => Ok(self.data[dst].copy_from_slice(&self.heap[src])),
            (Section::Data, Section::Heap) => Ok(self.heap[dst].copy_from_slice(&self.data[src])),
            (Section::Heap, Section::Heap) => Ok(self.heap.copy_within(src, dst.start)),
            (Section::Heap, Section::Stack) => Ok(self.stack[dst].copy_from_slice(&self.heap[src])),
            (Section::Stack, Section::Heap) => Ok(self.heap[dst].copy_from_slice(&self.stack[src])),
            (Section::Stack, Section::Stack) => Ok(self.stack.copy_within(src, dst.start)),
        }
    }

    pub fn read(&self, addr: VirtualAddress, range: Range<usize>) -> Fallible<&[u8]> {
        let addr = self.to_physical(addr).ok_or(NullAccess)?;
        self.read_physical(addr, range)
    }

    pub fn read_physical(&self, addr: PhysicalAddress, range: Range<usize>) -> Fallible<&[u8]> {
        let range = range.start + addr.1 as usize..range.end + addr.1 as usize;
        match addr.0 {
            Section::Data => Ok(&self.data[range]),
            Section::Heap => Ok(&self.heap[range]),
            Section::Stack => Ok(&self.stack[range]),
        }
    }

    pub fn write(&mut self, addr: VirtualAddress, bytes: &[u8]) -> Fallible<()> {
        let addr = self.to_physical(addr).ok_or(NullAccess)?;
        self.write_physical(addr, bytes)
    }

    pub fn write_physical(&mut self, addr: PhysicalAddress, bytes: &[u8]) -> Fallible<()> {
        let range = addr.1 as usize..bytes.len() + addr.1 as usize;
        match addr.0 {
            Section::Data => Ok(self.data[range].copy_from_slice(bytes)),
            Section::Heap => Ok(self.heap[range].copy_from_slice(bytes)),
            Section::Stack => Ok(self.stack[range].copy_from_slice(bytes)),
        }
    }

    fn execute(
        &mut self,
        _lazy: &mut LazyUpdate,
        foreign: &Resources<&Foreign>,
        res: &mut Resources<(&String, &mut NamedType, &mut Node)>,
        ir: Ir,
    ) -> Fallible<Result> {
        let mut value = SmallVec::new();
        let result = match ir.instr {
            Instruction::Alloca => {
                let t = match ir.args[0].val {
                    Value::Arg(r) => {
                        TypeId::from_bytes(&self.arguments[&Argument::new(r, self.invocation)])
                    }
                    Value::Register(r) => {
                        TypeId::from_bytes(&self.registers[&r.build(self.invocation)])
                    }
                    Value::Type(t) => t,
                    _ => return Err(From::from(TypeError)),
                };
                let t = t.type_info(res, &self.target);
                let n = match ir.args[1].val {
                    Value::Arg(r) => {
                        u64::from_bytes(&self.arguments[&Argument::new(r, self.invocation)])
                    }
                    Value::Register(r) => {
                        u64::from_bytes(&self.registers[&r.build(self.invocation)])
                    }
                    Value::Uint(n) => n,
                    _ => return Err(From::from(TypeError)),
                };
                let bytes = From::from(
                    self.to_virtual(PhysicalAddress(Section::Stack, self.stack_ptr as _))
                        .to_bytes(),
                );
                value = bytes;
                self.stack_ptr += t.size * n as usize;
                Ok(Result::Continue(1))
            }
            Instruction::Store => {
                let t = ir.args[1].typ;
                let t = t.type_info(res, &self.target);
                let ptr = match ir.args[0].val {
                    Value::Arg(r) => VirtualAddress::from_bytes(
                        &self.arguments[&Argument::new(r, self.invocation)],
                    ),
                    Value::Register(r) => {
                        VirtualAddress::from_bytes(&self.registers[&r.build(self.invocation)])
                    }
                    Value::Address(ptr) => ptr,
                    _ => return Err(From::from(TypeError)),
                };
                let mut value = vec![0_u8; t.size];
                match ir.args[1].val {
                    Value::Unit => {}
                    Value::Arg(r) => {
                        value.copy_from_slice(&self.arguments[&Argument::new(r, self.invocation)])
                    }
                    Value::Register(r) => {
                        value.copy_from_slice(&self.registers[&r.build(self.invocation)])
                    }
                    Value::Function(id) => (id as u64).write_bytes(&mut value),
                    Value::Label(_b) => todo!(),
                    Value::Bool(p) => u8::from(p).write_bytes(&mut value),
                    Value::Uint(int) => int.write_bytes(&mut value),
                    Value::Float(flt) => flt.write_bytes(&mut value),
                    Value::Address(addr) => addr.write_bytes(&mut value),
                    Value::Type(typ) => typ.write_bytes(&mut value),
                    Value::Node(node) => node.write_bytes(&mut value),
                    Value::Array(_) => todo!(),
                    Value::Struct(_) => todo!(),
                    Value::Union(_) => todo!(),
                    Value::Tagged(..) => todo!(),
                    Value::Ffi(foreign) => foreign.write_bytes(&mut value),
                }
                self.write(ptr, &value)?;
                Ok(Result::Continue(1))
            }
            Instruction::Load => {
                let t = ir.typ.type_info(res, &self.target);
                let ptr = match ir.args[0].val {
                    Value::Arg(r) => VirtualAddress::from_bytes(
                        &self.arguments[&Argument::new(r, self.invocation)],
                    ),
                    Value::Register(r) => {
                        VirtualAddress::from_bytes(&self.registers[&r.build(self.invocation)])
                    }
                    Value::Address(ptr) => ptr,
                    _ => return Err(From::from(TypeError)),
                };
                let bytes = self
                    .read(ptr, 0..t.size)?
                    .iter()
                    .copied()
                    .collect::<SmallVec<_>>();
                value = bytes;
                Ok(Result::Continue(1))
            }
            Instruction::Call => {
                let t = ir.typ.type_info(res, &self.target);
                let func = match ir.args[0].val {
                    Value::Arg(r) => Some(u64::from_bytes(
                        &self.arguments[&Argument::new(r, self.invocation)],
                    ) as usize),
                    Value::Register(r) => {
                        Some(u64::from_bytes(&self.registers[&r.build(self.invocation)]) as usize)
                    }
                    Value::Function(handle) => Some(handle),
                    Value::Ffi(handle) => {
                        let ffi = foreign.get(handle).unwrap();
                        let result = (ffi.as_ref())(self, res, &ir.args[1..])?;
                        let mut bytes = smallvec![0_u8; t.size];
                        match result.val {
                            Value::Unit => {}
                            Value::Arg(r) => bytes.copy_from_slice(
                                &self.arguments[&Argument::new(r, self.invocation)],
                            ),
                            Value::Register(r) => {
                                bytes.copy_from_slice(&self.registers[&r.build(self.invocation)])
                            }
                            Value::Function(id) => (id as u64).write_bytes(&mut bytes),
                            Value::Label(_b) => todo!(),
                            Value::Bool(p) => u8::from(p).write_bytes(&mut bytes),
                            Value::Uint(int) => int.write_bytes(&mut bytes),
                            Value::Float(flt) => flt.write_bytes(&mut bytes),
                            Value::Address(addr) => addr.write_bytes(&mut bytes),
                            Value::Type(typ) => typ.write_bytes(&mut bytes),
                            Value::Node(node) => node.write_bytes(&mut bytes),
                            Value::Array(_) => todo!(),
                            Value::Struct(_) => todo!(),
                            Value::Union(_) => todo!(),
                            Value::Tagged(..) => todo!(),
                            Value::Ffi(foreign) => foreign.write_bytes(&mut bytes),
                        }
                        value = bytes;
                        None
                    }
                    _ => return Err(From::from(TypeError)),
                };

                if let Some(func) = func {
                    self.call_stack
                        .push((self.stack_ptr, self.invocation, self.instr_ptr));
                    self.instr_ptr = ExecAddress(func as _, 0, ir.binding.unwrap());
                    let new_invocation = rand::random();

                    for (idx, TypedValue { typ, val }) in ir.args[1..].iter().enumerate() {
                        let t = typ.type_info(res, &self.target);

                        let mut value = smallvec![0_u8; t.size];
                        match val {
                            Value::Unit => {}
                            Value::Arg(r) => value.copy_from_slice(
                                &self.arguments[&Argument::new(*r, self.invocation)],
                            ),
                            Value::Register(r) => {
                                value.copy_from_slice(&self.registers[&r.build(self.invocation)])
                            }
                            Value::Function(id) => (*id as u64).write_bytes(&mut value),
                            Value::Label(_b) => todo!(),
                            Value::Bool(p) => u8::from(*p).write_bytes(&mut value),
                            Value::Uint(int) => int.write_bytes(&mut value),
                            Value::Float(flt) => flt.write_bytes(&mut value),
                            Value::Address(addr) => addr.write_bytes(&mut value),
                            Value::Type(typ) => typ.write_bytes(&mut value),
                            Value::Node(node) => node.write_bytes(&mut value),
                            Value::Array(_) => todo!(),
                            Value::Struct(_) => todo!(),
                            Value::Union(_) => todo!(),
                            Value::Tagged(..) => todo!(),
                            Value::Ffi(foreign) => foreign.write_bytes(&mut value),
                        }
                        self.arguments
                            .insert(Argument::new(idx as _, new_invocation), value);
                    }
                    self.invocation = new_invocation;

                    Ok(Result::Continue(0))
                } else {
                    Ok(Result::Continue(1))
                }
            }
            Instruction::NewNode => {
                let n = match ir.args[0].val {
                    Value::Arg(r) => {
                        u8::from_bytes(&self.arguments[&Argument::new(r, self.invocation)])
                    }
                    Value::Register(r) => {
                        u8::from_bytes(&self.registers[&r.build(self.invocation)])
                    }
                    Value::Uint(n) => n as u8,
                    _ => return Err(From::from(TypeError)),
                };
                let typ = ir.args[1].typ;
                let payload = match ir.args[1].val {
                    Value::Arg(r) => {
                        let value = &self.arguments[&Argument::new(r, self.invocation)];
                        let payload = match typ.group {
                            TypeGroup::None => Payload::Unit,
                            TypeGroup::Type => Payload::Type(TypeId::from_bytes(value)),
                            TypeGroup::Node => todo!(),
                            TypeGroup::Int => match value.len() {
                                1 => Payload::Integer(u8::from_bytes(value) as u64),
                                2 => Payload::Integer(u16::from_bytes(value) as u64),
                                4 => Payload::Integer(u32::from_bytes(value) as u64),
                                8 => Payload::Integer(u64::from_bytes(value)),
                                _ => panic!("invalid byte length"),
                            },
                            TypeGroup::Float => match value.len() {
                                4 => Payload::Float(f32::from_bytes(value) as f64),
                                8 => Payload::Float(f64::from_bytes(value)),
                                _ => panic!("invalid byte length"),
                            },
                            TypeGroup::Struct => todo!(),
                            TypeGroup::Tagged => todo!(),
                            TypeGroup::Enum => todo!(),
                            TypeGroup::Union => todo!(),
                            TypeGroup::Function => todo!(),
                            TypeGroup::Pointer => todo!(),
                            TypeGroup::Array => todo!(),
                            TypeGroup::Slice => todo!(),
                        };
                        Some(payload)
                    }
                    Value::Register(r) => {
                        let value = &self.registers[&r.build(self.invocation)];
                        let payload = match typ.group {
                            TypeGroup::None => Payload::Unit,
                            TypeGroup::Type => Payload::Type(TypeId::from_bytes(value)),
                            TypeGroup::Node => todo!(),
                            TypeGroup::Int => match value.len() {
                                1 => Payload::Integer(u8::from_bytes(value) as u64),
                                2 => Payload::Integer(u16::from_bytes(value) as u64),
                                4 => Payload::Integer(u32::from_bytes(value) as u64),
                                8 => Payload::Integer(u64::from_bytes(value)),
                                _ => panic!("invalid byte length"),
                            },
                            TypeGroup::Float => match value.len() {
                                4 => Payload::Float(f32::from_bytes(value) as f64),
                                8 => Payload::Float(f64::from_bytes(value)),
                                _ => panic!("invalid byte length"),
                            },
                            TypeGroup::Struct => todo!(),
                            TypeGroup::Tagged => todo!(),
                            TypeGroup::Enum => todo!(),
                            TypeGroup::Union => todo!(),
                            TypeGroup::Function => todo!(),
                            TypeGroup::Pointer => todo!(),
                            TypeGroup::Array => todo!(),
                            TypeGroup::Slice => todo!(),
                        };
                        Some(payload)
                    }
                    Value::Uint(i) => Some(Payload::Integer(i)),
                    Value::Float(x) => Some(Payload::Float(x)),
                    Value::Type(t) => Some(Payload::Type(t)),
                    Value::Unit => None,
                    Value::Function(id) => Some(Payload::Function(id)),
                    _ => return Err(From::from(TypeError)),
                };
                let kind = ast::Kind::try_from(n).expect("invalid node kind");
                let mut children: SmallVec<[Option<NodeId>; 4]> = SmallVec::new();
                for child in &ir.args[2..] {
                    match child.val {
                        Value::Arg(r) => children.push(Some(NodeId::from_bytes(
                            &self.arguments[&Argument::new(r, self.invocation)],
                        ))),
                        Value::Register(r) => children.push(Some(NodeId::from_bytes(
                            &self.registers[&r.build(self.invocation)],
                        ))),
                        Value::Node(n) => children.push(Some(n)),
                        _ => return Err(From::from(TypeError)),
                    }
                }
                let mut node = Node::new(kind, Handle::nil(), children);
                node.payload = payload;
                let mut bytes = smallvec![0_u8; type_info::NODE.size];
                node.id().write_bytes(&mut bytes);
                res.insert(node.id(), node);
                value = bytes;
                Ok(Result::Continue(1))
            }
            Instruction::Return => {
                if self.call_stack.len() == 1 {
                    Ok(Result::Return(ir.args[0].clone()))
                } else {
                    let t = self.text[self.instr_ptr.0 as usize]
                        .result_type
                        .type_info(res, &self.target);

                    let r = self.instr_ptr.2;
                    let (sp, inv, ip) = self.call_stack.pop().unwrap();
                    let old_invocation = self.invocation;
                    self.stack_ptr = sp;
                    self.instr_ptr = ip;
                    self.invocation = inv;

                    let binding = r.build(self.invocation);
                    let mut value = smallvec![0_u8; t.size];
                    match ir.args[0].val {
                        Value::Unit => {}
                        Value::Arg(r) => value
                            .copy_from_slice(&self.arguments[&Argument::new(r, old_invocation)]),
                        Value::Register(r) => {
                            value.copy_from_slice(&self.registers[&r.build(old_invocation)])
                        }
                        Value::Function(id) => (id as u64).write_bytes(&mut value),
                        Value::Label(_b) => todo!(),
                        Value::Bool(p) => u8::from(p).write_bytes(&mut value),
                        Value::Uint(int) => int.write_bytes(&mut value),
                        Value::Float(flt) => flt.write_bytes(&mut value),
                        Value::Address(addr) => addr.write_bytes(&mut value),
                        Value::Type(typ) => typ.write_bytes(&mut value),
                        Value::Node(node) => node.write_bytes(&mut value),
                        Value::Array(_) => todo!(),
                        Value::Struct(_) => todo!(),
                        Value::Union(_) => todo!(),
                        Value::Tagged(..) => todo!(),
                        Value::Ffi(foreign) => foreign.write_bytes(&mut value),
                    }
                    self.registers.insert(binding, value);
                    Ok(Result::Continue(1))
                }
            }
            Instruction::Jmp => {
                match ir.args[0].val {
                    Value::Label(b) => {
                        let func = &self.text[self.instr_ptr.0 as usize];
                        let b = &func.blocks[b.number() as usize];
                        self.instr_ptr.1 = b.start as _;
                    }
                    _ => return Err(From::from(TypeError)),
                }
                Ok(Result::Continue(0))
            }
            Instruction::IfThenElse => {
                let cond = match ir.args[0].val {
                    Value::Bool(p) => p,
                    Value::Arg(r) => {
                        u8::from_bytes(&self.arguments[&Argument::new(r, self.invocation)]) == 1
                    }
                    Value::Register(r) => {
                        u8::from_bytes(&self.registers[&r.build(self.invocation)]) == 1
                    }
                    _ => return Err(From::from(TypeError)),
                };
                if cond {
                    match ir.args[1].val {
                        Value::Label(b) => {
                            let func = &self.text[self.instr_ptr.0 as usize];
                            let b = &func.blocks[b.number() as usize];
                            self.instr_ptr.1 = b.start as _;
                        }
                        _ => return Err(From::from(TypeError)),
                    }
                } else {
                    match ir.args[2].val {
                        Value::Label(b) => {
                            let func = &self.text[self.instr_ptr.0 as usize];
                            let b = &func.blocks[b.number() as usize];
                            self.instr_ptr.1 = b.start as _;
                        }
                        _ => return Err(From::from(TypeError)),
                    }
                }
                Ok(Result::Continue(0))
            }
        };
        if let Some(r) = ir.binding {
            let binding = r.build(self.invocation);
            self.registers.insert(binding, value);
        }
        result
    }

    fn run(
        &mut self,
        lazy: &mut LazyUpdate,
        foreign: &Resources<&Foreign>,
        res: &mut Resources<(&String, &mut NamedType, &mut Node)>,
    ) -> Fallible<TypedValue> {
        loop {
            let func = &self.text[self.instr_ptr.0 as usize];
            let ir: Ir = func.body[self.instr_ptr.1 as usize].clone();
            match self.execute(lazy, foreign, res, ir)? {
                Result::Continue(inc) => self.instr_ptr.1 += inc,
                Result::Return(value) => return Ok(value),
            }
        }
    }

    pub fn call(
        &mut self,
        lazy: &mut LazyUpdate,
        foreign: &Resources<&Foreign>,
        res: &mut Resources<(&String, &mut NamedType, &mut Node)>,
        f: FuncId,
        args: &[TypedValue],
    ) -> Fallible<TypedValue> {
        self.call_stack
            .push((self.stack_ptr, self.invocation, self.instr_ptr));
        self.invocation = rand::random();
        self.instr_ptr = ExecAddress(f as _, 0, BindingPrototype::new(0, 0));
        for (idx, TypedValue { typ, val }) in args.iter().enumerate() {
            let t = typ.type_info(res, &self.target);

            let mut value = smallvec![0_u8; t.size];
            match val {
                Value::Unit => {}
                Value::Arg(r) => {
                    value.copy_from_slice(&self.arguments[&Argument::new(*r, self.invocation)])
                }
                Value::Register(r) => {
                    value.copy_from_slice(&self.registers[&r.build(self.invocation)])
                }
                Value::Function(id) => (*id as u64).write_bytes(&mut value),
                Value::Label(_b) => todo!(),
                Value::Bool(p) => u8::from(*p).write_bytes(&mut value),
                Value::Uint(int) => int.write_bytes(&mut value),
                Value::Float(flt) => flt.write_bytes(&mut value),
                Value::Address(addr) => addr.write_bytes(&mut value),
                Value::Type(typ) => typ.write_bytes(&mut value),
                Value::Node(node) => node.write_bytes(&mut value),
                Value::Array(_) => todo!(),
                Value::Struct(_) => todo!(),
                Value::Union(_) => todo!(),
                Value::Tagged(..) => todo!(),
                Value::Ffi(foreign) => foreign.write_bytes(&mut value),
            }
            self.arguments
                .insert(Argument::new(idx as _, self.invocation), value);
        }
        let result = self.run(lazy, foreign, res)?;
        let result_value = match result {
            TypedValue {
                typ,
                val: Value::Register(r),
            } => {
                let value = &self.registers[&r.build(self.invocation)];
                match typ.group {
                    TypeGroup::None => Value::Unit,
                    TypeGroup::Type => Value::Type(TypeId::from_bytes(value)),
                    TypeGroup::Node => Value::Node(NodeId::from_bytes(value)),
                    TypeGroup::Int => match value.len() {
                        1 => Value::Uint(u8::from_bytes(value) as u64),
                        2 => Value::Uint(u16::from_bytes(value) as u64),
                        4 => Value::Uint(u32::from_bytes(value) as u64),
                        8 => Value::Uint(u64::from_bytes(value)),
                        _ => panic!("invalid byte length"),
                    },
                    TypeGroup::Float => match value.len() {
                        4 => Value::Float(f32::from_bytes(value) as f64),
                        8 => Value::Float(f64::from_bytes(value)),
                        _ => panic!("invalid byte length"),
                    },
                    TypeGroup::Struct => todo!(),
                    TypeGroup::Tagged => todo!(),
                    TypeGroup::Enum => todo!(),
                    TypeGroup::Union => todo!(),
                    TypeGroup::Function => todo!(),
                    TypeGroup::Pointer => todo!(),
                    TypeGroup::Array => todo!(),
                    TypeGroup::Slice => todo!(),
                }
            }
            TypedValue {
                typ,
                val: Value::Arg(r),
            } => {
                let value = &self.arguments[&Argument::new(r, self.invocation)];
                match typ.group {
                    TypeGroup::None => Value::Unit,
                    TypeGroup::Type => Value::Type(TypeId::from_bytes(value)),
                    TypeGroup::Node => Value::Node(NodeId::from_bytes(value)),
                    TypeGroup::Int => match value.len() {
                        1 => Value::Uint(u8::from_bytes(value) as u64),
                        2 => Value::Uint(u16::from_bytes(value) as u64),
                        4 => Value::Uint(u32::from_bytes(value) as u64),
                        8 => Value::Uint(u64::from_bytes(value)),
                        _ => panic!("invalid byte length"),
                    },
                    TypeGroup::Float => match value.len() {
                        4 => Value::Float(f32::from_bytes(value) as f64),
                        8 => Value::Float(f64::from_bytes(value)),
                        _ => panic!("invalid byte length"),
                    },
                    TypeGroup::Struct => todo!(),
                    TypeGroup::Tagged => todo!(),
                    TypeGroup::Enum => todo!(),
                    TypeGroup::Union => todo!(),
                    TypeGroup::Function => todo!(),
                    TypeGroup::Pointer => todo!(),
                    TypeGroup::Array => todo!(),
                    TypeGroup::Slice => todo!(),
                }
            }
            TypedValue { val: x, .. } => x,
        };
        let (sp, inv, ip) = self.call_stack.pop().unwrap();
        self.stack_ptr = sp;
        self.instr_ptr = ip;
        self.invocation = inv;
        Ok(TypedValue::new(result.typ, result_value))
    }
}
