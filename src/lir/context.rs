use std::convert::{TryFrom, TryInto};
use std::mem;
use std::ops::Range;

use failure::{Fail, Fallible};
use hashbrown::HashMap;

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
pub struct ExecAddress(pub(crate) u32, pub(crate) u32);

pub type ScopeId = usize;

#[derive(Debug, Clone)]
pub struct Scope {
    pub(crate) parent: Option<ScopeId>,
    pub(crate) base_ptr: u64,
    pub(crate) bindings: HashMap<i32, PhysicalAddress>,
}

#[derive(Debug, Clone)]
pub struct Scopes {
    pub(crate) curr: Vec<ScopeId>,
    pub(crate) list: Vec<Scope>,
}

impl Scopes {
    pub fn new() -> Self {
        Self {
            curr: Vec::new(),
            list: Vec::new(),
        }
    }

    pub fn get(&self, key: Binding) -> Option<PhysicalAddress> {
        self.list[key.0].bindings.get(&key.1).copied()
    }

    pub fn begin(&mut self, id: ScopeId) {
        self.curr.push(id);
    }

    pub fn end(&mut self) -> u64 {
        let base_ptr = self.current().base_ptr;
        self.curr.pop();
        base_ptr
    }

    pub fn current(&self) -> &Scope {
        &self.list[self.curr.last().copied().unwrap()]
    }

    pub(crate) fn current_mut(&mut self) -> &mut Scope {
        &mut self.list[self.curr.last().copied().unwrap()]
    }
}

#[derive(Debug, Clone)]
pub enum Result {
    Continue,
    Return(Value),
}

#[derive(Debug, Clone)]
pub struct ExecutionContext {
    pub(crate) target: Target,
    pub(crate) bindings: Scopes,
    pub(crate) call_stack: Vec<ExecAddress>,
    pub(crate) instr_ptr: ExecAddress,
    pub(crate) data: Vec<Global>,
    pub(crate) heap: Vec<u8>,
    pub(crate) stack: Vec<u8>,
    pub(crate) stack_ptr: usize,
    pub(crate) data_addr: u64,
    pub(crate) heap_addr: u64,
    pub(crate) stack_addr: u64,
}

impl ExecutionContext {
    pub const MAIN: GlobId = 0;

    pub fn new(target: Target) -> Self {
        Self {
            target,
            bindings: Scopes::new(),
            call_stack: Vec::new(),
            instr_ptr: ExecAddress(0, 0),
            data: Vec::new(),
            stack: vec![0; DEFAULT_STACK_SPACE],
            heap: vec![0; DEFAULT_HEAP_SPACE],
            stack_ptr: 0,
            data_addr: 0x100000,
            heap_addr: 0x4000000,
            stack_addr: 0x80000000,
        }
    }

    pub fn builder(res: Resources<&NamedType>, target: Target) -> Builder {
        Builder {
            res,
            ctx: Self::new(target),
        }
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
            (Section::Data, _) => Err(From::from(InvalidSectionAccess)),
            (_, Section::Data) => Err(From::from(InvalidSectionAccess)),
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
            Section::Data => Err(From::from(InvalidSectionAccess)),
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
            Section::Data => Err(From::from(InvalidSectionAccess)),
            Section::Heap => Ok(self.heap[range].copy_from_slice(bytes)),
            Section::Stack => Ok(self.stack[range].copy_from_slice(bytes)),
        }
    }

    fn execute(
        &mut self,
        lazy: &mut LazyUpdate,
        res: &Resources<&NamedType>,
        ir: Ir,
    ) -> Fallible<Result> {
        match ir.instr {
            Instruction::Alloca => {
                let t = match ir.args[0] {
                    Value::Global(handle) => match self.data[handle] {
                        Global::Constant(Value::Type(t)) => t,
                        _ => return Err(From::from(TypeError)),
                    },
                    Value::Type(t) => t,
                    _ => return Err(From::from(TypeError)),
                };
                let t = t.type_info(res, &self.target);
                let n = match ir.args[1] {
                    Value::Uint(n) => n,
                    _ => return Err(From::from(TypeError)),
                };
                self.stack_ptr += t.size * n as usize;
                Ok(Result::Continue)
            }
            Instruction::Store => {
                let t = match ir.args[0] {
                    Value::Global(handle) => match self.data[handle] {
                        Global::Constant(Value::Type(t)) => t,
                        _ => return Err(From::from(TypeError)),
                    },
                    Value::Type(t) => t,
                    _ => return Err(From::from(TypeError)),
                };
                let t = t.type_info(res, &self.target);
                let ptr = match ir.args[1] {
                    Value::Global(handle) => match self.data[handle] {
                        Global::Constant(Value::Address(ptr)) => ptr,
                        _ => return Err(From::from(TypeError)),
                    },
                    Value::Address(ptr) => ptr,
                    _ => return Err(From::from(TypeError)),
                };
                let mut value = vec![0_u8; t.size];
                match ir.args[2] {
                    Value::Unit => {}
                    Value::Global(_handle) => todo!(),
                    Value::Uint(int) => int.write_bytes(&mut value),
                    Value::Float(flt) => flt.write_bytes(&mut value),
                    Value::Unref(addr) => value.copy_from_slice(self.read(addr, 0..t.size)?),
                    Value::Address(addr) => addr.write_bytes(&mut value),
                    Value::Type(typ) => typ.write_bytes(&mut value),
                    Value::Node(node) => node.write_bytes(&mut value),
                    Value::Array(_) => todo!(),
                    Value::Struct(_) => todo!(),
                    Value::Union(_) => todo!(),
                    Value::Tagged(..) => todo!(),
                }
                self.write(ptr, &value)?;
                Ok(Result::Continue)
            }
            Instruction::Load => {
                let t = match ir.args[0] {
                    Value::Global(handle) => match self.data[handle] {
                        Global::Constant(Value::Type(t)) => t,
                        _ => return Err(From::from(TypeError)),
                    },
                    Value::Type(t) => t,
                    _ => return Err(From::from(TypeError)),
                };
                let t = t.type_info(res, &self.target);
                let ptr = match ir.args[1] {
                    Value::Global(handle) => match self.data[handle] {
                        Global::Constant(Value::Address(ptr)) => ptr,
                        _ => return Err(From::from(TypeError)),
                    },
                    Value::Address(ptr) => ptr,
                    _ => return Err(From::from(TypeError)),
                };
                let addr = self.to_virtual(PhysicalAddress(Section::Stack, self.stack_ptr as _));
                self.copy_from_virtual_to_virtual(ptr, addr, 0..t.size)?;
                self.stack_ptr += t.size;
                Ok(Result::Continue)
            }
            Instruction::Call => {
                let func = match ir.args[0] {
                    Value::Global(handle) => handle,
                    _ => return Err(From::from(TypeError)),
                };

                self.call_stack.push(self.instr_ptr);
                self.instr_ptr = ExecAddress(func as _, 0);

                Ok(Result::Continue)
            }
            Instruction::NewNode => {
                let kind = match ir.args[0] {
                    Value::Uint(n) => ast::Kind::try_from(n as u8).expect("invalid node kind"),
                    _ => return Err(From::from(TypeError)),
                };
                let mut children: SmallVec<[Option<NodeId>; 4]> = SmallVec::new();
                for child in &ir.args[1..] {
                    match child {
                        Value::Global(handle) => match self.data[*handle] {
                            Global::Constant(Value::Node(n)) => children.push(Some(n)),
                            _ => return Err(From::from(TypeError)),
                        },
                        Value::Node(n) => children.push(Some(*n)),
                        _ => return Err(From::from(TypeError)),
                    }
                }
                let node = Node::new(kind, children);
                let mut bytes = vec![0_u8; type_info::NODE.size];
                node.id().write_bytes(&mut bytes);
                lazy.insert(node.id(), node);
                let addr = PhysicalAddress(Section::Stack, self.stack_ptr as _);
                self.write_physical(addr, &bytes)?;
                self.stack_ptr += type_info::NODE.size;
                Ok(Result::Continue)
            }
            Instruction::Begin => {
                let n = match ir.args[0] {
                    Value::Uint(n) => n,
                    _ => return Err(From::from(TypeError)),
                };
                self.bindings.begin(n as _);
                debug_assert_eq!(self.stack_ptr as u64, self.bindings.current().base_ptr);
                Ok(Result::Continue)
            }
            Instruction::End => {
                self.stack_ptr = self.bindings.end() as _;
                Ok(Result::Continue)
            }
            Instruction::Return => {
                if self.call_stack.len() == 1 {
                    let result = Result::Return(ir.args[0].clone());
                    self.stack_ptr = self.bindings.end() as _;
                    Ok(result)
                } else {
                    self.stack_ptr = self.bindings.end() as _;
                    self.instr_ptr = self.call_stack.pop().unwrap();
                    Ok(Result::Continue)
                }
            }
        }
    }

    fn run(&mut self, lazy: &mut LazyUpdate, res: &Resources<&NamedType>) -> Fallible<Value> {
        loop {
            let func = match &self.data[self.instr_ptr.0 as usize] {
                Global::Function(func) => func,
                _ => panic!("global is not a function"),
            };
            let ir: Ir = func.body[self.instr_ptr.1 as usize].clone();
            self.instr_ptr.1 += 1;
            match self.execute(lazy, res, ir)? {
                Result::Continue => {}
                Result::Return(value) => return Ok(value),
            }
        }
    }

    pub fn call(
        &mut self,
        lazy: &mut LazyUpdate,
        res: &Resources<&NamedType>,
        f: GlobId,
        args: &[TypedValue],
    ) -> Fallible<Value> {
        self.call_stack.push(self.instr_ptr);
        self.instr_ptr = ExecAddress(f as _, 0);
        for TypedValue { typ, val } in args {
            let t = typ.type_info(res, &self.target);

            let mut value = vec![0_u8; t.size];
            match val {
                Value::Unit => {}
                Value::Global(_handle) => todo!(),
                Value::Uint(int) => int.write_bytes(&mut value),
                Value::Float(flt) => flt.write_bytes(&mut value),
                Value::Unref(addr) => value.copy_from_slice(self.read(*addr, 0..t.size)?),
                Value::Address(addr) => addr.write_bytes(&mut value),
                Value::Type(typ) => typ.write_bytes(&mut value),
                Value::Node(node) => node.write_bytes(&mut value),
                Value::Array(_) => todo!(),
                Value::Struct(_) => todo!(),
                Value::Union(_) => todo!(),
                Value::Tagged(..) => todo!(),
            }
            self.write_physical(PhysicalAddress(Section::Stack, self.stack_ptr as _), &value)?;

            self.stack_ptr += t.size;
        }
        let result = self.run(lazy, res);
        self.instr_ptr = self.call_stack.pop().unwrap();
        result
    }
}
