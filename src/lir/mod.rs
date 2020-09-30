use std::convert::TryInto;
use std::fmt::{self, Display};
use std::mem;

use failure::Fallible;
use hashbrown::{HashMap, HashSet};
use lazy_static::lazy_static;
use smallvec::SmallVec;

use crate::assets::{Handle, Resources};
use crate::ast::{Kind, Node, NodeId, Visit, VisitResult};
use crate::types::{primitive, NamedType, NonConcrete, Type, TypeGroup, TypeId, TypeInfo};
use crate::values::Payload;

use self::context::{ExecutionContext, TypeError, VirtualAddress};
use self::repr::*;

pub mod backend;
pub mod builder;
pub mod compile;
pub mod context;
pub mod repr;
pub mod target;

pub mod foreign {
    use super::*;

    lazy_static! {
        pub static ref PTR: Handle<Foreign> = Handle::new();
        pub static ref FN: Handle<Foreign> = Handle::new();
        pub static ref FORMAT_AST: Handle<Foreign> = Handle::new();
        pub static ref COMPILE: Handle<Foreign> = Handle::new();
    }

    pub fn init(mut res: Resources<&mut Foreign>) {
        res.insert(*PTR, Box::new(type_ptr));
        res.insert(*FN, Box::new(type_fn));
        res.insert(*FORMAT_AST, Box::new(format_ast));
        res.insert(*COMPILE, Box::new(compile));
    }

    fn type_ptr(
        ctx: &mut ExecutionContext,
        res: &mut Resources<(&String, &mut NamedType, &mut Node)>,
        args: &[TypedValue],
    ) -> Fallible<TypedValue> {
        let pointee = match args[0].val {
            Value::Type(t) => t,
            Value::Arg(r) => TypeId::from_bytes(&ctx.arguments[&Argument::new(r, ctx.invocation)]),
            Value::Register(r) => TypeId::from_bytes(&ctx.registers[&r.build(ctx.invocation)]),
            _ => return Err(From::from(TypeError)),
        };
        let named = NamedType {
            name: None,
            t: Type::Pointer(pointee),
        };
        let handle = Handle::new();
        res.insert(handle, named);
        let t = TypeId {
            group: TypeGroup::Pointer,
            concrete: NonConcrete::Type(handle),
        };
        Ok(TypedValue::new(*primitive::TYPE, Value::Type(t)))
    }

    fn type_fn(
        ctx: &mut ExecutionContext,
        res: &mut Resources<(&String, &mut NamedType, &mut Node)>,
        args: &[TypedValue],
    ) -> Fallible<TypedValue> {
        let result_type = match args[0].val {
            Value::Type(t) => t,
            Value::Arg(r) => TypeId::from_bytes(&ctx.arguments[&Argument::new(r, ctx.invocation)]),
            Value::Register(r) => TypeId::from_bytes(&ctx.registers[&r.build(ctx.invocation)]),
            _ => return Err(From::from(TypeError)),
        };

        let mut param_types = SmallVec::new();
        for arg in args.iter().skip(1) {
            let param = match arg.val {
                Value::Type(t) => t,
                Value::Arg(r) => {
                    TypeId::from_bytes(&ctx.arguments[&Argument::new(r, ctx.invocation)])
                }
                Value::Register(r) => TypeId::from_bytes(&ctx.registers[&r.build(ctx.invocation)]),
                _ => return Err(From::from(TypeError)),
            };
            param_types.push(param);
        }

        let named = NamedType {
            name: None,
            t: Type::Function {
                result_type,
                param_types,
            },
        };
        let handle = Handle::new();
        res.insert(handle, named);
        let t = TypeId {
            group: TypeGroup::Function,
            concrete: NonConcrete::Type(handle),
        };
        Ok(TypedValue::new(*primitive::TYPE, Value::Type(t)))
    }

    fn format_ast(
        ctx: &mut ExecutionContext,
        res: &mut Resources<(&String, &mut NamedType, &mut Node)>,
        args: &[TypedValue],
    ) -> Fallible<TypedValue> {
        let node = match args[0].val {
            Value::Node(node) => node,
            Value::Arg(r) => NodeId::from_bytes(&ctx.arguments[&Argument::new(r, ctx.invocation)]),
            Value::Register(r) => NodeId::from_bytes(&ctx.registers[&r.build(ctx.invocation)]),
            _ => return Err(From::from(TypeError)),
        };

        let mut mapping = HashMap::new();

        // PERF: clone is inefficient?
        let node = unsafe { res.get::<Node>(node).unwrap().as_ref().unsafe_clone() };
        let node = node.clone_with(res, |res, this, children| {
            match this.kind {
                Kind::Application => {
                    let func = res.get(children[0].unwrap()).unwrap();
                    let replace = match func.kind {
                        Kind::Nil if func.alternative => match func.payload.unwrap() {
                            Payload::Identifier(ident) => {
                                match res.get::<String>(ident).unwrap().as_str() {
                                    "replace" => true,
                                    _ => false,
                                }
                            }
                            _ => false,
                        },
                        _ => false,
                    };
                    if replace {
                        let ident = res.get(children[1].unwrap()).unwrap();
                        let ident = match ident.kind {
                            Kind::Nil => match ident.payload.unwrap() {
                                Payload::Identifier(ident) => Some(
                                    res.get::<String>(ident).unwrap()[1..]
                                        .parse::<usize>()
                                        .unwrap(),
                                ),
                                _ => None,
                            },
                            _ => None,
                        };
                        if let Some(num) = ident {
                            let id = match args[1 + num].val {
                                Value::Arg(r) => NodeId::from_bytes(
                                    &ctx.arguments[&Argument::new(r, ctx.invocation)],
                                ),
                                Value::Register(r) => {
                                    NodeId::from_bytes(&ctx.registers[&r.build(ctx.invocation)])
                                }
                                Value::Node(node) => node,
                                _ => panic!("not a node"),
                            };
                            let new = res.get(id).unwrap().as_ref().clone();
                            mapping.insert(id, new.id());
                            return new;
                        }
                    }
                }
                _ => {}
            }
            let mut new = this.clone();
            mapping.insert(this.id(), new.id());
            new.children = children;
            new
        });

        let mut payloads = HashMap::new();

        node.visit(Visit::Postorder, res, |_, node, _| {
            if let Some(mut payload) = node.payload {
                match &mut payload {
                    Payload::Type(TypeId {
                        concrete: NonConcrete::Typeof(t),
                        ..
                    }) => {
                        let handle = *t;
                        *t = mapping[&handle];
                    }
                    _ => {}
                }
                payloads.insert(node.id(), payload);
            }
            VisitResult::Recurse
        });

        for (handle, payload) in payloads {
            res.get_mut(handle).unwrap().payload = Some(payload);
        }

        let handle = node.id();
        res.insert(node.id(), node);
        Ok(TypedValue::new(*primitive::NODE, Value::Node(handle)))
    }

    fn compile(
        ctx: &mut ExecutionContext,
        _res: &mut Resources<(&String, &mut NamedType, &mut Node)>,
        args: &[TypedValue],
    ) -> Fallible<TypedValue> {
        let ptr = match args[0].val {
            Value::Arg(r) => {
                VirtualAddress::from_bytes(&ctx.arguments[&Argument::new(r, ctx.invocation)])
            }
            Value::Register(r) => {
                VirtualAddress::from_bytes(&ctx.registers[&r.build(ctx.invocation)])
            }
            Value::Address(ptr) => ptr,
            _ => return Err(From::from(TypeError)),
        };
        let bytes = ctx.read_nul(ptr)?;
        let string = String::from_utf8_lossy(&bytes[..bytes.len() - 1]).into_owned();
        ctx.vars.compile_list.push(string);
        Ok(TypedValue {
            typ: *primitive::UNIT,
            val: Value::Unit,
        })
    }
}

pub const ICMP_EQ: u8 = 0;
pub const ICMP_NE: u8 = 1;
pub const ICMP_LT: u8 = 2;
pub const ICMP_GT: u8 = 3;
pub const ICMP_LE: u8 = 4;
pub const ICMP_GE: u8 = 5;

#[derive(Debug, Clone, Copy)]
pub struct Lifetime {
    pub(crate) block: BasicBlock,
    pub(crate) position: u32,
}

impl Lifetime {
    pub fn new(block: BasicBlock, position: u32) -> Self {
        Self { block, position }
    }

    pub fn empty(block: BasicBlock) -> Self {
        Self { block, position: 0 }
    }

    pub fn to_cmp(&self) -> (BasicBlock, u32) {
        (self.block, self.position)
    }

    pub fn ge(&self, other: &Self, prot: &[BasicBlockPrototype]) -> bool {
        if self.block == other.block {
            self.position >= other.position
        } else if prot.is_child(self.block, other.block) {
            false
        } else if prot.is_child(other.block, self.block) {
            true
        } else {
            false
        }
    }

    pub fn gt(&self, other: &Self, prot: &[BasicBlockPrototype]) -> bool {
        if self.block == other.block {
            self.position > other.position
        } else if prot.is_child(self.block, other.block) {
            false
        } else if prot.is_child(other.block, self.block) {
            true
        } else {
            false
        }
    }
}

pub type ThreadId = Handle<context::ExecutionContext>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct BindingPrototype {
    scope: usize,
    number: i32,
}

impl BindingPrototype {
    pub fn new(scope: usize, number: i32) -> Self {
        Self { scope, number }
    }

    pub fn inc(&mut self) {
        self.number += 1;
    }

    pub fn scope(&self) -> usize {
        self.scope
    }

    pub fn number(&self) -> i32 {
        self.number
    }

    pub fn build(&self, invocation: u64) -> Binding {
        Binding {
            prot: *self,
            invocation,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Binding {
    prot: BindingPrototype,
    invocation: u64,
}

impl Binding {
    pub fn get(&self) -> &BindingPrototype {
        &self.prot
    }

    pub fn invocation(&self) -> u64 {
        self.invocation
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Argument {
    prot: i32,
    invocation: u64,
}

impl Argument {
    pub fn new(prot: i32, invocation: u64) -> Self {
        Self { prot, invocation }
    }

    pub fn get(&self) -> i32 {
        self.prot
    }

    pub fn invocation(&self) -> u64 {
        self.invocation
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct FuncId {
    pub offset: u32,
    pub idx: u32,
}

impl FuncId {
    pub fn new(offset: usize, idx: usize) -> Self {
        let offset = offset as _;
        let idx = idx as _;
        Self { offset, idx }
    }
}

impl Repr for FuncId {
    fn type_info(&self) -> TypeInfo {
        Self::static_type_info()
    }

    fn write_bytes(&self, out: &mut [u8]) {
        out[..4].copy_from_slice(&self.offset.to_le_bytes());
        out[4..].copy_from_slice(&self.idx.to_le_bytes());
    }

    fn copy_from_bytes(&mut self, bytes: &[u8]) {
        if bytes.len() != mem::size_of::<u64>() {
            panic!("attempt to copy from slice of invalid length");
        }
        self.offset = u32::from_le_bytes(bytes[..8].try_into().unwrap());
        self.idx = u32::from_le_bytes(bytes[8..].try_into().unwrap());
    }
}

impl ReprExt for FuncId {
    fn static_type_info() -> TypeInfo {
        TypeInfo::new(mem::size_of::<u32>() * 2, mem::align_of::<u64>())
    }

    fn from_bytes(bytes: &[u8]) -> Self {
        if bytes.len() != mem::size_of::<u32>() * 2 {
            panic!("attempt to copy from slice of invalid length");
        }
        Self {
            offset: u32::from_le_bytes(bytes[..4].try_into().unwrap()),
            idx: u32::from_le_bytes(bytes[4..].try_into().unwrap()),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct TypedValue {
    pub typ: TypeId,
    pub val: Value,
}

impl TypedValue {
    pub fn new(typ: TypeId, val: Value) -> Self {
        Self { typ, val }
    }
}

pub type Elems = SmallVec<[TypedValue; 4]>;
pub type Variants = SmallVec<[Elems; 4]>;
pub type Bytes = SmallVec<[u8; 32]>;
pub type Foreign = Box<
    dyn Fn(
            &mut ExecutionContext,
            &mut Resources<(&String, &mut NamedType, &mut Node)>,
            &[TypedValue],
        ) -> Fallible<TypedValue>
        + Send
        + Sync
        + 'static,
>;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Value {
    Unit,
    Arg(i32),
    Register(BindingPrototype),
    Address(context::VirtualAddress),
    Bool(bool),
    Uint(u64),
    Float(f64),
    Type(TypeId),
    Node(NodeId),
    Array(Handle<Elems>),
    Struct(Handle<Elems>),
    Tagged(Handle<TypedValue>, Handle<Elems>, Handle<Variants>),
    Union(Handle<Bytes>),
    Function(FuncId),
    Label(BasicBlock),
    Ffi(Handle<Foreign>),
}

impl Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Unit => write!(f, "()"),
            Value::Arg(i) => write!(f, "%-{}", i),
            Value::Register(b) => write!(f, "%{}/{}", b.scope(), b.number()),
            Value::Address(addr) => write!(f, "0x{:016x}", addr.0),
            Value::Bool(p) => write!(f, "{}", p),
            Value::Uint(u) => write!(f, "{}", u),
            Value::Float(x) => write!(f, "{:?}", x),
            Value::Type(id) => write!(f, "type {:?}", id.group),
            Value::Node(id) => write!(f, "node {}", id.display()),
            Value::Array(..) => todo!(),
            // TODO
            Value::Struct(_fields) => write!(f, "()"),
            Value::Tagged(..) => todo!(),
            Value::Union(..) => todo!(),
            Value::Function(func) => write!(f, "@{}+{}", func.offset, func.idx),
            Value::Label(b) => write!(f, ".L{}", b.number()),
            Value::Ffi(id) => write!(f, "ffi {}", id.display()),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Instruction {
    /// One argument: u64
    Alloca,
    /// Two arguments: an address and anything
    Store,
    /// One argument: an address
    Load,
    /// One argument: anything
    Copy,
    /// At least one argument: a function or fn reference
    Call,
    /// At least one argument: a u64 (node kind)
    NewNode,
    /// One argument: any value
    Return,
    /// One argument: a basic block
    Br,
    /// Three arguments: any boolean value and two basic blocks
    CondBr,
    /// Any even number of arguments: value, basic block pairs
    Phi,
    /// Two arguments: two integers
    Add,
    /// Two arguments: two integers
    Sub,
    /// Two arguments: two integers
    Mul,
    /// Two arguments: two integers
    Div,
    /// Two arguments: two integers
    Rem,
    /// Two arguments: two booleans or two integers of the same size
    BitAnd,
    /// Two arguments: two booleans or two integers of the same size
    BitOr,
    /// Two arguments: two booleans or two integers of the same size
    BitXor,
    /// Three arguments: a literal integer in the range `[0; 6)`, two booleans or two integers of the same size
    Icmp,
}

#[derive(Debug, Clone, Copy)]
pub enum RegisterConstraint {
    // on amd64: rax
    PhiRegister,
    // on amd64: rax
    ReturnValue,
    // on amd64: rdi
    ReturnByRef,
    // on amd64: rsi
    HandlerValue,
    // on amd64: rsi
    HandlerByRef,
}

#[derive(Debug, Clone)]
pub struct Ir {
    pub(crate) constraint: Option<RegisterConstraint>,
    pub(crate) binding: Option<BindingPrototype>,
    pub(crate) life: Lifetime,
    pub(crate) instr: Instruction,
    pub(crate) args: SmallVec<[TypedValue; 4]>,
    pub(crate) typ: TypeId,
}

impl Ir {
    pub fn new(
        basic_block: BasicBlock,
        binding: BindingPrototype,
        typ: TypeId,
        instr: Instruction,
        args: SmallVec<[TypedValue; 4]>,
    ) -> Self {
        Self {
            constraint: None,
            binding: Some(binding),
            life: Lifetime::empty(basic_block),
            instr,
            args,
            typ,
        }
    }

    pub fn new_void(
        basic_block: BasicBlock,
        instr: Instruction,
        args: SmallVec<[TypedValue; 4]>,
    ) -> Self {
        Self {
            constraint: None,
            binding: None,
            life: Lifetime::empty(basic_block),
            instr,
            args,
            typ: *primitive::UNIT,
        }
    }

    pub fn with_constraint(mut self, constraint: RegisterConstraint) -> Self {
        self.constraint = Some(constraint);
        self
    }
}

impl Display for Ir {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(binding) = self.binding {
            write!(f, "%{}/{} := ", binding.scope(), binding.number())?;
        }
        write!(f, "{:?}", self.instr)?;
        if let Some(arg) = self.args.first() {
            write!(f, " {}", arg.val)?;
        }
        for arg in self.args.iter().skip(1) {
            write!(f, ", {}", arg.val)?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BasicBlock(u32);

impl BasicBlock {
    pub fn new(number: u32) -> Self {
        Self(number)
    }

    pub fn number(&self) -> u32 {
        self.0
    }
}

#[derive(Debug, Clone)]
pub struct BasicBlockPrototype {
    number: u32,
    start: usize,
    children: SmallVec<[u32; 2]>,
}

impl BasicBlockPrototype {
    pub fn new(number: u32) -> Self {
        Self {
            number,
            start: 0,
            children: SmallVec::new(),
        }
    }

    pub fn with_start(number: u32, start: usize) -> Self {
        Self {
            number,
            start,
            children: SmallVec::new(),
        }
    }

    pub fn as_ref(&self) -> BasicBlock {
        BasicBlock::new(self.number)
    }

    pub(self) fn add(&mut self, number: u32) {
        self.children.push(number);
    }
}

pub trait BasicBlockExt {
    fn is_child(&self, this: BasicBlock, other: BasicBlock) -> bool;
}

impl BasicBlockExt for &[BasicBlockPrototype] {
    fn is_child(&self, this: BasicBlock, other: BasicBlock) -> bool {
        let mut children = self[this.number() as usize].children.to_vec();
        let mut append = Vec::<u32>::new();
        while !children.is_empty() {
            for child in children.drain(..) {
                if child == other.number() {
                    return true;
                }
                append.extend(&self[child as usize].children);
            }
            children.extend(append.drain(..));
        }
        false
    }
}

#[derive(Debug, Clone)]
pub struct Global {
    pub(crate) name: String,
    pub(crate) typ: TypeId,
    pub(crate) offset: usize,
    pub(crate) is_const: bool,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub(crate) name: String,
    pub(crate) param_types: SmallVec<[TypeId; 4]>,
    pub(crate) result_type: TypeId,
    pub(crate) body: Vec<Ir>,
    pub(crate) blocks: Vec<BasicBlockPrototype>,
    pub(crate) is_ref: HashSet<BindingPrototype>,
}

impl Function {
    pub fn param_types(&self) -> &[TypeId] {
        &self.param_types
    }

    pub fn result_type(&self) -> TypeId {
        self.result_type
    }

    pub fn add_basic_block(&mut self, start: usize, parents: &[u32]) -> BasicBlock {
        let number = self.blocks.len() as u32;
        for &p in parents {
            self.blocks[p as usize].add(number);
        }
        self.blocks
            .push(BasicBlockPrototype::with_start(number, start));
        BasicBlock::new(number)
    }

    fn lifetime_inner<'a, I>(
        &self,
        visited: &mut HashSet<BasicBlock>,
        binding: BindingPrototype,
        start: &mut Option<Lifetime>,
        end: &mut Option<Lifetime>,
        iter: I,
    ) where
        I: Iterator<Item = &'a Ir>,
    {
        for ir in iter {
            if ir.binding == Some(binding) {
                if start.is_none() {
                    *start = Some(ir.life);
                    *end = Some(ir.life);
                } else {
                    *end = Some(ir.life);
                }
            }
            match ir.instr {
                Instruction::Br => {
                    match ir.args[0].val {
                        Value::Label(bb) => {
                            if !visited.contains(&bb) {
                                visited.insert(bb);
                                let bb = &self.blocks[bb.number() as usize];
                                self.lifetime_inner(
                                    visited,
                                    binding,
                                    start,
                                    end,
                                    self.body[bb.start..].iter(),
                                );
                            }
                        }
                        _ => {}
                    }
                    break;
                }
                Instruction::CondBr => {
                    match ir.args[1].val {
                        Value::Label(bb) => {
                            if !visited.contains(&bb) {
                                visited.insert(bb);
                                let bb = &self.blocks[bb.number() as usize];
                                self.lifetime_inner(
                                    visited,
                                    binding,
                                    start,
                                    end,
                                    self.body[bb.start..].iter(),
                                );
                            }
                        }
                        _ => {}
                    }
                    match ir.args[2].val {
                        Value::Label(bb) => {
                            if !visited.contains(&bb) {
                                visited.insert(bb);
                                let bb = &self.blocks[bb.number() as usize];
                                self.lifetime_inner(
                                    visited,
                                    binding,
                                    start,
                                    end,
                                    self.body[bb.start..].iter(),
                                );
                            }
                        }
                        _ => {}
                    }
                    break;
                }
                Instruction::Return => break,
                _ => {}
            }
        }
    }

    pub fn lifetime(&self, binding: BindingPrototype) -> (Lifetime, Lifetime) {
        let mut start = None;
        let mut end = None;
        let mut visited = HashSet::new();
        self.lifetime_inner(
            &mut visited,
            binding,
            &mut start,
            &mut end,
            self.body.iter(),
        );
        let start = start.unwrap();
        let end = end.unwrap();
        (start, end)
    }

    pub fn is_ref(&self, r: BindingPrototype) -> bool {
        self.is_ref.contains(&r)
    }
}

impl Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "%{} :: {{", self.name)?;
        if let Some(t) = self.param_types.get(0) {
            write!(f, "type {:?}", t.group)?;
        }
        for t in self.param_types.iter().skip(1) {
            write!(f, "; type {:?}", t.group)?;
        }
        writeln!(f, "}} => (")?;
        for (ip, ir) in self.body.iter().enumerate() {
            for (l, block) in self.blocks.iter().enumerate() {
                if block.start == ip {
                    writeln!(f, ".L{}:", l)?;
                    break;
                }
            }
            writeln!(f, "  {}", ir)?;
        }
        writeln!(f, ");")
    }
}

#[cfg(test)]
mod tests {
    use crate::assets::*;
    use crate::types::*;

    use super::context::*;
    use super::target::*;
    use super::*;

    #[test]
    fn sanity() {
        let world = World::new();
        world.init_asset::<Foreign>();
        world.init_asset::<NamedType>();
        world.init_asset::<Node>();
        world.init_asset::<String>();

        let mut lazy = LazyUpdate::new();

        let mut main = 0;
        let (_, mut ctx) = ExecutionContext::builder(world.resources > (), Target::default())
            .function("main")
            .result(*primitive::SINT)
            .build_return(TypedValue::new(*primitive::SINT, Value::Uint(5)))
            .build(&mut main)
            .build();
        assert_eq!(
            ctx.call(
                &mut lazy,
                &world.resources(),
                &mut world.resources(),
                main,
                &[]
            )
            .unwrap(),
            Value::Uint(5)
        );
    }
}
