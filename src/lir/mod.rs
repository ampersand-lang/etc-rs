use std::iter;
use std::fmt::{self, Display};

use smallvec::SmallVec;
use lazy_static::lazy_static;
use failure::Fallible;

use crate::assets::{Handle, Resources};
use crate::ast::{Kind, Node, NodeId};
use crate::types::{NamedType, Type, TypeGroup, TypeId, TypeOrPlaceholder};
use crate::values::Payload;

use self::context::{ExecutionContext, TypeError};
use self::repr::*;

pub mod builder;
pub mod codegen;
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
    }
    
    pub fn init(mut res: Resources<&mut Foreign>) {
        res.insert(*PTR, Box::new(type_ptr));
        res.insert(*FN, Box::new(type_fn));
        res.insert(*FORMAT_AST, Box::new(format_ast));
    }

    fn type_ptr(ctx: &mut ExecutionContext, res: &mut Resources<(&String, &mut NamedType, &mut Node)>, args: &[Value]) -> Fallible<Value> {
        let pointee = match args[0] {
            Value::Type(t) => t,
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
            concrete: TypeOrPlaceholder::Type(handle),
        };
        Ok(Value::Type(t))
    }

    fn type_fn(ctx: &mut ExecutionContext, res: &mut Resources<(&String, &mut NamedType, &mut Node)>, args: &[Value]) -> Fallible<Value> {
        let result_type = match args[0] {
            Value::Type(t) => t,
            Value::Register(r) => TypeId::from_bytes(&ctx.registers[&r.build(ctx.invocation)]),
            _ => return Err(From::from(TypeError)),
        };

        let mut param_types = SmallVec::new();
        for arg in args.iter().skip(1) {
            let param = match arg {
                Value::Type(t) => *t,
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
            concrete: TypeOrPlaceholder::Type(handle),
        };
        Ok(Value::Type(t))
    }

    fn format_ast(ctx: &mut ExecutionContext, res: &mut Resources<(&String, &mut NamedType, &mut Node)>, args: &[Value]) -> Fallible<Value> {
        let node = match args[0] {
            Value::Node(node) => node,
            Value::Register(r) => NodeId::from_bytes(&ctx.registers[&r.build(ctx.invocation)]),
            _ => return Err(From::from(TypeError)),
        };

        // PERF: clone is inefficient?
        let node = res.get::<Node>(node).unwrap().as_ref().clone();
        let node = node.clone_with(res, |res, this, children| {
            match this.kind {
                Kind::Application => {
                    let func = res.get(children[0].unwrap()).unwrap();
                    let replace = match func.kind {
                        Kind::Nil if func.alternative => {
                            match func.payload.unwrap() {
                                Payload::Identifier(ident) => {
                                    match res.get::<String>(ident).unwrap().as_str() {
                                        "replace" => true,
                                        _ => false,
                                    }
                                }
                                _ => false,
                            }
                        }
                        _ => false,
                    };
                    if replace {
                        let ident = res.get(children[1].unwrap()).unwrap();
                        let ident = match ident.kind {
                            Kind::Nil => {
                                match ident.payload.unwrap() {
                                    Payload::Identifier(ident) => {
                                        Some(res.get::<String>(ident).unwrap()[1..].parse::<usize>().unwrap())
                                    }
                                    _ => None,
                                }
                            }
                            _ => None,
                        };
                        if let Some(num) = ident {
                            let id = match args[1 + num] {
                                Value::Register(r) => {
                                    NodeId::from_bytes(&ctx.registers[&r.build(ctx.invocation)])
                                }
                                Value::Node(node) => node,
                                _ => panic!("not a node"),
                            };
                            return res.get(id).unwrap().as_ref().clone();
                        }
                    }
                }
                _ => {}
            }
            this.clone()
        });

        let handle = node.id();
        res.insert(node.id(), node);
        Ok(Value::Node(handle))
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

pub type FuncId = usize;

#[derive(Debug, Clone, PartialEq)]
pub struct TypedValue {
    pub typ: TypeId,
    pub val: Value,
}

pub type Elems = SmallVec<[Value; 4]>;
pub type Fields = SmallVec<[TypedValue; 4]>;
pub type Variants = SmallVec<[Fields; 4]>;
pub type Bytes = SmallVec<[u8; 32]>;
pub type Foreign = Box<dyn Fn(&mut ExecutionContext, &mut Resources<(&String, &mut NamedType, &mut Node)>, &[Value]) -> Fallible<Value> + Send + Sync + 'static>;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Value {
    Unit,
    Register(BindingPrototype),
    Address(context::VirtualAddress),
    Uint(u64),
    Float(f64),
    Type(TypeId),
    Node(NodeId),
    Array(Handle<Elems>),
    Struct(Handle<Fields>),
    Tagged(Handle<Value>, Handle<Fields>, Handle<Variants>),
    Union(Handle<Bytes>),
    Function(FuncId),
    Ffi(Handle<Foreign>),
}

impl Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Unit => write!(f, "()"),
            Value::Register(b) => write!(f, "%{}/{}", b.scope(), b.number()),
            Value::Address(addr) => write!(f, "0x{:016x}", addr.0),
            Value::Uint(u) => write!(f, "{}", u),
            Value::Float(x) => write!(f, "{:?}", x),
            Value::Type(id) => write!(f, "type {:?}", id.group),
            Value::Node(id) => write!(f, "node {}", id.display()),
            Value::Array(..) => todo!(),
            Value::Struct(..) => todo!(),
            Value::Tagged(..) => todo!(),
            Value::Union(..) => todo!(),
            Value::Function(func) => write!(f, "%{}", func),
            Value::Ffi(id) => write!(f, "ffi {}", id.display()),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Instruction {
    /// Two arguments: a type and a u64
    Alloca,
    /// Three arguments: a type, an address and anything
    Store,
    /// Two argument: a type and an address
    Load,
    /// At least one argument: a global
    Call,
    /// At least one argument: a u64 (node kind)
    NewNode,
    /// One argument: any value
    Return,
}

#[derive(Debug, Clone)]
pub struct Ir {
    pub(crate) binding: Option<BindingPrototype>,
    pub(crate) instr: Instruction,
    pub(crate) args: SmallVec<[Value; 4]>,
}

impl Display for Ir {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(binding) = self.binding {
            write!(f, "%{}/{} := ", binding.scope(), binding.number())?;
        }
        write!(f, "{:?}", self.instr)?;
        if let Some(arg) = self.args.first() {
            write!(f, " {}", arg)?;
        }
        for arg in self.args.iter().skip(1) {
            write!(f, ", {}", arg)?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct Function {
    pub(crate) name: String,
    pub(crate) param_types: SmallVec<[TypeId; 4]>,
    pub(crate) result_type: TypeId,
    pub(crate) body: Vec<Ir>,
}

impl Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "%{} := (...) => {{", self.name)?;
        for ir in &self.body {
            writeln!(f, "  {}", ir)?;
        }
        writeln!(f, "}};")
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
        world.init_asset::<NamedType>();

        let mut lazy = LazyUpdate::new();

        let mut main = 0;
        let (res, mut ctx) =
            ExecutionContext::builder(world.resources::<&NamedType>(), Target::default())
                .function("main")
                .result(*primitive::SINT)
                .build_return(Value::Uint(5))
                .build(&mut main)
                .build();
        assert_eq!(
            ctx.call(&mut lazy, &res, main, &[]).unwrap(),
            Value::Uint(5)
        );
    }
}
