use std::fmt::{self, Display, Write};
use std::ops::{Add, Mul, Sub};

use bitflags::bitflags;
use hashbrown::HashMap;
use smallvec::SmallVec;

use crate::lir::{BasicBlockPrototype, BindingPrototype, Lifetime};
use crate::types::TypeInfo;
use crate::utils::IntPtr;

pub use call_conv::*;
pub use linscan::*;

mod call_conv;
mod linscan;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Address {
    pub(crate) base: Option<Register>,
    pub(crate) index: Option<Register>,
    pub(crate) size: Option<u64>,
    pub(crate) offset: Option<u64>,
}

impl From<Register> for Address {
    fn from(reg: Register) -> Self {
        Self {
            base: Some(reg),
            index: None,
            size: None,
            offset: None,
        }
    }
}

impl From<u64> for Address {
    fn from(ptr: u64) -> Self {
        Self {
            base: None,
            index: None,
            size: None,
            offset: Some(ptr),
        }
    }
}

bitflags! {
    pub struct Bits: u8 {
        const L = 0b00000001;
        const H = 0b00000010;
        const X = 0b00000011;
        const E = 0b00001111;
        const R = 0b11111111;
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Register {
    Rax(Bits),
    Rcx(Bits),
    Rdx(Bits),
    Rbx(Bits),
    Rsp(Bits),
    Rbp(Bits),
    Rsi(Bits),
    Rdi(Bits),
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,
}

impl Register {
    pub const fn rax() -> Self {
        Self::Rax(Bits::R)
    }

    pub const fn rdx() -> Self {
        Self::Rdx(Bits::R)
    }

    pub const fn rcx() -> Self {
        Self::Rcx(Bits::R)
    }

    pub const fn rbx() -> Self {
        Self::Rbx(Bits::R)
    }

    pub const fn rsp() -> Self {
        Self::Rsp(Bits::R)
    }

    pub const fn rbp() -> Self {
        Self::Rbp(Bits::R)
    }

    pub const fn rsi() -> Self {
        Self::Rsi(Bits::R)
    }

    pub const fn rdi() -> Self {
        Self::Rdi(Bits::R)
    }

    pub const fn r8() -> Self {
        Self::R8
    }

    pub const fn r9() -> Self {
        Self::R9
    }

    pub const fn r10() -> Self {
        Self::R10
    }

    pub const fn r11() -> Self {
        Self::R11
    }

    pub const fn r12() -> Self {
        Self::R12
    }

    pub const fn r13() -> Self {
        Self::R13
    }

    pub const fn r14() -> Self {
        Self::R14
    }

    pub const fn r15() -> Self {
        Self::R15
    }
}

impl Add<u64> for Register {
    type Output = Address;

    fn add(self, other: u64) -> Self::Output {
        Address {
            base: Some(self),
            index: None,
            size: None,
            offset: Some(other),
        }
    }
}

impl Sub<u64> for Register {
    type Output = Address;

    fn sub(self, other: u64) -> Self::Output {
        self + -(other as i64) as u64
    }
}

impl Add<Address> for Register {
    type Output = Address;

    fn add(self, other: Address) -> Self::Output {
        Address {
            base: Some(self),
            ..other
        }
    }
}

impl Mul<u64> for Register {
    type Output = Address;

    fn mul(self, other: u64) -> Self::Output {
        Address {
            base: None,
            index: Some(self),
            size: Some(other),
            offset: None,
        }
    }
}

impl Add<u64> for Address {
    type Output = Address;

    fn add(self, other: u64) -> Self::Output {
        Address {
            offset: Some(other),
            ..self
        }
    }
}

impl Sub<u64> for Address {
    type Output = Address;

    fn sub(self, other: u64) -> Self::Output {
        self + -(other as i64) as u64
    }
}

impl Display for Register {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Rax(Bits::L) => write!(f, "al"),
            Self::Rax(Bits::H) => write!(f, "ah"),
            Self::Rax(Bits::X) => write!(f, "ax"),
            Self::Rax(Bits::E) => write!(f, "eax"),
            Self::Rax(Bits::R) => write!(f, "rax"),
            Self::Rax(_) => Err(fmt::Error),
            Self::Rdx(Bits::L) => write!(f, "dl"),
            Self::Rdx(Bits::H) => write!(f, "dh"),
            Self::Rdx(Bits::X) => write!(f, "dx"),
            Self::Rdx(Bits::E) => write!(f, "edx"),
            Self::Rdx(Bits::R) => write!(f, "rdx"),
            Self::Rdx(_) => Err(fmt::Error),
            Self::Rcx(Bits::L) => write!(f, "cl"),
            Self::Rcx(Bits::H) => write!(f, "ch"),
            Self::Rcx(Bits::X) => write!(f, "cx"),
            Self::Rcx(Bits::E) => write!(f, "ecx"),
            Self::Rcx(Bits::R) => write!(f, "rcx"),
            Self::Rcx(_) => Err(fmt::Error),
            Self::Rbx(Bits::L) => write!(f, "bl"),
            Self::Rbx(Bits::H) => write!(f, "bh"),
            Self::Rbx(Bits::X) => write!(f, "bx"),
            Self::Rbx(Bits::E) => write!(f, "ebx"),
            Self::Rbx(Bits::R) => write!(f, "rbx"),
            Self::Rbx(_) => Err(fmt::Error),
            Self::Rsp(Bits::X) => write!(f, "sp"),
            Self::Rsp(Bits::E) => write!(f, "esp"),
            Self::Rsp(Bits::R) => write!(f, "rsp"),
            Self::Rsp(_) => Err(fmt::Error),
            Self::Rbp(Bits::X) => write!(f, "bp"),
            Self::Rbp(Bits::E) => write!(f, "ebp"),
            Self::Rbp(Bits::R) => write!(f, "rbp"),
            Self::Rbp(_) => Err(fmt::Error),
            Self::Rsi(Bits::X) => write!(f, "si"),
            Self::Rsi(Bits::E) => write!(f, "esi"),
            Self::Rsi(Bits::R) => write!(f, "rsi"),
            Self::Rsi(_) => Err(fmt::Error),
            Self::Rdi(Bits::X) => write!(f, "di"),
            Self::Rdi(Bits::E) => write!(f, "edi"),
            Self::Rdi(Bits::R) => write!(f, "rdi"),
            Self::Rdi(_) => Err(fmt::Error),
            Self::R8 => write!(f, "r8"),
            Self::R9 => write!(f, "r9"),
            Self::R10 => write!(f, "r10"),
            Self::R11 => write!(f, "r11"),
            Self::R12 => write!(f, "r12"),
            Self::R13 => write!(f, "r13"),
            Self::R14 => write!(f, "r14"),
            Self::R15 => write!(f, "r15"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Size {
    Byte,
    Word,
    Dword,
    Qword,
}

impl Size {
    pub fn from_bytes(bytes: usize) -> Option<Self> {
        match bytes {
            1 => Some(Self::Byte),
            2 => Some(Self::Word),
            4 => Some(Self::Dword),
            8 => Some(Self::Qword),
            _ => None,
        }
    }
}

impl Display for Size {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Byte => write!(f, "byte"),
            Self::Word => write!(f, "word"),
            Self::Dword => write!(f, "dword"),
            Self::Qword => write!(f, "qword"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypedArgument {
    pub info: TypeInfo,
    pub arg: Argument,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Argument {
    Register(Register),
    Uint(u64),
    Float32(f32),
    Float64(f64),
    Identifier(String),
    Memory {
        base: Option<Register>,
        index: Option<Register>,
        size: Option<u64>,
        offset: Option<u64>,
    },
    BasicBlock(BasicBlock),
}

impl Argument {
    pub fn is_register(&self) -> bool {
        matches!(self, Argument::Register(..))
    }

    pub fn is_literal(&self) -> bool {
        matches!(
            self,
            Argument::Uint(..)
                | Argument::Float32(..)
                | Argument::Float64(..)
                | Argument::Identifier(..)
                | Argument::BasicBlock(..)
        )
    }

    pub fn is_address(&self) -> bool {
        matches!(self, Argument::Memory { .. })
    }
}

impl From<Register> for Argument {
    fn from(arg: Register) -> Self {
        Self::Register(arg)
    }
}

impl From<u64> for Argument {
    fn from(arg: u64) -> Self {
        Self::Uint(arg)
    }
}

impl From<f32> for Argument {
    fn from(arg: f32) -> Self {
        Self::Float32(arg)
    }
}

impl From<f64> for Argument {
    fn from(arg: f64) -> Self {
        Self::Float64(arg)
    }
}

impl From<String> for Argument {
    fn from(arg: String) -> Self {
        Self::Identifier(arg)
    }
}

impl From<Address> for Argument {
    fn from(arg: Address) -> Self {
        Self::Memory {
            base: arg.base,
            index: arg.index,
            size: arg.size,
            offset: arg.offset,
        }
    }
}

impl From<BasicBlock> for Argument {
    fn from(arg: BasicBlock) -> Self {
        Self::BasicBlock(arg)
    }
}

impl From<RegisterPurpose> for Argument {
    fn from(arg: RegisterPurpose) -> Self {
        match arg {
            RegisterPurpose::Allocated(reg) => Self::from(reg),
            RegisterPurpose::Spilled(offset) => Self::from(Register::rbp() - offset),
        }
    }
}

impl Display for Argument {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Register(r) => write!(f, "{}", r),
            Self::Uint(i) => write!(f, "{}", i),
            Self::Float32(x) => write!(f, "__?float32?__({})", x),
            Self::Float64(x) => write!(f, "__?float64?__({})", x),
            Self::Identifier(i) => write!(f, "{}", i),
            Self::Memory {
                base,
                index,
                size,
                offset,
            } => {
                write!(f, "[")?;
                if let Some(base) = base {
                    write!(f, "{}", base)?;
                }
                if let Some(index) = index {
                    if base.is_some() {
                        write!(f, " + ")?;
                    }
                    write!(f, "{}", index)?;
                    if let Some(size) = size {
                        write!(f, " * {}", size)?;
                    }
                }
                if let Some(offset) = offset {
                    let offset = *offset as i64;
                    let (offset, sign) = if offset < 0 {
                        (-offset as u64, '-')
                    } else {
                        (offset as u64, '+')
                    };
                    if base.is_some() || index.is_some() {
                        write!(f, " {} ", sign)?;
                    }
                    write!(f, "{}", offset)?;
                }
                write!(f, "]")
            }
            Self::BasicBlock(bb) => write!(f, "._{}", bb.0),
        }
    }
}

#[derive(Debug)]
pub struct LineBuilder<'a> {
    output: &'a mut Vec<String>,
    label: Option<String>,
    opcode: Option<&'static str>,
    size: Option<Size>,
    arguments: SmallVec<[Argument; 4]>,
}

impl<'a> LineBuilder<'a> {
    pub fn label(mut self, label: String) -> Self {
        self.label = Some(label);
        self
    }

    pub fn opcode(mut self, opcode: &'static str) -> Self {
        self.opcode = Some(opcode);
        self
    }

    pub fn size(mut self, size: Size) -> Self {
        self.size = Some(size);
        self
    }

    pub fn argument<T: Into<Argument>>(mut self, arg: T) -> Self {
        self.arguments.push(arg.into());
        self
    }

    pub fn build(self) -> fmt::Result {
        let mut line = String::new();
        if let Some(label) = self.label {
            write!(&mut line, ".{}:", label)?;
            if self.opcode.is_some() {
                write!(&mut line, "\n")?;
            }
        }
        if let Some(opcode) = self.opcode {
            write!(&mut line, "    {} ", opcode)?;
        }
        if let Some(size) = self.size {
            write!(&mut line, "{} ", size)?;
        }
        if let Some(arg) = self.arguments.get(0) {
            write!(&mut line, "{}", arg)?;
        }
        for arg in self.arguments.iter().skip(1) {
            write!(&mut line, ", {}", arg)?;
        }
        self.output.push(line);
        Ok(())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct BasicBlock(u64);

#[derive(Debug)]
pub struct BasicBlockBuilder {
    block: Vec<String>,
    bb: BasicBlock,
}

impl BasicBlockBuilder {
    pub fn instruction(&mut self) -> LineBuilder<'_> {
        LineBuilder {
            output: &mut self.block,
            label: None,
            opcode: None,
            size: None,
            arguments: SmallVec::new(),
        }
    }
}

pub struct FunctionBuilder {
    name: String,
    parameters: SmallVec<[TypeInfo; 6]>,
    basic_blocks: HashMap<BasicBlock, BasicBlockBuilder>,
    block_list: Vec<BasicBlock>,
    bb: BasicBlock,
    export: bool,
    naked: bool,
    locals: HashMap<BindingPrototype, Interval>,
    allocator: Allocator,
    block_listing: Vec<BasicBlockPrototype>,
}

impl FunctionBuilder {
    pub fn new(
        name: String,
        parameters: SmallVec<[TypeInfo; 6]>,
        pool: Vec<Register>,
        block_listing: Vec<BasicBlockPrototype>,
    ) -> Self {
        Self {
            name,
            parameters,
            basic_blocks: HashMap::new(),
            block_list: Vec::new(),
            bb: BasicBlock(0),
            export: false,
            naked: false,
            locals: HashMap::new(),
            allocator: Allocator::new(pool),
            block_listing,
        }
    }

    pub fn export(&mut self, export: bool) {
        self.export = export;
    }

    pub fn naked(&mut self, naked: bool) {
        self.naked = naked;
    }

    pub fn is_naked(&mut self) -> bool {
        self.naked
    }

    pub fn parameters(&self) -> &[TypeInfo] {
        &self.parameters
    }

    pub fn add_local(
        &mut self,
        binding: BindingPrototype,
        ti: TypeInfo,
        start: Lifetime,
        end: Lifetime,
    ) -> &mut Self {
        self.allocator.add(Interval::new(binding, ti, start, end));
        self
    }

    pub fn add_stack(
        &mut self,
        binding: BindingPrototype,
        ti: TypeInfo,
        start: Lifetime,
        end: Lifetime,
    ) -> &mut Self {
        self.allocator
            .add(Interval::on_stack(binding, ti, start, end));
        self
    }

    pub fn allocate(&mut self) -> &mut Self {
        self.allocator.allocate(&self.block_listing);
        let locals = self.allocator.iter().map(|i| (i.name, *i));
        self.locals.extend(locals);
        self
    }

    pub fn local(&self, b: BindingPrototype) -> Option<&Interval> {
        self.locals.get(&b)
    }

    pub fn local_size(&self) -> u64 {
        self.allocator.size()
    }

    pub fn build(mut self, call_conv: &dyn CallConv) -> String {
        call_conv.begin(&mut self).expect("begin failed");
        call_conv.end(&mut self).expect("end failed");

        let mut output = String::new();
        if self.export {
            write!(&mut output, "global {}\n", self.name).expect("formatting failed");
        }
        write!(&mut output, "{}:\n", self.name).expect("formatting failed");
        for bb in self.block_list {
            let num = bb.0;
            let bb = &self.basic_blocks[&bb];
            write!(&mut output, "._{}:\n", num).expect("formatting failed");
            for line in &bb.block {
                write!(&mut output, "{}\n", line).expect("formatting failed");
            }
        }
        write!(&mut output, "\n").expect("formatting failed");
        output
    }

    pub fn add_basic_block(&mut self) -> BasicBlock {
        let bb = self.bb;
        self.bb.0 += 1;
        self.block_list.push(bb);
        self.basic_blocks.insert(
            bb,
            BasicBlockBuilder {
                block: Vec::new(),
                bb,
            },
        );
        bb
    }

    pub fn add_basic_block_at_start(&mut self) -> BasicBlock {
        let bb = self.bb;
        self.bb.0 += 1;
        self.block_list.insert(0, bb);
        self.basic_blocks.insert(
            bb,
            BasicBlockBuilder {
                block: Vec::new(),
                bb,
            },
        );
        bb
    }

    pub fn add_basic_block_before(&mut self, bb: BasicBlock) -> BasicBlock {
        let idx = self.block_list.iter().position(|bb1| bb1 == &bb).unwrap();
        let bb = self.bb;
        self.bb.0 += 1;
        self.block_list.insert(idx, bb);
        self.basic_blocks.insert(
            bb,
            BasicBlockBuilder {
                block: Vec::new(),
                bb,
            },
        );
        bb
    }

    pub fn add_basic_block_after(&mut self, bb: BasicBlock) -> BasicBlock {
        let idx = self.block_list.iter().position(|bb1| bb1 == &bb).unwrap();
        let bb = self.bb;
        self.bb.0 += 1;
        self.block_list.insert(idx + 1, bb);
        self.basic_blocks.insert(
            bb,
            BasicBlockBuilder {
                block: Vec::new(),
                bb,
            },
        );
        bb
    }

    pub fn basic_block(&self, bb: BasicBlock) -> &BasicBlockBuilder {
        &self.basic_blocks[&bb]
    }

    pub fn basic_block_mut(&mut self, bb: BasicBlock) -> &mut BasicBlockBuilder {
        self.basic_blocks.get_mut(&bb).unwrap()
    }
}

#[allow(dead_code)]
pub struct Extern {
    name: String,
    result_type: TypeInfo,
    param_types: SmallVec<[TypeInfo; 6]>,
    call_conv: Box<dyn CallConv>,
}

impl Extern {
    pub fn build(self) -> String {
        format!("extern {}\n", self.name)
    }
}

#[derive(Debug)]
pub struct Data {
    name: String,
    info: TypeInfo,
    data: Vec<u8>,
}

impl Data {
    pub fn build(self) -> String {
        let mut output = String::new();
        write!(&mut output, "align {}\n{}: db", self.info.align, self.name)
            .expect("formatting failed");
        if let Some(byte) = self.data.get(0) {
            write!(&mut output, "{}", byte).expect("formatting failed");
        }
        for byte in self.data.iter().skip(1) {
            write!(&mut output, ",{}", byte).expect("formatting failed");
        }
        output
    }
}

#[derive(Debug)]
pub struct Bss {
    name: String,
    info: TypeInfo,
}

impl Bss {
    pub fn build(self) -> String {
        format!(
            "align {}\n{}: resb {}",
            self.info.align, self.name, self.info.size
        )
    }
}

#[derive(Default)]
pub struct Global {
    externs: HashMap<String, Extern>,
    data: Vec<Data>,
    bss: Vec<Bss>,
}

impl Global {
    pub fn add_extern(&mut self, ext: Extern) {
        self.externs.insert(ext.name.clone(), ext);
    }

    pub fn add_data(&mut self, data: Data) {
        self.data.push(data);
    }

    pub fn add_bss(&mut self, bss: Bss) {
        self.bss.push(bss);
    }
}

pub struct CodeBuilder {
    functions: HashMap<String, FunctionBuilder>,
}

impl CodeBuilder {
    pub fn new() -> Self {
        Self {
            functions: HashMap::new(),
        }
    }

    pub fn build(self, call_conv: &dyn CallConv) -> String {
        self.functions
            .into_iter()
            .map(|(_, func)| func.build(call_conv))
            .collect()
    }

    pub fn add_function(
        &mut self,
        call_conv: &dyn CallConv,
        name: String,
        param_types: &[TypeInfo],
        listing: Vec<BasicBlockPrototype>,
    ) -> &mut FunctionBuilder {
        let free = call_conv.free(param_types);
        self.functions.entry(name.clone()).or_insert_with(|| {
            FunctionBuilder::new(name, param_types.iter().copied().collect(), free, listing)
        })
    }

    pub fn function_mut(&mut self, name: &str) -> Option<&mut FunctionBuilder> {
        self.functions.get_mut(name)
    }
}

pub struct ProgramBuilder {
    pub global: Global,
    pub codegen: CodeBuilder,
    pub call_conv: Box<dyn CallConv>,
}

impl ProgramBuilder {
    pub fn new(call_conv: impl CallConv) -> Self {
        Self {
            global: Global::default(),
            codegen: CodeBuilder::new(),
            call_conv: Box::new(call_conv),
        }
    }

    pub fn build(self) -> String {
        let externs = self
            .global
            .externs
            .into_iter()
            .map(|(_, ext)| ext.build())
            .collect::<String>();
        let data = self
            .global
            .data
            .into_iter()
            .map(Data::build)
            .collect::<String>();
        let bss = self
            .global
            .bss
            .into_iter()
            .map(Bss::build)
            .collect::<String>();
        let text = self.codegen.build(&*self.call_conv);

        format!(
            "    bits 64\n    section .text\n{}\n{}    section .data\n{}    section .bss\n{}",
            externs, text, data, bss,
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lir;

    #[test]
    fn codegen() {
        let mut program = ProgramBuilder::new(AmpCall64);

        let start = program.codegen.add_function(
            &*program.call_conv,
            "_start".to_string(),
            &[],
            vec![BasicBlockPrototype::with_start(0, 0)],
        );
        start.export(true);
        start.naked(true);
        let bb = start.add_basic_block();
        program
            .call_conv
            .build_call(start, bb, "main".to_string(), TypeInfo::new(8, 8), &[])
            .unwrap();
        let basic_block = start.basic_block_mut(bb);
        basic_block
            .instruction()
            .opcode("mov")
            .argument(Register::rax())
            .argument(60)
            .build()
            .unwrap();
        basic_block
            .instruction()
            .opcode("mov")
            .argument(Register::rdi())
            .argument(0)
            .build()
            .unwrap();
        basic_block.instruction().opcode("syscall").build().unwrap();

        let main = program.codegen.add_function(
            &*program.call_conv,
            "main".to_string(),
            &[],
            vec![BasicBlockPrototype::with_start(0, 0)],
        );
        let bb = main.add_basic_block();
        let a = BindingPrototype::new(0, 0);
        let b = BindingPrototype::new(0, 1);
        main.add_local(
            a,
            TypeInfo::new(8, 8),
            Lifetime::new(lir::BasicBlock::new(0), 0),
            Lifetime::new(lir::BasicBlock::new(0), 1),
        )
        .add_stack(
            b,
            TypeInfo::new(8, 8),
            Lifetime::new(lir::BasicBlock::new(0), 0),
            Lifetime::new(lir::BasicBlock::new(0), 1),
        )
        .allocate();
        let a = main.local(a).copied().unwrap();
        let b = main.local(b).copied().unwrap();
        let basic_block = main.basic_block_mut(bb);
        basic_block
            .instruction()
            .opcode("mov")
            .argument(a.reg)
            .argument(0)
            .build()
            .unwrap();
        basic_block
            .instruction()
            .opcode("mov")
            .argument(b.reg)
            .argument(a.reg)
            .build()
            .unwrap();
        program
            .call_conv
            .build_ret(
                main,
                bb,
                TypedArgument {
                    info: TypeInfo::new(8, 8),
                    arg: 0_u64.into(),
                },
            )
            .unwrap();

        let code = program.build();
        println!("{}", code);
    }
}
