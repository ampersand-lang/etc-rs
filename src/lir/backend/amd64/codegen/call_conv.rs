use failure::Fallible;
use hashbrown::HashSet;

use crate::types::TypeInfo;
use crate::utils::IntPtr;

use super::*;

pub trait CallConv: Send + Sync + 'static {
    fn name(&self) -> &'static str;
    fn free(&self, args: &[TypeInfo]) -> Vec<Register>;
    fn begin(&self, builder: &mut FunctionBuilder) -> Fallible<()>;
    fn end(&self, builder: &mut FunctionBuilder) -> Fallible<()>;
    fn argument(&self, builder: &FunctionBuilder, arg: i32) -> Fallible<Argument>;
    fn build_call(
        &self,
        builder: &mut FunctionBuilder,
        bb: BasicBlock,
        func: Argument,
        result: TypeInfo,
        args: &[TypedArgument],
    ) -> Fallible<()>;
    fn build_ret(
        &self,
        builder: &mut FunctionBuilder,
        bb: BasicBlock,
        value: TypedArgument,
    ) -> Fallible<()>;
}

pub struct AmpCall64;

impl CallConv for AmpCall64 {
    fn name(&self) -> &'static str {
        "amp-call_amd64"
    }

    fn free(&self, args: &[TypeInfo]) -> Vec<Register> {
        let mut free = HashSet::with_capacity(16);
        free.insert(Register::rcx());
        free.insert(Register::rbx());
        free.insert(Register::rdi());
        free.insert(Register::rsi());
        free.insert(Register::r8());
        free.insert(Register::r9());
        free.insert(Register::r10());
        free.insert(Register::r11());
        free.insert(Register::r12());
        free.insert(Register::r13());
        free.insert(Register::r14());
        free.insert(Register::r15());
        let params = &[
            Register::rdi(),
            Register::rsi(),
            Register::rdx(),
            Register::rcx(),
            Register::r8(),
            Register::r9(),
        ];
        let mut idx = 0;
        for arg in args {
            if idx == params.len() {
                break;
            }
            if arg.size <= 8 {
                free.remove(&params[idx]);
                idx += 1;
            }
        }
        free.into_iter().collect()
    }

    fn begin(&self, builder: &mut FunctionBuilder) -> Fallible<()> {
        if !builder.is_naked() {
            let size = builder.local_size();
            let bb = builder.add_basic_block_at_start();
            let bb = builder.basic_block_mut(bb);
            bb.instruction()
                .opcode("push")
                .argument(Register::rbp())
                .build()?;
            bb.instruction()
                .opcode("mov")
                .argument(Register::rbp())
                .argument(Register::rsp())
                .build()?;
            if size != 0 {
                bb.instruction()
                    .opcode("sub")
                    .argument(Register::rsp())
                    .argument(size)
                    .build()?;
            }
        }
        Ok(())
    }

    fn end(&self, builder: &mut FunctionBuilder) -> Fallible<()> {
        if !builder.is_naked() {
            let bb = builder.add_basic_block();
            let bb = builder.basic_block_mut(bb);
            bb.instruction().label("_ret".to_string()).build()?;
            bb.instruction()
                .opcode("mov")
                .argument(Register::rsp())
                .argument(Register::rbp())
                .build()?;
            bb.instruction()
                .opcode("pop")
                .argument(Register::rbp())
                .build()?;
            bb.instruction().opcode("ret").build()?;
        }
        Ok(())
    }

    fn argument(&self, builder: &FunctionBuilder, arg: i32) -> Fallible<Argument> {
        let arg = arg as usize;
        let params = builder.parameters();
        if arg >= params.len() {
            Err(failure::err_msg("invalid arity"))
        } else {
            let target = &[
                Register::rdi(),
                Register::rsi(),
                Register::rdx(),
                Register::rcx(),
                Register::r8(),
                Register::r9(),
            ];
            let mut register = 0;
            let mut offset = 0;
            for param in &params[..arg] {
                let size = param.size.max(8);
                offset = offset.align_up(param.align);
                offset += size;
                if size <= 8 {
                    register += 1;
                }
            }
            if register < target.len() {
                Ok(target[register].into())
            } else {
                Ok((Register::rbp() - offset as u64).into())
            }
        }
    }

    fn build_call(
        &self,
        builder: &mut FunctionBuilder,
        bb: BasicBlock,
        func: Argument,
        result: TypeInfo,
        args: &[TypedArgument],
    ) -> Fallible<()> {
        let bb = builder.basic_block_mut(bb);
        let target = &[
            Register::rdi(),
            Register::rsi(),
            Register::rdx(),
            Register::rcx(),
            Register::r8(),
            Register::r9(),
        ];
        let mut tgt_idx = 0;
        if result.size > 8 {
            tgt_idx += 1;
            // TODO: move this sub to local variable allocator
            bb.instruction()
                .opcode("sub")
                .argument(Register::rsp())
                .argument(result.size as u64)
                .build()?;
            bb.instruction()
                .opcode("mov")
                .argument(Register::rdi())
                .argument(Register::rsp())
                .build()?;
        }
        let mut stack_size = 0;
        for arg in args.iter().take(6) {
            if arg.info.size > 8 {
                stack_size = stack_size.align_up(arg.info.align);
                stack_size += arg.info.size;
            }
        }
        for arg in args.iter().skip(6) {
            stack_size = stack_size.align_up(arg.info.align);
            stack_size += arg.info.size;
        }
        if stack_size != 0 {
            bb.instruction()
                .opcode("sub")
                .argument(Register::rsp())
                .argument(stack_size as u64)
                .build()?;
        }
        let mut stack_offset = 0;
        for value in args.iter().take(6) {
            if value.info.size <= 8 {
                bb.instruction()
                    .opcode("mov")
                    .argument(target[tgt_idx])
                    .argument(value.arg.clone())
                    .build()?;
            } else {
                stack_offset = stack_offset.align_up(value.info.align);
                match value.arg {
                    Argument::Memory {
                        base,
                        index,
                        size,
                        offset,
                    } => {
                        let stack_offset = stack_offset as u64;
                        let size = if index.is_some() {
                            size.unwrap_or(1)
                        } else {
                            size.unwrap_or(0)
                        };
                        let offset = offset.unwrap_or(0);
                        let address = match (base, index, size, offset) {
                            (Some(base), _, 0, offset) => base + offset,
                            (Some(base), Some(size), n, offset) => base + size * n + offset,
                            (None, Some(size), n, offset) => size * n + offset,
                            (Some(base), None, _, offset) => base + offset,
                            (None, None, _, offset) => Address::from(offset),
                        };
                        for i in 0..size.align_up(8) / 8 {
                            let offset = i * 8;
                            let mut address = address;
                            address.offset =
                                address.offset.map(|off| off + offset).or(Some(offset));
                            bb.instruction()
                                .opcode("mov")
                                .argument(Register::rax())
                                .argument(address)
                                .build()?;
                            bb.instruction()
                                .opcode("mov")
                                .argument(Register::rsp() + (stack_offset + offset))
                                .argument(Register::rax())
                                .build()?;
                            bb.instruction()
                                .opcode("lea")
                                .argument(target[tgt_idx])
                                .argument(Register::rsp() + (stack_offset + offset))
                                .build()?;
                        }
                    }
                    // other argument types are <= 8 bytes
                    _ => unimplemented!(),
                }
                stack_offset += value.info.size;
            }
        }
        for value in args.iter().skip(6) {
            stack_offset = stack_offset.align_up(value.info.align);
            match value.arg {
                Argument::Memory {
                    base,
                    index,
                    size,
                    offset,
                } => {
                    let stack_offset = stack_offset as u64;
                    let size = if index.is_some() {
                        size.unwrap_or(1)
                    } else {
                        size.unwrap_or(0)
                    };
                    let offset = offset.unwrap_or(0);
                    let address = match (base, index, size, offset) {
                        (Some(base), _, 0, offset) => base + offset,
                        (Some(base), Some(size), n, offset) => base + size * n + offset,
                        (None, Some(size), n, offset) => size * n + offset,
                        (Some(base), None, _, offset) => base + offset,
                        (None, None, _, offset) => Address::from(offset),
                    };
                    for i in 0..size.align_up(8) / 8 {
                        let offset = i * 8;
                        let mut address = address;
                        address.offset = address.offset.map(|off| off + offset).or(Some(offset));
                        bb.instruction()
                            .opcode("mov")
                            .argument(Register::rax())
                            .argument(address)
                            .build()?;
                        bb.instruction()
                            .opcode("mov")
                            .argument(Register::rsp() + (stack_offset + offset))
                            .argument(Register::rax())
                            .build()?;
                    }
                }
                // other argument types are <= 8 bytes
                _ => unimplemented!(),
            }
            stack_offset += value.info.size;
        }
        bb.instruction().opcode("call").argument(func).build()?;
        if stack_size != 0 {
            bb.instruction()
                .opcode("add")
                .argument(Register::rsp())
                .argument(stack_size as u64)
                .build()?;
        }
        Ok(())
    }

    fn build_ret(
        &self,
        builder: &mut FunctionBuilder,
        bb: BasicBlock,
        value: TypedArgument,
    ) -> Fallible<()> {
        let is_naked = builder.is_naked();
        let bb = builder.basic_block_mut(bb);
        if value.info.size <= 8 {
            bb.instruction()
                .opcode("mov")
                .argument(Register::rax())
                .argument(value.arg)
                .build()?;
        } else {
            match value.arg {
                Argument::Memory {
                    base,
                    index,
                    size,
                    offset,
                } => {
                    let size = if index.is_some() {
                        size.unwrap_or(1)
                    } else {
                        size.unwrap_or(0)
                    };
                    let offset = offset.unwrap_or(0);
                    let address = match (base, index, size, offset) {
                        (Some(base), _, 0, offset) => base + offset,
                        (Some(base), Some(size), n, offset) => base + size * n + offset,
                        (None, Some(size), n, offset) => size * n + offset,
                        (Some(base), None, _, offset) => base + offset,
                        (None, None, _, offset) => Address::from(offset),
                    };
                    for i in 0..size.align_up(8) / 8 {
                        let offset = i * 8;
                        let mut address = address;
                        address.offset = address.offset.map(|off| off + offset).or(Some(offset));
                        bb.instruction()
                            .opcode("mov")
                            .argument(Register::rax())
                            .argument(address)
                            .build()?;
                        bb.instruction()
                            .opcode("mov")
                            .argument(Register::rdi() + offset)
                            .argument(Register::rax())
                            .build()?;
                    }
                }
                // other argument types are <= 8 bytes
                _ => unimplemented!(),
            }
        }
        if is_naked {
            bb.instruction().opcode("ret").build()?;
        } else {
            bb.instruction()
                .opcode("jmp")
                .argument("._ret".to_string())
                .build()?;
        }
        Ok(())
    }
}
