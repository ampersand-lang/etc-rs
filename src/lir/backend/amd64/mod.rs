use failure::Fallible;
use smallvec::SmallVec;

use crate::assets::{Resources, World};
use crate::ast::{Node, RootNode};
use crate::lir::{
    context::ExecutionContext, target::Target, Instruction, Value, ICMP_EQ, ICMP_GE, ICMP_GT,
    ICMP_LE, ICMP_LT, ICMP_NE,
};
use crate::types::{NamedType, NonConcrete, Type, TypeInfo};

use super::Backend;

pub use self::codegen::*;

mod codegen;

pub struct Amd64 {
    builder: Option<ProgramBuilder>,
}

impl Default for Amd64 {
    fn default() -> Self {
        Self {
            builder: Some(ProgramBuilder::new(AmpCall64)),
        }
    }
}

impl Backend for Amd64 {
    fn name(&self) -> &'static str {
        "amd64/x86_64"
    }

    fn build(&mut self) -> Fallible<Vec<u8>> {
        self.builder
            .take()
            .map(|builder| Ok(builder.build().into_bytes()))
            .unwrap_or_else(|| Ok(Vec::new()))
    }

    fn run(&mut self, world: &World) -> Fallible<()> {
        let target = world.global::<Target>();
        let roots: Resources<&mut RootNode> = world.resources();
        let mut threads: Resources<&mut ExecutionContext> = world.resources();
        let named_types: Resources<&NamedType> = world.resources();
        let nodes: Resources<&Node> = world.resources();

        let program = self.builder.as_mut().expect("program was already built");

        for (_, root_node) in roots.iter_mut::<RootNode>() {
            let root = nodes.get::<Node>(root_node.0).unwrap();
            let ctx = threads
                .remove::<ExecutionContext>(root.thread.unwrap())
                .unwrap();
            let functions = ctx
                .iter()
                .map(|func| func.name.to_string())
                .collect::<Vec<_>>();
            for func in ctx.functions() {
                let params = func
                    .param_types
                    .iter()
                    .map(|t| t.type_info(&named_types, &target))
                    .collect::<SmallVec<[_; 6]>>();
                let builder = program.codegen.add_function(
                    &*program.call_conv,
                    func.name.to_string(),
                    &params,
                    func.blocks.clone(),
                );
                for ir in &func.body {
                    match ir.instr {
                        Instruction::Alloca => {
                            let t = match ir.args[0].val {
                                Value::Type(t) => t,
                                _ => todo!(),
                            };
                            let t = t.type_info(&named_types, &target);
                            let n = match ir.args[1].val {
                                Value::Uint(i) => i as usize,
                                _ => todo!(),
                            };
                            let (start, end) = func.lifetime(ir.binding.unwrap());
                            let r = ir.binding.unwrap();
                            if func.is_ref(r) {
                                builder.add_stack(
                                    r,
                                    TypeInfo::new(t.size * n, t.align),
                                    start,
                                    end,
                                );
                            } else {
                                builder.add_local(
                                    r,
                                    TypeInfo::new(t.size * n, t.align),
                                    start,
                                    end,
                                );
                            }
                        }
                        Instruction::Load => {
                            let t = ir.typ.type_info(&named_types, &target);
                            let (start, end) = func.lifetime(ir.binding.unwrap());
                            builder.add_local(ir.binding.unwrap(), t, start, end);
                        }
                        Instruction::Call => {
                            let t = ir.typ.type_info(&named_types, &target);
                            let (start, end) = func.lifetime(ir.binding.unwrap());
                            builder.add_local(ir.binding.unwrap(), t, start, end);
                        }
                        Instruction::Add
                        | Instruction::Sub
                        | Instruction::Mul
                        | Instruction::Div
                        | Instruction::Rem
                        | Instruction::BitAnd
                        | Instruction::BitOr
                        | Instruction::BitXor
                        | Instruction::Icmp => {
                            let t = ir.typ.type_info(&named_types, &target);
                            let (start, end) = func.lifetime(ir.binding.unwrap());
                            builder.add_local(ir.binding.unwrap(), t, start, end);
                        }
                        _ => {}
                    }
                }
                builder.allocate();
                let bb = builder.add_basic_block();
                for ir in func.body {
                    match ir.instr {
                        Instruction::Alloca => {}
                        Instruction::Store => {
                            let t = ir.args[1].typ;
                            let t = t.type_info(&named_types, &target);
                            if t.size > 8 {
                                todo!()
                            } else {
                                let target = match ir.args[0].val {
                                    Value::Register(r) => *builder.local(r).unwrap(),
                                    _ => todo!(),
                                };
                                match ir.args[1].val {
                                    Value::Bool(p) => {
                                        let bb = builder.basic_block_mut(bb);
                                        bb.instruction()
                                            .opcode("mov")
                                            .size(Size::from_bytes(1).unwrap())
                                            .argument(target.reg)
                                            .argument(p as u8 as u64)
                                            .build()?;
                                    }
                                    Value::Uint(int) => {
                                        let bb = builder.basic_block_mut(bb);
                                        bb.instruction()
                                            .opcode("mov")
                                            .size(Size::from_bytes(t.size).unwrap())
                                            .argument(target.reg)
                                            .argument(int)
                                            .build()?;
                                    }
                                    Value::Arg(r) => {
                                        let source = program.call_conv.argument(builder, r)?;
                                        if target.reg.is_spilled() && source.is_address() {
                                            let bb = builder.basic_block_mut(bb);
                                            bb.instruction()
                                                .opcode("push")
                                                .size(Size::from_bytes(t.size).unwrap())
                                                .argument(source)
                                                .build()?;
                                            bb.instruction()
                                                .opcode("pop")
                                                .size(Size::from_bytes(t.size).unwrap())
                                                .argument(target.reg)
                                                .build()?;
                                        } else {
                                            let bb = builder.basic_block_mut(bb);
                                            bb.instruction()
                                                .opcode("mov")
                                                .argument(target.reg)
                                                .argument(source)
                                                .build()?;
                                        }
                                    }
                                    Value::Register(r) => {
                                        let source = *builder.local(r).unwrap();
                                        if target.reg.is_spilled() && source.reg.is_spilled() {
                                            let bb = builder.basic_block_mut(bb);
                                            bb.instruction()
                                                .opcode("push")
                                                .size(Size::from_bytes(t.size).unwrap())
                                                .argument(source.reg)
                                                .build()?;
                                            bb.instruction()
                                                .opcode("pop")
                                                .size(Size::from_bytes(t.size).unwrap())
                                                .argument(target.reg)
                                                .build()?;
                                        } else {
                                            let bb = builder.basic_block_mut(bb);
                                            bb.instruction()
                                                .opcode("mov")
                                                .argument(target.reg)
                                                .argument(source.reg)
                                                .build()?;
                                        }
                                    }
                                    Value::Function(i) => {
                                        let source = functions[i.idx as usize].to_string();
                                        let bb = builder.basic_block_mut(bb);
                                        bb.instruction()
                                            .opcode("mov")
                                            .argument(target.reg)
                                            .argument(source)
                                            .build()?;
                                    }
                                    _ => todo!(),
                                }
                            }
                        }
                        Instruction::Load => {
                            let t = ir.typ.type_info(&named_types, &target);
                            if t.size > 8 {
                                todo!()
                            } else {
                                let target = *builder.local(ir.binding.unwrap()).unwrap();
                                match ir.args[0].val {
                                    Value::Register(r) => {
                                        let source = *builder.local(r).unwrap();
                                        if target.reg.is_spilled() && source.reg.is_spilled() {
                                            let bb = builder.basic_block_mut(bb);
                                            bb.instruction()
                                                .opcode("push")
                                                .size(Size::from_bytes(t.size).unwrap())
                                                .argument(source.reg)
                                                .build()?;
                                            bb.instruction()
                                                .opcode("pop")
                                                .size(Size::from_bytes(t.size).unwrap())
                                                .argument(target.reg)
                                                .build()?;
                                        } else {
                                            let bb = builder.basic_block_mut(bb);
                                            bb.instruction()
                                                .opcode("mov")
                                                .argument(target.reg)
                                                .argument(source.reg)
                                                .build()?;
                                        }
                                    }
                                    _ => todo!(),
                                }
                            }
                        }
                        Instruction::Call => {
                            let t = ir.typ.type_info(&named_types, &target);

                            let param_types = match ir.args[0].typ.concrete {
                                NonConcrete::Type(handle) => named_types.get(handle).unwrap(),
                                _ => todo!(),
                            };
                            let param_types = match param_types.t {
                                Type::Function {
                                    ref param_types, ..
                                } => param_types,
                                _ => todo!(),
                            };

                            let func_ref = match ir.args[0].val {
                                Value::Arg(r) => program.call_conv.argument(builder, r)?,
                                Value::Register(r) => builder.local(r).unwrap().reg.into(),
                                Value::Function(i) => functions[i.idx as usize].to_string().into(),
                                _ => todo!(),
                            };
                            let mut arguments = SmallVec::<[_; 6]>::new();
                            for (idx, &arg) in ir.args[2..].iter().enumerate() {
                                let info = param_types[idx].type_info(&named_types, &target);
                                let arg = match arg.val {
                                    Value::Bool(p) => (p as u8 as u64).into(),
                                    Value::Uint(i) => i.into(),
                                    Value::Arg(r) => program.call_conv.argument(builder, r)?,
                                    Value::Register(r) => builder.local(r).unwrap().reg.into(),
                                    Value::Function(i) => {
                                        functions[i.idx as usize].to_string().into()
                                    }
                                    _ => todo!(),
                                };
                                arguments.push(TypedArgument { info, arg });
                            }
                            program
                                .call_conv
                                .build_call(builder, bb, func_ref, t, &arguments)?;
                        }
                        Instruction::Return => {
                            let t = ir.args[0].typ;
                            let t = t.type_info(&named_types, &target);
                            let result = match ir.args[0].val {
                                Value::Bool(p) => TypedArgument {
                                    info: t,
                                    arg: (p as u8 as u64).into(),
                                },
                                Value::Uint(u) => TypedArgument {
                                    info: t,
                                    arg: u.into(),
                                },
                                Value::Arg(r) => {
                                    let source = program.call_conv.argument(builder, r)?;
                                    TypedArgument {
                                        info: t,
                                        arg: source,
                                    }
                                }
                                Value::Register(r) => {
                                    let source = builder.local(r).unwrap();
                                    TypedArgument {
                                        info: t,
                                        arg: source.reg.into(),
                                    }
                                }
                                _ => todo!(),
                            };
                            program.call_conv.build_ret(builder, bb, result)?;
                        }
                        Instruction::Add => {
                            let t = ir.typ.type_info(&named_types, &target);
                            if t.size != 8 {
                                todo!();
                            }
                            let arg0 = match ir.args[0].val {
                                Value::Arg(r) => {
                                    let source = program.call_conv.argument(builder, r)?;
                                    TypedArgument {
                                        info: t,
                                        arg: source.into(),
                                    }
                                }
                                Value::Register(r) => {
                                    let source = builder.local(r).unwrap();
                                    TypedArgument {
                                        info: t,
                                        arg: source.reg.into(),
                                    }
                                }
                                Value::Uint(n) => TypedArgument {
                                    info: t,
                                    arg: n.into(),
                                },
                                _ => todo!(),
                            };
                            let arg1 = match ir.args[1].val {
                                Value::Arg(r) => {
                                    let source = program.call_conv.argument(builder, r)?;
                                    TypedArgument {
                                        info: t,
                                        arg: source.into(),
                                    }
                                }
                                Value::Register(r) => {
                                    let source = *builder.local(r).unwrap();
                                    TypedArgument {
                                        info: t,
                                        arg: source.reg.into(),
                                    }
                                }
                                Value::Uint(n) => TypedArgument {
                                    info: t,
                                    arg: n.into(),
                                },
                                _ => todo!(),
                            };

                            let target = *builder.local(ir.binding.unwrap()).unwrap();

                            let bb = builder.basic_block_mut(bb);
                            bb.instruction()
                                .opcode("mov")
                                .argument(Register::rax())
                                .argument(arg0.arg)
                                .build()?;
                            bb.instruction()
                                .opcode("add")
                                .argument(Register::rax())
                                .argument(arg1.arg)
                                .build()?;
                            bb.instruction()
                                .opcode("mov")
                                .argument(target.reg)
                                .argument(Register::rax())
                                .build()?;
                        }
                        Instruction::Sub => {
                            let t = ir.typ.type_info(&named_types, &target);
                            if t.size != 8 {
                                todo!();
                            }
                            let arg0 = match ir.args[0].val {
                                Value::Arg(r) => {
                                    let source = program.call_conv.argument(builder, r)?;
                                    TypedArgument {
                                        info: t,
                                        arg: source.into(),
                                    }
                                }
                                Value::Register(r) => {
                                    let source = builder.local(r).unwrap();
                                    TypedArgument {
                                        info: t,
                                        arg: source.reg.into(),
                                    }
                                }
                                Value::Uint(n) => TypedArgument {
                                    info: t,
                                    arg: n.into(),
                                },
                                _ => todo!(),
                            };
                            let arg1 = match ir.args[1].val {
                                Value::Arg(r) => {
                                    let source = program.call_conv.argument(builder, r)?;
                                    TypedArgument {
                                        info: t,
                                        arg: source.into(),
                                    }
                                }
                                Value::Register(r) => {
                                    let source = *builder.local(r).unwrap();
                                    TypedArgument {
                                        info: t,
                                        arg: source.reg.into(),
                                    }
                                }
                                Value::Uint(n) => TypedArgument {
                                    info: t,
                                    arg: n.into(),
                                },
                                _ => todo!(),
                            };

                            let target = *builder.local(ir.binding.unwrap()).unwrap();

                            let bb = builder.basic_block_mut(bb);
                            bb.instruction()
                                .opcode("mov")
                                .argument(Register::rax())
                                .argument(arg0.arg)
                                .build()?;
                            bb.instruction()
                                .opcode("sub")
                                .argument(Register::rax())
                                .argument(arg1.arg)
                                .build()?;
                            bb.instruction()
                                .opcode("mov")
                                .argument(target.reg)
                                .argument(Register::rax())
                                .build()?;
                        }
                        Instruction::Mul => {
                            let t = ir.typ.type_info(&named_types, &target);
                            if t.size != 8 {
                                todo!();
                            }
                            let arg0 = match ir.args[0].val {
                                Value::Arg(r) => {
                                    let source = program.call_conv.argument(builder, r)?;
                                    TypedArgument {
                                        info: t,
                                        arg: source.into(),
                                    }
                                }
                                Value::Register(r) => {
                                    let source = builder.local(r).unwrap();
                                    TypedArgument {
                                        info: t,
                                        arg: source.reg.into(),
                                    }
                                }
                                Value::Uint(n) => TypedArgument {
                                    info: t,
                                    arg: n.into(),
                                },
                                _ => todo!(),
                            };
                            let arg1 = match ir.args[1].val {
                                Value::Arg(r) => {
                                    let source = program.call_conv.argument(builder, r)?;
                                    TypedArgument {
                                        info: t,
                                        arg: source.into(),
                                    }
                                }
                                Value::Register(r) => {
                                    let source = *builder.local(r).unwrap();
                                    TypedArgument {
                                        info: t,
                                        arg: source.reg.into(),
                                    }
                                }
                                Value::Uint(n) => TypedArgument {
                                    info: t,
                                    arg: n.into(),
                                },
                                _ => todo!(),
                            };

                            let target = *builder.local(ir.binding.unwrap()).unwrap();

                            let bb = builder.basic_block_mut(bb);
                            bb.instruction()
                                .opcode("mov")
                                .argument(Register::rax())
                                .argument(arg0.arg)
                                .build()?;
                            bb.instruction()
                                .opcode("imul")
                                .argument(Register::rax())
                                .argument(arg1.arg)
                                .build()?;
                            bb.instruction()
                                .opcode("mov")
                                .argument(target.reg)
                                .argument(Register::rax())
                                .build()?;
                        }
                        Instruction::Div => {
                            let t = ir.typ.type_info(&named_types, &target);
                            if t.size != 8 {
                                todo!();
                            }
                            let arg0 = match ir.args[0].val {
                                Value::Arg(r) => {
                                    let source = program.call_conv.argument(builder, r)?;
                                    TypedArgument {
                                        info: t,
                                        arg: source.into(),
                                    }
                                }
                                Value::Register(r) => {
                                    let source = builder.local(r).unwrap();
                                    TypedArgument {
                                        info: t,
                                        arg: source.reg.into(),
                                    }
                                }
                                Value::Uint(n) => TypedArgument {
                                    info: t,
                                    arg: n.into(),
                                },
                                _ => todo!(),
                            };
                            let arg1 = match ir.args[1].val {
                                Value::Arg(r) => {
                                    let source = program.call_conv.argument(builder, r)?;
                                    TypedArgument {
                                        info: t,
                                        arg: source.into(),
                                    }
                                }
                                Value::Register(r) => {
                                    let source = *builder.local(r).unwrap();
                                    TypedArgument {
                                        info: t,
                                        arg: source.reg.into(),
                                    }
                                }
                                Value::Uint(n) => TypedArgument {
                                    info: t,
                                    arg: n.into(),
                                },
                                _ => todo!(),
                            };

                            let target = *builder.local(ir.binding.unwrap()).unwrap();

                            let bb = builder.basic_block_mut(bb);
                            bb.instruction()
                                .opcode("mov")
                                .argument(Register::rdx())
                                .argument(0)
                                .build()?;
                            bb.instruction()
                                .opcode("mov")
                                .argument(Register::rax())
                                .argument(arg0.arg)
                                .build()?;
                            bb.instruction().opcode("idiv").argument(arg1.arg).build()?;
                            bb.instruction()
                                .opcode("mov")
                                .argument(target.reg)
                                .argument(Register::rax())
                                .build()?;
                        }
                        Instruction::Rem => {
                            let t = ir.typ.type_info(&named_types, &target);
                            if t.size != 8 {
                                todo!();
                            }
                            let arg0 = match ir.args[0].val {
                                Value::Arg(r) => {
                                    let source = program.call_conv.argument(builder, r)?;
                                    TypedArgument {
                                        info: t,
                                        arg: source.into(),
                                    }
                                }
                                Value::Register(r) => {
                                    let source = builder.local(r).unwrap();
                                    TypedArgument {
                                        info: t,
                                        arg: source.reg.into(),
                                    }
                                }
                                Value::Uint(n) => TypedArgument {
                                    info: t,
                                    arg: n.into(),
                                },
                                _ => todo!(),
                            };
                            let arg1 = match ir.args[1].val {
                                Value::Arg(r) => {
                                    let source = program.call_conv.argument(builder, r)?;
                                    TypedArgument {
                                        info: t,
                                        arg: source.into(),
                                    }
                                }
                                Value::Register(r) => {
                                    let source = *builder.local(r).unwrap();
                                    TypedArgument {
                                        info: t,
                                        arg: source.reg.into(),
                                    }
                                }
                                Value::Uint(n) => TypedArgument {
                                    info: t,
                                    arg: n.into(),
                                },
                                _ => todo!(),
                            };

                            let target = *builder.local(ir.binding.unwrap()).unwrap();

                            let bb = builder.basic_block_mut(bb);
                            bb.instruction()
                                .opcode("mov")
                                .argument(Register::rdx())
                                .argument(0)
                                .build()?;
                            bb.instruction()
                                .opcode("mov")
                                .argument(Register::rax())
                                .argument(arg0.arg)
                                .build()?;
                            bb.instruction().opcode("idiv").argument(arg1.arg).build()?;
                            bb.instruction()
                                .opcode("mov")
                                .argument(target.reg)
                                .argument(Register::rdx())
                                .build()?;
                        }
                        Instruction::BitAnd => {
                            let t = ir.typ.type_info(&named_types, &target);
                            let arg0 = match ir.args[0].val {
                                Value::Arg(r) => {
                                    let source = program.call_conv.argument(builder, r)?;
                                    TypedArgument {
                                        info: t,
                                        arg: source.into(),
                                    }
                                }
                                Value::Register(r) => {
                                    let source = builder.local(r).unwrap();
                                    TypedArgument {
                                        info: t,
                                        arg: source.reg.into(),
                                    }
                                }
                                Value::Uint(n) => TypedArgument {
                                    info: t,
                                    arg: n.into(),
                                },
                                Value::Bool(n) => TypedArgument {
                                    info: t,
                                    arg: (n as u8 as u64).into(),
                                },
                                _ => todo!(),
                            };
                            let arg1 = match ir.args[1].val {
                                Value::Arg(r) => {
                                    let source = program.call_conv.argument(builder, r)?;
                                    TypedArgument {
                                        info: t,
                                        arg: source.into(),
                                    }
                                }
                                Value::Register(r) => {
                                    let source = *builder.local(r).unwrap();
                                    TypedArgument {
                                        info: t,
                                        arg: source.reg.into(),
                                    }
                                }
                                Value::Uint(n) => TypedArgument {
                                    info: t,
                                    arg: n.into(),
                                },
                                Value::Bool(n) => TypedArgument {
                                    info: t,
                                    arg: (n as u8 as u64).into(),
                                },
                                _ => todo!(),
                            };

                            let target = *builder.local(ir.binding.unwrap()).unwrap();

                            let bb = builder.basic_block_mut(bb);
                            bb.instruction()
                                .opcode("mov")
                                .argument(Register::rax())
                                .argument(arg0.arg)
                                .build()?;
                            bb.instruction()
                                .opcode("and")
                                .argument(Register::rax())
                                .argument(arg1.arg)
                                .build()?;
                            bb.instruction()
                                .opcode("mov")
                                .argument(target.reg)
                                .argument(Register::rax())
                                .build()?;
                        }
                        Instruction::BitOr => {
                            let t = ir.typ.type_info(&named_types, &target);
                            let arg0 = match ir.args[0].val {
                                Value::Arg(r) => {
                                    let source = program.call_conv.argument(builder, r)?;
                                    TypedArgument {
                                        info: t,
                                        arg: source.into(),
                                    }
                                }
                                Value::Register(r) => {
                                    let source = builder.local(r).unwrap();
                                    TypedArgument {
                                        info: t,
                                        arg: source.reg.into(),
                                    }
                                }
                                Value::Uint(n) => TypedArgument {
                                    info: t,
                                    arg: n.into(),
                                },
                                Value::Bool(n) => TypedArgument {
                                    info: t,
                                    arg: (n as u8 as u64).into(),
                                },
                                _ => todo!(),
                            };
                            let arg1 = match ir.args[1].val {
                                Value::Arg(r) => {
                                    let source = program.call_conv.argument(builder, r)?;
                                    TypedArgument {
                                        info: t,
                                        arg: source.into(),
                                    }
                                }
                                Value::Register(r) => {
                                    let source = *builder.local(r).unwrap();
                                    TypedArgument {
                                        info: t,
                                        arg: source.reg.into(),
                                    }
                                }
                                Value::Uint(n) => TypedArgument {
                                    info: t,
                                    arg: n.into(),
                                },
                                Value::Bool(n) => TypedArgument {
                                    info: t,
                                    arg: (n as u8 as u64).into(),
                                },
                                _ => todo!(),
                            };

                            let target = *builder.local(ir.binding.unwrap()).unwrap();

                            let bb = builder.basic_block_mut(bb);
                            bb.instruction()
                                .opcode("mov")
                                .argument(Register::rax())
                                .argument(arg0.arg)
                                .build()?;
                            bb.instruction()
                                .opcode("or")
                                .argument(Register::rax())
                                .argument(arg1.arg)
                                .build()?;
                            bb.instruction()
                                .opcode("mov")
                                .argument(target.reg)
                                .argument(Register::rax())
                                .build()?;
                        }
                        Instruction::BitXor => {
                            let t = ir.typ.type_info(&named_types, &target);
                            let arg0 = match ir.args[0].val {
                                Value::Arg(r) => {
                                    let source = program.call_conv.argument(builder, r)?;
                                    TypedArgument {
                                        info: t,
                                        arg: source.into(),
                                    }
                                }
                                Value::Register(r) => {
                                    let source = builder.local(r).unwrap();
                                    TypedArgument {
                                        info: t,
                                        arg: source.reg.into(),
                                    }
                                }
                                Value::Uint(n) => TypedArgument {
                                    info: t,
                                    arg: n.into(),
                                },
                                Value::Bool(n) => TypedArgument {
                                    info: t,
                                    arg: (n as u8 as u64).into(),
                                },
                                _ => todo!(),
                            };
                            let arg1 = match ir.args[1].val {
                                Value::Arg(r) => {
                                    let source = program.call_conv.argument(builder, r)?;
                                    TypedArgument {
                                        info: t,
                                        arg: source.into(),
                                    }
                                }
                                Value::Register(r) => {
                                    let source = *builder.local(r).unwrap();
                                    TypedArgument {
                                        info: t,
                                        arg: source.reg.into(),
                                    }
                                }
                                Value::Uint(n) => TypedArgument {
                                    info: t,
                                    arg: n.into(),
                                },
                                Value::Bool(n) => TypedArgument {
                                    info: t,
                                    arg: (n as u8 as u64).into(),
                                },
                                _ => todo!(),
                            };

                            let target = *builder.local(ir.binding.unwrap()).unwrap();

                            let bb = builder.basic_block_mut(bb);
                            bb.instruction()
                                .opcode("mov")
                                .argument(Register::rax())
                                .argument(arg0.arg)
                                .build()?;
                            bb.instruction()
                                .opcode("xor")
                                .argument(Register::rax())
                                .argument(arg1.arg)
                                .build()?;
                            bb.instruction()
                                .opcode("mov")
                                .argument(target.reg)
                                .argument(Register::rax())
                                .build()?;
                        }
                        Instruction::Icmp => {
                            let t = ir.typ.type_info(&named_types, &target);
                            let icmp = match ir.args[0].val {
                                Value::Uint(n) => n as u8,
                                _ => panic!("icmp must be a literal integer!"),
                            };
                            assert!(matches!(icmp, 0..=5), "icmp is not in [0; 5)!");
                            let arg0 = match ir.args[1].val {
                                Value::Arg(r) => {
                                    let source = program.call_conv.argument(builder, r)?;
                                    TypedArgument {
                                        info: t,
                                        arg: source.into(),
                                    }
                                }
                                Value::Register(r) => {
                                    let source = builder.local(r).unwrap();
                                    TypedArgument {
                                        info: t,
                                        arg: source.reg.into(),
                                    }
                                }
                                Value::Uint(n) => TypedArgument {
                                    info: t,
                                    arg: n.into(),
                                },
                                _ => todo!(),
                            };
                            let arg1 = match ir.args[2].val {
                                Value::Arg(r) => {
                                    let source = program.call_conv.argument(builder, r)?;
                                    TypedArgument {
                                        info: t,
                                        arg: source.into(),
                                    }
                                }
                                Value::Register(r) => {
                                    let source = *builder.local(r).unwrap();
                                    TypedArgument {
                                        info: t,
                                        arg: source.reg.into(),
                                    }
                                }
                                Value::Uint(n) => TypedArgument {
                                    info: t,
                                    arg: n.into(),
                                },
                                _ => todo!(),
                            };

                            let target = *builder.local(ir.binding.unwrap()).unwrap();

                            let bb = builder.basic_block_mut(bb);
                            bb.instruction()
                                .opcode("cmp")
                                .argument(arg0.arg)
                                .argument(arg1.arg)
                                .build()?;
                            match icmp {
                                ICMP_EQ => {
                                    bb.instruction()
                                        .opcode("popf")
                                        .argument(Register::eax())
                                        .build()?;
                                    bb.instruction()
                                        .opcode("shr")
                                        .argument(Register::eax())
                                        .argument(6)
                                        .build()?;
                                    bb.instruction()
                                        .opcode("and")
                                        .argument(Register::eax())
                                        .argument(1)
                                        .build()?;
                                }
                                ICMP_NE => {
                                    bb.instruction()
                                        .opcode("popf")
                                        .argument(Register::eax())
                                        .build()?;
                                    bb.instruction()
                                        .opcode("shr")
                                        .argument(Register::eax())
                                        .argument(6)
                                        .build()?;
                                    bb.instruction()
                                        .opcode("and")
                                        .argument(Register::eax())
                                        .argument(1)
                                        .build()?;
                                    bb.instruction()
                                        .opcode("xor")
                                        .argument(Register::eax())
                                        .argument(1)
                                        .build()?;
                                }
                                ICMP_LT => {
                                    bb.instruction()
                                        .opcode("popf")
                                        .argument(Register::eax())
                                        .build()?;
                                    bb.instruction()
                                        .opcode("shr")
                                        .argument(Register::eax())
                                        .argument(7)
                                        .build()?;
                                    bb.instruction()
                                        .opcode("and")
                                        .argument(Register::eax())
                                        .argument(1)
                                        .build()?;
                                }
                                ICMP_GT => {
                                    bb.instruction()
                                        .opcode("popf")
                                        .argument(Register::eax())
                                        .build()?;
                                    bb.instruction()
                                        .opcode("popf")
                                        .argument(Register::edx())
                                        .build()?;
                                    bb.instruction()
                                        .opcode("shr")
                                        .argument(Register::eax())
                                        .argument(7)
                                        .build()?;
                                    bb.instruction()
                                        .opcode("shr")
                                        .argument(Register::edx())
                                        .argument(6)
                                        .build()?;
                                    bb.instruction()
                                        .opcode("and")
                                        .argument(Register::eax())
                                        .argument(1)
                                        .build()?;
                                    bb.instruction()
                                        .opcode("xor")
                                        .argument(Register::eax())
                                        .argument(1)
                                        .build()?;
                                    bb.instruction()
                                        .opcode("and")
                                        .argument(Register::edx())
                                        .argument(1)
                                        .build()?;
                                    bb.instruction()
                                        .opcode("xor")
                                        .argument(Register::edx())
                                        .argument(1)
                                        .build()?;
                                    bb.instruction()
                                        .opcode("and")
                                        .argument(Register::eax())
                                        .argument(Register::edx())
                                        .build()?;
                                }
                                ICMP_LE => {
                                    bb.instruction()
                                        .opcode("popf")
                                        .argument(Register::eax())
                                        .build()?;
                                    bb.instruction()
                                        .opcode("popf")
                                        .argument(Register::edx())
                                        .build()?;
                                    bb.instruction()
                                        .opcode("shr")
                                        .argument(Register::eax())
                                        .argument(7)
                                        .build()?;
                                    bb.instruction()
                                        .opcode("shr")
                                        .argument(Register::edx())
                                        .argument(6)
                                        .build()?;
                                    bb.instruction()
                                        .opcode("and")
                                        .argument(Register::eax())
                                        .argument(1)
                                        .build()?;
                                    bb.instruction()
                                        .opcode("and")
                                        .argument(Register::edx())
                                        .argument(1)
                                        .build()?;
                                    bb.instruction()
                                        .opcode("or")
                                        .argument(Register::eax())
                                        .argument(Register::edx())
                                        .build()?;
                                }
                                ICMP_GE => {
                                    bb.instruction()
                                        .opcode("popf")
                                        .argument(Register::eax())
                                        .build()?;
                                    bb.instruction()
                                        .opcode("shr")
                                        .argument(Register::eax())
                                        .argument(7)
                                        .build()?;
                                    bb.instruction()
                                        .opcode("and")
                                        .argument(Register::eax())
                                        .argument(1)
                                        .build()?;
                                    bb.instruction()
                                        .opcode("xor")
                                        .argument(Register::eax())
                                        .argument(1)
                                        .build()?;
                                }
                                _ => unreachable!(),
                            }
                            bb.instruction()
                                .opcode("movzx")
                                .argument(target.reg)
                                .argument(Register::eax())
                                .build()?;
                        }
                        _ => todo!(),
                    }
                }
            }
        }
        Ok(())
    }
}
