use failure::Fallible;

use crate::assets::{LazyUpdate, Resources, Static};
use crate::ast::{Node, RootNode};
use crate::lir::{codegen::*, context::ExecutionContext, target::Target, Instruction, Value};
use crate::types::{NamedType, TypeInfo};

pub fn codegen_update(
    _lazy: &mut LazyUpdate,
    target: &Static<Target>,
    roots: Resources<&mut RootNode>,
    mut threads: Resources<&mut ExecutionContext>,
    named_types: Resources<&NamedType>,
    nodes: Resources<&Node>,
) -> Fallible<Option<&'static str>> {
    let mut program = ProgramBuilder::new(AmpCall64);
    for (_, root_node) in roots.iter_mut::<RootNode>() {
        let root = nodes.get::<Node>(root_node.0).unwrap();
        let ctx = threads
            .remove::<ExecutionContext>(root.thread.unwrap())
            .unwrap();
        for func in ctx.functions() {
            let builder =
                program
                    .codegen
                    .add_function(&*program.call_conv, "main".to_string(), &[], func.blocks.clone());
            for ir in &func.body {
                match ir.instr {
                    Instruction::Alloca => {
                        let t = match ir.args[0] {
                            Value::Type(t) => t,
                            _ => todo!(),
                        };
                        let t = t.type_info(&named_types, &target);
                        let n = match ir.args[1] {
                            Value::Uint(i) => i as usize,
                            _ => todo!(),
                        };
                        let (start, end) = func.lifetime(ir.binding.unwrap());
                        let r = ir.binding.unwrap();
                        if func.is_ref(r) {
                            builder.add_stack(r, TypeInfo::new(t.size * n, t.align), start, end);
                        } else {
                            builder.add_local(r, TypeInfo::new(t.size * n, t.align), start, end);
                        }
                    }
                    Instruction::Load => {
                        let t = match ir.args[0] {
                            Value::Type(t) => t,
                            _ => todo!(),
                        };
                        let t = t.type_info(&named_types, &target);
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
                        let t = match ir.args[0] {
                            Value::Type(t) => t,
                            _ => todo!(),
                        };
                        let t = t.type_info(&named_types, &target);
                        if t.size > 8 {
                            todo!()
                        } else {
                            let target = match ir.args[1] {
                                Value::Register(r) => *builder.local(r).unwrap(),
                                _ => todo!(),
                            };
                            match ir.args[2] {
                                Value::Uint(int) => {
                                    let bb = builder.basic_block_mut(bb);
                                    bb.instruction()
                                        .opcode("mov")
                                        .size(Size::from_bytes(t.size).unwrap())
                                        .argument(target.reg)
                                        .argument(int)
                                        .build()?;
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
                                _ => todo!(),
                            }
                        }
                    }
                    Instruction::Load => {
                        let t = match ir.args[0] {
                            Value::Type(t) => t,
                            _ => todo!(),
                        };
                        let t = t.type_info(&named_types, &target);
                        if t.size > 8 {
                            todo!()
                        } else {
                            let target = *builder.local(ir.binding.unwrap()).unwrap();
                            match ir.args[1] {
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
                    Instruction::Return => {
                        let t = match ir.args[0] {
                            Value::Type(t) => t,
                            _ => todo!(),
                        };
                        let t = t.type_info(&named_types, &target);
                        let result = match ir.args[1] {
                            Value::Uint(u) => {
                                TypedArgument {
                                    info: t,
                                    arg: u.into(),
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
                        program.call_conv.build_ret(
                            builder,
                            bb,
                            result,
                        )?;
                    }
                    _ => todo!(),
                }
            }
        }
    }
    let output = program.build();
    println!("{}", output);
    Ok(Some("finish"))
}
