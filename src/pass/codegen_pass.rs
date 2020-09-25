use failure::Fallible;
use smallvec::SmallVec;

use crate::assets::{LazyUpdate, Resources, Static};
use crate::ast::{Node, RootNode};
use crate::lir::{codegen::*, context::ExecutionContext, target::Target, Instruction, Value};
use crate::types::{NamedType, Type, TypeInfo, TypeOrPlaceholder};

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
        let functions = ctx.iter().map(|func| func.name.to_string()).collect::<Vec<_>>();
        for func in ctx.functions() {
            let params = func.param_types.iter().map(|t| t.type_info(&named_types, target)).collect::<SmallVec<[_; 6]>>();
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
                            builder.add_stack(r, TypeInfo::new(t.size * n, t.align), start, end);
                        } else {
                            builder.add_local(r, TypeInfo::new(t.size * n, t.align), start, end);
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
                                    let source = functions[i].to_string();
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
                            TypeOrPlaceholder::Type(handle) => {
                                named_types.get(handle).unwrap()
                            }
                            _ => todo!(),
                        };
                        let param_types = match param_types.t {
                            Type::Function { ref param_types, .. } => param_types,
                            _ => todo!(),
                        };
                        
                        let func_ref = match ir.args[0].val {
                            Value::Arg(r) => {
                                program.call_conv.argument(builder, r)?
                            }
                            Value::Register(r) => {
                                builder.local(r).unwrap().reg.into()
                            }
                            Value::Function(i) => {
                                functions[i].to_string().into()
                            }
                            _ => todo!(),
                        };
                        let mut arguments = SmallVec::<[_; 6]>::new();
                        for (idx, &arg) in ir.args[2..].iter().enumerate() {
                            let info = param_types[idx].type_info(&named_types, target);
                            let arg = match arg.val {
                                Value::Uint(i) => {
                                    i.into()
                                }
                                Value::Arg(r) => {
                                    program.call_conv.argument(builder, r)?
                                }
                                Value::Register(r) => {
                                    builder.local(r).unwrap().reg.into()
                                }
                                Value::Function(i) => {
                                    functions[i].to_string().into()
                                }
                                _ => todo!(),
                            };
                            arguments.push(TypedArgument { info, arg });
                        }
                        program.call_conv.build_call(builder, bb, func_ref, t, &arguments)?;
                    }
                    Instruction::Return => {
                        let t = ir.args[0].typ;
                        let t = t.type_info(&named_types, &target);
                        let result = match ir.args[0].val {
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
                    _ => todo!(),
                }
            }
        }
    }
    let output = program.build();
    println!("{}", output);
    Ok(Some("finish"))
}
