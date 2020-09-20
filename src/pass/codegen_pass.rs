use failure::Fallible;

use crate::assets::{LazyUpdate, Resources};
use crate::ast::{Node, RootNode};
use crate::lir::{codegen::*, context::ExecutionContext, Instruction, Value};
use crate::types::TypeInfo;

pub fn codegen_update(
    _lazy: &mut LazyUpdate,
    roots: Resources<&mut RootNode>,
    mut threads: Resources<&mut ExecutionContext>,
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
                    .add_function(&*program.call_conv, "main".to_string(), &[]);
            let bb = builder.add_basic_block();
            for ir in func.body {
                match ir.instr {
                    Instruction::Return => match ir.args[1] {
                        Value::Uint(u) => {
                            program.call_conv.build_ret(
                                builder,
                                bb,
                                TypedArgument {
                                    info: TypeInfo::new(8, 8),
                                    arg: u.into(),
                                },
                            )?;
                        }
                        _ => todo!(),
                    },
                    _ => todo!(),
                }
            }
        }
    }
    let output = program.build();
    println!("{}", output);
    Ok(Some("finish"))
}
