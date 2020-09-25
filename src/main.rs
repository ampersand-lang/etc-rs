#![cfg_attr(feature = "docs", warn(missing_docs))]

//! The reference compiler for ampersand.

use assets::*;
use ast::{Node, RootNode, Visit, VisitResult};
use dispatch::Dispatcher;
use lexer::{Lexer, Location};
use lir::{
    context::ExecutionContext, foreign, target::Target, Binding, Bytes, Elems, Foreign, TypedValue,
    Variants,
};
use parser::{grammar, State};
use pipeline::*;
use scope::Scope;
use system::*;
use types::{builtin, primitive, NamedType};
use values::Payload;

pub mod utils;

pub mod assets;
pub mod ast;
pub mod dispatch;
pub mod error;
pub mod lexer;
pub mod lir;
pub mod parser;
pub mod pass;
pub mod pipeline;
pub mod scope;
pub mod system;
pub mod types;
pub mod values;

const SRC: &str = include_str!("../examples/hello.amp");

fn main() {
    better_panic::install();

    let world = World::new();
    world.init_asset::<RootNode>();
    world.init_asset::<Node>();
    world.init_asset::<NamedType>();
    world.init_asset::<Payload>();
    world.init_asset::<ExecutionContext>();
    world.init_asset::<TypedValue>();
    world.init_asset::<Dispatcher>();
    world.init_asset::<Scope>();
    world.init_asset::<String>();
    world.init_asset::<Binding>();
    world.init_asset::<Location>();
    world.init_asset::<Elems>();
    world.init_asset::<Variants>();
    world.init_asset::<Bytes>();
    world.init_asset::<Foreign>();
    world.init_static::<Target>();

    primitive::init(world.resources());
    builtin::init(world.resources());
    foreign::init(world.resources());
    dispatch::init(world.resources());

    let node = grammar::parse(&mut State {
        lexer: Lexer::new("hello.amp", SRC, world.resources()),
        nodes: world.resources(),
    })
    .unwrap();

    println!(
        "{}",
        ast::PrettyPrinter::with_default(
            world.resources(),
            world.resources(),
            world.resources(),
            node
        )
    );

    let handle = Handle::new();
    let root = RootNode(node, false);
    world.insert(handle, root);

    let mut pipeline = Pipeline::new();
    pipeline.add_stage(pass::VALIDATE_PASS);
    pipeline.add_stage(pass::UNIVERSE_PASS);
    pipeline.add_stage(pass::MIR_PASS);
    pipeline.add_stage(pass::SCOPE_PASS);
    pipeline.add_stage(pass::INFER_PASS);
    pipeline.add_stage(pass::COLLAPSE_PASS);
    pipeline.add_stage(pass::COMPILE_PASS);
    pipeline.add_stage(pass::EXEC_PASS);
    pipeline.add_stage(pass::CODEGEN_PASS);
    pipeline.repeat(|world| {
        let roots = world.resources::<&mut RootNode>();
        let nodes = world.resources::<&Node>();
        for (_, mut root_node) in roots.iter_mut::<RootNode>() {
            let root = nodes.get::<Node>(root_node.0).unwrap();

            let mut min_universe = i32::MAX;
            let mut max_universe = i32::MIN;
            root.visit(Visit::Postorder, &nodes, |_, node, _| {
                min_universe = min_universe.min(node.universe);
                max_universe = max_universe.max(node.universe);
                VisitResult::Recurse
            });

            if min_universe == max_universe {
                root_node.1 = true;
            }
        }

        Some(pass::MIR_PASS)
    });
    pipeline.add_system_to_stage(pass::VALIDATE_PASS, pass::validate_update.system());
    pipeline.add_system_to_stage(pass::UNIVERSE_PASS, pass::universe_update.system());
    pipeline.add_system_to_stage(pass::MIR_PASS, pass::mir_update.system());
    pipeline.add_system_to_stage(pass::SCOPE_PASS, pass::scope_update.system());
    pipeline.add_system_to_stage(pass::INFER_PASS, pass::infer_update.system());
    pipeline.add_system_to_stage(pass::COLLAPSE_PASS, pass::collapse_update.system());
    pipeline.add_system_to_stage(pass::COMPILE_PASS, pass::compile_update.system());
    pipeline.add_system_to_stage(pass::EXEC_PASS, pass::exec_update.system());
    pipeline.add_system_to_stage(pass::CODEGEN_PASS, pass::codegen_update.system());

    if let Err(err) = pipeline.run(&world) {
        eprintln!("errors:");
        eprintln!("{}", err);
    }
}
