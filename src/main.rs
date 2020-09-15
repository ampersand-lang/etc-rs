use peekmore_asref::PeekMore;

use assets::*;
use ast::{Node, RootNode};
use dispatch::Dispatcher;
use lir::{context::ExecutionContext, Binding, Value};
use pipeline::*;
use scope::Scope;
use system::*;
use types::NamedType;
use values::Payload;
use lexer::{Lexer, Location};
use parser::{grammar, State};

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
    let world = World::new();
    world.init_asset::<RootNode>();
    world.init_asset::<Node>();
    world.init_asset::<NamedType>();
    world.init_asset::<Payload>();
    world.init_asset::<ExecutionContext>();
    world.init_asset::<Value>();
    world.init_asset::<Dispatcher>();
    world.init_asset::<Scope>();
    world.init_asset::<String>();
    world.init_asset::<Binding>();
    world.init_asset::<Location>();

    let node = grammar::parse(&mut State {
        lexer: Lexer::new("hello.amp", SRC, world.resources::<(&mut String, &mut Location)>()).peekmore(),
        nodes: world.resources::<&mut Node>(),
    }).unwrap();

    let handle = Handle::new();
    let root = RootNode(node);
    world.insert(handle, root);

    let mut pipeline = Pipeline::new();
    pipeline.add_stage(pass::CONST_PASS);
    pipeline.add_stage(pass::SCOPE_PASS);
    pipeline.add_stage(pass::INFER_PASS);
    pipeline.add_stage(pass::COMPILE_PASS);
    pipeline.add_stage(pass::EXEC_PASS);
    pipeline.add_system_to_stage(pass::CONST_PASS, pass::const_update.system());
    pipeline.add_system_to_stage(pass::SCOPE_PASS, pass::scope_update.system());
    pipeline.add_system_to_stage(pass::INFER_PASS, pass::infer_update.system());
    pipeline.add_system_to_stage(pass::COMPILE_PASS, pass::compile_update.system());
    pipeline.add_system_to_stage(pass::EXEC_PASS, pass::exec_update.system());

    if let Err(err) = pipeline.run(&world) {
        eprintln!("{}", err);
    }
}
