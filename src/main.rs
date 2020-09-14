use assets::*;
use system::*;
use pipeline::*;
use ast::{Node, RootNode};
use lir::context::ExecutionContext;
use types::NamedType;
use values::Value;
use dispatch::Dispatcher;
use scope::Scope;

pub mod error;
pub mod assets;
pub mod system;
pub mod pipeline;
pub mod ast;
pub mod types;
pub mod values;
pub mod pass;
pub mod lir;
pub mod dispatch;
pub mod scope;

fn main() {
    let world = World::new();
    world.init_asset::<RootNode>();
    world.init_asset::<Node>();
    world.init_asset::<Value>();
    world.init_asset::<NamedType>();
    world.init_asset::<ExecutionContext>();
    world.init_asset::<Dispatcher>();
    world.init_asset::<Scope>();
    world.init_asset::<String>();
    
    let mut pipeline = Pipeline::new();
    pipeline.add_stage(pass::CONST_PASS);
    pipeline.add_stage(pass::INFER_PASS);
    pipeline.add_stage(pass::COMPILE_PASS);
    pipeline.add_stage(pass::EXEC_PASS);
    pipeline.add_system_to_stage(pass::CONST_PASS, pass::const_update.system());
    pipeline.add_system_to_stage(pass::INFER_PASS, pass::infer_update.system());
    pipeline.add_system_to_stage(pass::COMPILE_PASS, pass::compile_update.system());
    pipeline.add_system_to_stage(pass::EXEC_PASS, pass::exec_update.system());

    if let Err(err) = pipeline.run(&world) {
        eprintln!("{}", err);
    }
}
