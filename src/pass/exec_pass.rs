use failure::Fallible;

use crate::assets::{LazyUpdate, Resources};
use crate::ast::{Node, RootNode};
use crate::lir::context::ExecutionContext;
use crate::types::NamedType;

pub fn exec_update(
    lazy: &mut LazyUpdate,
    roots: Resources<&mut RootNode>,
    mut threads: Resources<&mut ExecutionContext>,
    types: Resources<&NamedType>,
    mut nodes: Resources<&Node>,
) -> Fallible<()> {
    for (_, root_node) in roots.iter_mut::<RootNode>() {
        let root = nodes.get::<Node>(root_node.0).unwrap();
        let mut ctx = threads.get_mut::<ExecutionContext>(root.thread.unwrap()).unwrap();
        let result = ctx.call(lazy, &types, ExecutionContext::MAIN, &[])?;
        println!("{:?}", result);
    }
    Ok(())
}
