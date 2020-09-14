use failure::Fallible;

use crate::assets::{LazyUpdate, Resources};
use crate::ast::{Node, RootNode};
use crate::lir::{compile::Compile, context::ExecutionContext, target::Target, ThreadId, Value};
use crate::types::NamedType;
use crate::values::Value as Payload;

pub fn compile_update(
    lazy: &mut LazyUpdate,
    roots: Resources<&RootNode>,
    threads: Resources<&ExecutionContext>,
    mut types: Resources<&NamedType>,
    mut nodes: Resources<(&mut Node, &Payload, &mut Value)>,
) -> Fallible<()> {
    for (_, root_node) in roots.iter::<RootNode>() {
        let root = root_node.0;
        let thread_id = ThreadId::new();
        // TODO: supply proper target
        let builder = ExecutionContext::builder(types, Target::default());
        let (t, ctx) = Node::compile(root, &mut nodes, builder)?;
        types = t;
        nodes.get_mut::<Node>(root).unwrap().thread = Some(thread_id);
        lazy.insert(thread_id, ctx);
    }
    Ok(())
}
