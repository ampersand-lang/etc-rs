use failure::Fallible;

use crate::assets::{LazyUpdate, Resources, Static};
use crate::ast::{Node, RootNode};
use crate::lir::{
    compile::Compile, context::ExecutionContext, target::Target, Bytes, Elems, Fields, ThreadId,
    Value, Variants,
};
use crate::types::NamedType;
use crate::values::Payload;

pub fn compile_update(
    lazy: &mut LazyUpdate,
    target: &Static<Target>,
    roots: Resources<&RootNode>,
    _threads: Resources<&ExecutionContext>,
    mut types: Resources<&NamedType>,
    mut nodes: Resources<(
        &mut Node,
        &Payload,
        &String,
        &mut Value,
        &mut Elems,
        &mut Fields,
        &mut Variants,
        &mut Bytes,
    )>,
) -> Fallible<Option<&'static str>> {
    for (_, root_node) in roots.iter::<RootNode>() {
        let root = root_node.0;
        let thread_id = ThreadId::new();
        let builder = ExecutionContext::builder(types, target.as_ref().clone());
        let (t, ctx) = Node::compile(root, &mut nodes, builder)?;
        types = t;
        nodes.get_mut::<Node>(root).unwrap().thread = Some(thread_id);
        lazy.insert(thread_id, ctx);
    }
    Ok(None)
}
