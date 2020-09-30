use failure::Fallible;

use crate::assets::{LazyUpdate, Resources, Static};
use crate::ast::{Node, RootNode, Visit, VisitResult};
use crate::builder::BuilderMacro;
use crate::lir::{
    compile::Compile, context::ExecutionContext, target::Target, Bytes, Elems, ThreadId,
    TypedValue, Variants,
};
use crate::scope::Scope;
use crate::types::NamedType;
use crate::values::Payload;

pub fn compile_update(
    lazy: &mut LazyUpdate,
    target: &Static<Target>,
    roots: Resources<&RootNode>,
    mut threads: Resources<&mut ExecutionContext>,
    mut types: Resources<&mut NamedType>,
    mut nodes: Resources<(
        &mut Node,
        &Scope,
        &Payload,
        &String,
        &mut TypedValue,
        &mut Elems,
        &mut Variants,
        &mut Bytes,
    )>,
    builders: Resources<&BuilderMacro>,
) -> Fallible<Option<&'static str>> {
    for (_, root_node) in roots.iter::<RootNode>() {
        let root = root_node.0;
        let node = nodes.get::<Node>(root).unwrap();

        let mut min_universe = i32::MAX;
        let mut max_universe = i32::MIN;
        node.visit(Visit::Postorder, &nodes, |_, node, _| {
            min_universe = min_universe.min(node.universe);
            max_universe = max_universe.max(node.universe);
            VisitResult::Recurse
        });

        let node = nodes.get_mut::<Node>(root).unwrap();

        let builder = if min_universe != max_universe {
            let ctx = if let Some(thread) = node.thread {
                let mut ctx = threads.remove(thread).unwrap();
                let mut name = String::new();
                for _ in 0..16 {
                    name.push((b'a' + rand::random::<u8>() % 26) as char);
                }
                ctx.named_function_mut("main").unwrap().name = name;
                Some(ctx)
            } else {
                None
            };
            if let Some(ctx) = ctx {
                ExecutionContext::builder_with(ctx, types)
            } else {
                ExecutionContext::builder(types, target.as_ref().clone())
            }
        } else {
            ExecutionContext::builder(types, target.as_ref().clone())
        };
        let (t, ctx) = Node::compile(root, Some("main"), &mut nodes, &builders, builder)?;
        types = t;
        let thread_id = ThreadId::new();
        nodes.get_mut::<Node>(root).unwrap().thread = Some(thread_id);
        lazy.insert(thread_id, ctx);
    }
    Ok(None)
}
