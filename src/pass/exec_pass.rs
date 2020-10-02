use failure::Fallible;

use crate::assets::{LazyUpdate, Resources};
use crate::ast::{Node, RootNode, Visit, VisitResult};
use crate::lir::{context::ExecutionContext, Foreign, Value};
use crate::pass;
use crate::types::NamedType;
use crate::universe::Universe;

pub fn exec_update(
    lazy: &mut LazyUpdate,
    roots: Resources<&mut RootNode>,
    mut threads: Resources<&mut ExecutionContext>,
    foreign: Resources<&Foreign>,
    mut res: Resources<(&String, &mut NamedType, &mut Node)>,
) -> Fallible<Option<&'static str>> {
    let mut count = 0;
    let mut pos = 0;
    for (_, mut root_node) in roots.iter_mut::<RootNode>() {
        let root = res.get::<Node>(root_node.0).unwrap();
        let mut ctx = threads
            .get_mut::<ExecutionContext>(root.thread.unwrap())
            .unwrap();

        let mut min_universe = Universe::MAX;
        let mut max_universe = Universe::MIN;
        root.visit(Visit::Postorder, &res, |_, node, _| {
            min_universe = min_universe.min(&node.universe);
            max_universe = max_universe.max(&node.universe);
            VisitResult::Recurse
        });

        count += 1;
        if min_universe == max_universe {
            pos += 1;
            continue;
        }

        let main = ctx.main();
        let result = ctx.call(lazy, &foreign, &mut res, main, &[])?;
        match result.val {
            Value::Node(handle) => {
                let node = res.get(root_node.0).unwrap();
                let thread = node.thread.unwrap();
                let mut node = res.get_mut(handle).unwrap();
                node.thread = Some(thread);
                root_node.0 = handle;
            }
            _ => panic!("not a node"),
        }
    }

    if count == pos {
        Ok(Some(pass::BACKEND_PASS))
    } else {
        Ok(Some("repeat"))
    }
}

pub fn interpret_update(
    lazy: &mut LazyUpdate,
    roots: Resources<&mut RootNode>,
    mut threads: Resources<&mut ExecutionContext>,
    foreign: Resources<&Foreign>,
    mut res: Resources<(&String, &mut NamedType, &mut Node)>,
) -> Fallible<Option<&'static str>> {
    let mut count = 0;
    let mut pos = 0;
    for (_, mut root_node) in roots.iter_mut::<RootNode>() {
        let root = res.get::<Node>(root_node.0).unwrap();
        let mut ctx = threads
            .get_mut::<ExecutionContext>(root.thread.unwrap())
            .unwrap();

        let mut min_universe = Universe::MAX;
        let mut max_universe = Universe::MIN;
        root.visit(Visit::Postorder, &res, |_, node, _| {
            min_universe = min_universe.min(&node.universe);
            max_universe = max_universe.max(&node.universe);
            VisitResult::Recurse
        });

        count += 1;
        if min_universe == max_universe {
            pos += 1;
        }

        let main = ctx.main();
        let result = ctx.call(lazy, &foreign, &mut res, main, &[])?;
        match result.val {
            Value::Node(handle) => {
                let node = res.get(root_node.0).unwrap();
                let thread = node.thread.unwrap();
                let mut node = res.get_mut(handle).unwrap();
                node.thread = Some(thread);
                root_node.0 = handle;
            }
            _ => {}
        }
    }

    if count == pos {
        Ok(Some("finish"))
    } else {
        Ok(Some("repeat"))
    }
}
