use failure::Fallible;

use crate::pass;
use crate::assets::{LazyUpdate, Resources};
use crate::ast::{Node, RootNode, Visit, VisitResult};
use crate::lir::{context::ExecutionContext, Foreign, Value};
use crate::types::NamedType;

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
        println!("{}", ctx.display());
        
        let mut min_universe = i32::MAX;
        let mut max_universe = i32::MIN;
        root.visit(Visit::Postorder, &res, |_, node| {
            min_universe = min_universe.min(node.universe);
            max_universe = max_universe.max(node.universe);
            VisitResult::Recurse
        });

        count += 1;
        if min_universe == max_universe {
            pos += 1;
            continue;
        }
        
        let result = ctx.call(lazy, &foreign, &mut res, ExecutionContext::MAIN, &[])?;
        match result {
            Value::Node(handle) => {
                root_node.0 = handle;
            }
            _ => panic!("not a node"),
        }
    }
    
    if count == pos {
        Ok(Some(pass::CODEGEN_PASS))
    } else {
        Ok(Some("repeat"))
    }
}
