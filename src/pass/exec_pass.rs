use failure::Fallible;

use crate::assets::{LazyUpdate, Resources};
use crate::ast::{Node, RootNode};

pub fn exec_update(
    lazy: &mut LazyUpdate,
    roots: Resources<&RootNode>,
    mut nodes: Resources<&mut Node>,
) -> Fallible<()> {
    for (_, root_node) in roots.iter::<RootNode>() {
        let root = nodes.get::<Node>(root_node.0).unwrap();
        todo!()
    }
    Ok(())
}
