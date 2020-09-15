use failure::Fallible;
use hashbrown::HashSet;

use crate::assets::{LazyUpdate, Resources};
use crate::ast::{Kind, Node, RootNode, Visit};

pub fn const_update(
    _lazy: &mut LazyUpdate,
    roots: Resources<&RootNode>,
    mut nodes: Resources<&mut Node>,
) -> Fallible<()> {
    for (_, root_node) in roots.iter::<RootNode>() {
        let root = nodes.get::<Node>(root_node.0).unwrap();
        let mut make_const = HashSet::new();
        root.visit(Visit::Postorder, &nodes, |res, node| {
            if let Some(node) = node {
                match node.kind {
                    // TODO: don't
                    Kind::Nil => {
                        make_const.insert(node.id());
                    }
                    Kind::Block
                    | Kind::Function
                    | Kind::Application
                    | Kind::Binding
                    | Kind::Declaration
                    | Kind::Tuple
                    | Kind::TupleType
                    | Kind::Index
                    | Kind::Dotted
                    | Kind::Array => {
                        let mut is_const = true;
                        for child in &node.children {
                            let node = child.as_ref().map(|handle| res.get(*handle).unwrap());
                            if let Some(node) = node {
                                if !node.is_const {
                                    is_const = false;
                                    break;
                                }
                            }
                        }
                        if is_const {
                            make_const.insert(node.id());
                        }
                    }
                }
            }
        });
        for handle in make_const {
            nodes.get_mut::<Node>(handle).unwrap().is_const = true;
        }
    }
    Ok(())
}
