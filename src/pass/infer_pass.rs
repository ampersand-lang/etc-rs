use failure::Fallible;
use hashbrown::HashMap;

use crate::assets::{LazyUpdate, Resources};
use crate::ast::{Kind, Node, RootNode, Visit};
use crate::values::Value;
use crate::scope::Scope;
use crate::types::primitive;

pub fn infer_update(
    _lazy: &mut LazyUpdate,
    roots: Resources<&RootNode>,
    payloads: Resources<&Value>,
    scopes: Resources<&Scope>,
    mut nodes: Resources<&mut Node>,
) -> Fallible<()> {
    for (_, root_node) in roots.iter::<RootNode>() {
        let root = nodes.get::<Node>(root_node.0).unwrap();
        let mut types = HashMap::new();
        root.visit(Visit::Postorder, &nodes, |_, node| {
            if let Some(node) = node {
                match node.kind {
                    Kind::Nil => {
                        let typ = match &*payloads.get(node.payload.unwrap()).unwrap() {
                            Value::Unit => primitive::UNIT.clone(),
                            Value::Int(_) => primitive::SINT.clone(),
                            Value::Float(_) => primitive::FLOAT.clone(),
                            Value::String(_) => todo!(),
                            Value::Identifier(_) => todo!(),
                            Value::Type(_) => primitive::TYPE.clone(),
                            Value::Function(_) => todo!(),
                        };
                        types.insert(node.id(), typ);
                    }
                    Kind::Block => {
                        let typ = node.children.last().and_then(|last| last.and_then(|last| {
                            nodes.get::<Node>(last).unwrap().type_of.clone()
                        }));
                        if let Some(typ) = typ {
                            types.insert(node.id(), typ);
                        }
                    }
                    Kind::Function => todo!(),
                    Kind::Application => todo!(),
                    Kind::Binding => todo!(),
                }
            }
        });
        for (handle, typ) in types {
            nodes.get_mut::<Node>(handle).unwrap().type_of = Some(typ);
        }
    }
    Ok(())
}
