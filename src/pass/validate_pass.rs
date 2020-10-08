use failure::{Fail, Fallible};

use crate::assets::{LazyUpdate, Resources};
use crate::ast::{Kind, Node, RootNode, Visit, VisitResult};
use crate::error::MultiError;
use crate::lexer::Location;

#[derive(Debug, Fail)]
#[fail(display = "malformed tree at {}", _0)]
pub struct MalformedTree(Location);

pub fn validate_update(
    _lazy: &mut LazyUpdate,
    roots: Resources<&RootNode>,
    res: Resources<&Location>,
    nodes: Resources<&Node>,
) -> Fallible<Option<&'static str>> {
    let mut errors = Vec::new();
    for (_, root_node) in roots.iter::<RootNode>() {
        let root = nodes.get::<Node>(root_node.0).unwrap();
        root.visit(Visit::Postorder, &nodes, |_, node, _| {
            let loc = res
                .get::<Location>(node.location)
                .map(|loc| loc.as_ref().clone());
            match node.kind {
                Kind::Nil => {}
                Kind::Block => {
                    for child in node.children.iter().rev().skip(1).rev() {
                        if child.is_none() {
                            errors.push(From::from(MalformedTree(loc.clone().unwrap())));
                        }
                    }
                }
                Kind::Function => {
                    if node.children.len() != 2 {
                        errors.push(From::from(MalformedTree(loc.clone().unwrap())));
                        return VisitResult::Recurse;
                    }
                    let mut flag = false;
                    for child in &node.children {
                        if child.is_none() {
                            errors.push(From::from(MalformedTree(loc.clone().unwrap())));
                            flag = true;
                        }
                    }
                    if flag {
                        return VisitResult::Recurse;
                    }
                    let param_tuple = &*nodes.get::<Node>(node.children[0].unwrap()).unwrap();
                    match param_tuple.kind {
                        Kind::Nil | Kind::Tuple => {}
                        _ => {
                            errors.push(From::from(MalformedTree(loc.clone().unwrap())));
                        }
                    }
                }
                Kind::Application => {
                    if node.children.is_empty() {
                        errors.push(From::from(MalformedTree(loc.clone().unwrap())));
                        return VisitResult::Recurse;
                    }
                    for child in &node.children {
                        if child.is_none() {
                            errors.push(From::from(MalformedTree(loc.clone().unwrap())));
                        }
                    }
                }
                Kind::Binding | Kind::Global => {
                    if node.children.len() != 3 {
                        errors.push(From::from(MalformedTree(loc.clone().unwrap())));
                        return VisitResult::Recurse;
                    }
                    if node.children[0].is_none() {
                        errors.push(From::from(MalformedTree(loc.clone().unwrap())));
                    }
                    if node.children[2].is_none() {
                        errors.push(From::from(MalformedTree(loc.clone().unwrap())));
                    }
                }
                Kind::Assign => {
                    if node.children.len() != 2 {
                        errors.push(From::from(MalformedTree(loc.clone().unwrap())));
                        return VisitResult::Recurse;
                    }
                    if node.children[0].is_none() {
                        errors.push(From::from(MalformedTree(loc.clone().unwrap())));
                    }
                    if node.children[1].is_none() {
                        errors.push(From::from(MalformedTree(loc.clone().unwrap())));
                    }
                }
                Kind::Tuple => {
                    for child in &node.children {
                        if child.is_none() {
                            errors.push(From::from(MalformedTree(loc.clone().unwrap())));
                        }
                    }
                }
                Kind::Argument => {
                    if node.children.len() != 2 {
                        errors.push(From::from(MalformedTree(loc.clone().unwrap())));
                        return VisitResult::Recurse;
                    }
                    for child in &node.children {
                        if child.is_none() {
                            errors.push(From::from(MalformedTree(loc.clone().unwrap())));
                        }
                    }
                }
                Kind::Declaration => {
                    if node.children.len() != 2 {
                        errors.push(From::from(MalformedTree(loc.clone().unwrap())));
                        return VisitResult::Recurse;
                    }
                    for child in &node.children {
                        if child.is_none() {
                            errors.push(From::from(MalformedTree(loc.clone().unwrap())));
                        }
                    }
                }
                Kind::Array => {
                    for child in &node.children {
                        if child.is_none() {
                            errors.push(From::from(MalformedTree(loc.clone().unwrap())));
                        }
                    }
                }
                Kind::Index => {
                    if node.children.len() != 2 {
                        errors.push(From::from(MalformedTree(loc.clone().unwrap())));
                        return VisitResult::Recurse;
                    }
                    for child in &node.children {
                        if child.is_none() {
                            errors.push(From::from(MalformedTree(loc.clone().unwrap())));
                        }
                    }
                }
                Kind::Dotted => {
                    if node.children.len() < 2 {
                        errors.push(From::from(MalformedTree(loc.clone().unwrap())));
                        return VisitResult::Recurse;
                    }
                    for child in &node.children {
                        if child.is_none() {
                            errors.push(From::from(MalformedTree(loc.clone().unwrap())));
                        }
                    }
                }
                Kind::With => {
                    if node.children.len() < 2 {
                        errors.push(From::from(MalformedTree(loc.clone().unwrap())));
                        return VisitResult::Recurse;
                    }
                    for child in &node.children {
                        if child.is_none() {
                            errors.push(From::from(MalformedTree(loc.clone().unwrap())));
                        }
                    }
                }
            }
            VisitResult::Recurse
        });
    }
    if errors.is_empty() {
        Ok(None)
    } else {
        Err(From::from(MultiError::from(errors)))
    }
}
