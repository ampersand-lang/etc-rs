use std::iter;

use failure::Fallible;

use crate::assets::{Handle, LazyUpdate, Resources};
use crate::ast::{Kind, Node, RootNode, Visit, VisitResult};
use crate::values::Payload;

pub fn mir_update(
    lazy: &mut LazyUpdate,
    roots: Resources<&mut RootNode>,
    mut nodes: Resources<&mut Node>,
) -> Fallible<Option<&'static str>> {
    for (_, mut root_node) in roots.iter_mut::<RootNode>() {
        let root = nodes.get::<Node>(root_node.0).unwrap();

        let mut min_universe = i32::MAX;
        let mut max_universe = i32::MIN;
        root.visit(Visit::Postorder, &nodes, |_, node, _| {
            min_universe = min_universe.min(node.universe);
            max_universe = max_universe.max(node.universe);
            VisitResult::Recurse
        });

        if min_universe == max_universe {
            continue;
        }

        let mut generic_calls = Vec::new();

        root.visit(Visit::Postorder, &nodes, |_, node, parent| {
            if node.generic_call.is_some() {
                generic_calls.push((node.id(), parent.unwrap().id()));
            }
            VisitResult::Recurse
        });

        for &(gen, handle) in &generic_calls {
            let mut gen = nodes.get_mut(gen).unwrap();
            gen.generic_call = None;
            gen.no_newnode = true;
            let node = nodes.get(handle).unwrap();
            let mut node = node.as_ref().clone();
            for child in &mut node.children[1..] {
                let child_handle = child.as_mut().unwrap();
                let child = nodes.get(*child_handle).unwrap();
                let new = child.as_ref().clone();
                let handle = new.id();
                nodes.insert(new.id(), new);
                let new = handle;

                let mut kind = Node::new(Kind::Nil, Handle::nil(), iter::empty());
                let k: u8 = Kind::Nil.into();
                kind.payload = Some(Payload::Integer(k as _));
                let h = kind.id();
                nodes.insert(h, kind);
                let kind = h;

                let mut new_node = Node::new(Kind::Nil, Handle::nil(), iter::empty());
                let identifier = "new-node".to_string();
                let handle = Handle::from_hash(identifier.as_bytes());
                nodes.insert(handle, identifier);
                new_node.payload = Some(Payload::Identifier(handle));
                new_node.alternative = true;
                let h = new_node.id();
                nodes.insert(h, new_node);
                let new_node = h;

                let new_node = Node::new(
                    Kind::Application,
                    Handle::nil(),
                    iter::once(Some(new_node))
                        .chain(iter::once(Some(kind)))
                        .chain(iter::once(Some(new))),
                );
                let h = new_node.id();
                nodes.insert(h, new_node);
                let new_node = h;
                
                *child_handle = new_node;
            }
            nodes.get_mut(handle).unwrap().clone_from(node);
        }

        let mut constants = Vec::new();
        
        let root = nodes.get::<Node>(root_node.0).unwrap();

        root.visit(Visit::Preorder, &nodes, |_, node, parent| match node.kind {
            Kind::Argument | Kind::Binding | Kind::Declaration => VisitResult::Recurse,
            _ => {
                if node.universe == min_universe {
                    let mut dont = false;
                    node.visit(Visit::Postorder, &nodes, |_, node, _| {
                        if node.no_newnode {
                            dont = true;
                            return VisitResult::Break;
                        }
                        VisitResult::Recurse
                    });
                    constants.push((node.id(), parent.map(|p| p.id()), dont));
                    VisitResult::Continue
                } else {
                    VisitResult::Recurse
                }
            }
        });

        let mut block = Vec::new();
        let mut counter = 0;

        let mut replacements = Vec::new();

        for (handle, parent, no_newnode) in constants {
            let constant = nodes.get::<Node>(handle).unwrap();
            let clone = constant.as_ref().clone();

            let clone = match clone.kind {
                Kind::Function => {
                    let body = clone.children[1].unwrap();
                    let body = nodes.get(body).unwrap();
                    match &body.generic {
                        Some(variables) => {
                            let variables = variables.clone();
                            let mut replacements = Vec::new();
                            for &var in &variables {
                                let mut replacement = Node::new(Kind::Nil, Handle::nil(), iter::empty());
                                replacement.payload = Some(Payload::Identifier(var));
                                let handle = replacement.id();
                                lazy.insert(replacement.id(), replacement);
                                let replacement = handle;
                                replacements.push(replacement);
                            }
                            // PERF: clone is bad
                            let body = body.as_ref().clone();
                            let body = body.clone_with(&mut nodes, |_, this, children| {
                                match this.kind {
                                    Kind::Nil => match this.payload.as_ref().unwrap() {
                                        Payload::Identifier(ident) => {
                                            if let Some(pos) = variables.iter().position(|var| var == ident) {
                                                let mut replace = Node::new(Kind::Nil, Handle::nil(), iter::empty());

                                                let identifier = "replace".to_string();
                                                let handle = Handle::from_hash(identifier.as_bytes());
                                                lazy.insert(handle, identifier);
                                                replace.alternative = true;
                                                replace.payload = Some(Payload::Identifier(handle));

                                                let rid = replace.id();
                                                lazy.insert(replace.id(), replace);
                                                
                                                let mut var = Node::new(Kind::Nil, Handle::nil(), iter::empty());

                                                let identifier = format!("%{}", pos);
                                                let handle = Handle::from_hash(identifier.as_bytes());
                                                lazy.insert(handle, identifier);
                                                var.alternative = true;
                                                var.payload = Some(Payload::Identifier(handle));

                                                let vid = var.id();
                                                lazy.insert(var.id(), var);

                                                return Node::new(
                                                    Kind::Application,
                                                    Handle::nil(),
                                                    iter::once(Some(rid)).chain(iter::once(Some(vid))),
                                                );
                                                
                                            }
                                        }
                                        _ => {}
                                    },
                                    _ => {}
                                }
                                let mut new = this.clone();
                                new.children = children;
                                new
                            });
                            let handle = body.id();
                            lazy.insert(body.id(), body);
                            let body = handle;
                            
                            let mut quasiquote = Node::new(Kind::Nil, Handle::nil(), iter::empty());

                            let identifier = "quasiquote".to_string();
                            let handle = Handle::from_hash(identifier.as_bytes());
                            lazy.insert(handle, identifier);
                            quasiquote.alternative = true;
                            quasiquote.payload = Some(Payload::Identifier(handle));

                            let qqid = quasiquote.id();
                            lazy.insert(quasiquote.id(), quasiquote);

                            let quote = Node::new(
                                Kind::Application,
                                Handle::nil(),
                                iter::once(Some(qqid)).chain(iter::once(Some(body))),
                            );
                            let qid = quote.id();
                            lazy.insert(quote.id(), quote);

                            let mut format_ast = Node::new(Kind::Nil, Handle::nil(), iter::empty());

                            let identifier = "format-ast".to_string();
                            let handle = Handle::from_hash(identifier.as_bytes());
                            lazy.insert(handle, identifier);
                            format_ast.alternative = true;
                            format_ast.payload = Some(Payload::Identifier(handle));

                            let faid = format_ast.id();
                            lazy.insert(format_ast.id(), format_ast);
                            
                            let format = Node::new(
                                Kind::Application,
                                Handle::nil(),
                                iter::once(Some(faid))
                                    .chain(iter::once(Some(qid)))
                                    .chain(replacements.into_iter().map(Some)),
                            );
                            
                            let body = clone.children[1].unwrap();
                            let mut body = nodes.get_mut(body).unwrap();
                            body.clone_from(format);

                            if let Some(parent) = parent {
                                let parent = nodes.get(parent).unwrap();
                                if matches!(parent.kind, Kind::Binding) {
                                    let binding = parent.children[0].unwrap();
                                    let binding = nodes.get(binding).unwrap();
                                    let binding = binding.as_ref().clone();
                                    let handle = binding.id();
                                    nodes.insert(binding.id(), binding);
                                    let binding = handle;

                                    let func = clone;
                                    let handle = func.id();
                                    nodes.insert(func.id(), func);
                                    let func = handle;

                                    let assignment = Node::new(
                                        Kind::Binding,
                                        Handle::nil(),
                                        iter::once(Some(binding))
                                            .chain(iter::once(None))
                                            .chain(iter::once(Some(func))),
                                    );
                                    assignment
                                } else {
                                    clone
                                }
                            } else {
                                clone
                            }
                        }
                        None => clone
                    }
                }
                _ => clone,
            };
            
            let constant = nodes.get_mut::<Node>(handle).unwrap();

            let h = clone.id();
            lazy.insert(clone.id(), clone);
            let clone = h;

            let (constant, mut interpolate) = (clone, constant);

            if !no_newnode {
                let mut kind = Node::new(Kind::Nil, Handle::nil(), iter::empty());
                let k: u8 = Kind::Nil.into();
                kind.payload = Some(Payload::Integer(k as _));
                let h = kind.id();
                lazy.insert(h, kind);
                let kind = h;

                let mut new_node = Node::new(Kind::Nil, Handle::nil(), iter::empty());
                let identifier = "new-node".to_string();
                let handle = Handle::from_hash(identifier.as_bytes());
                lazy.insert(handle, identifier);
                new_node.payload = Some(Payload::Identifier(handle));
                new_node.alternative = true;
                let h = new_node.id();
                lazy.insert(h, new_node);
                let new_node = h;

                let new_node = Node::new(
                    Kind::Application,
                    Handle::nil(),
                    iter::once(Some(new_node))
                        .chain(iter::once(Some(kind)))
                        .chain(iter::once(Some(constant))),
                );
                let h = new_node.id();
                lazy.insert(h, new_node);
                let new_node = h;

                let mut binding1 = Node::new(Kind::Nil, Handle::nil(), iter::empty());
                let mut binding2 = Node::new(Kind::Nil, Handle::nil(), iter::empty());
                let mut binding3 = Node::new(Kind::Nil, Handle::nil(), iter::empty());

                let identifier = format!("%{}", counter);
                counter += 1;
                let handle = Handle::from_hash(identifier.as_bytes());
                lazy.insert(handle, identifier);
                binding1.payload = Some(Payload::Identifier(handle));
                binding2.payload = Some(Payload::Identifier(handle));
                binding3.payload = Some(Payload::Identifier(handle));

                let h = binding1.id();
                lazy.insert(binding1.id(), binding1);
                let binding = h;

                let h = binding2.id();
                lazy.insert(binding2.id(), binding2);
                let var = h;

                let h = binding3.id();
                lazy.insert(binding3.id(), binding3);
                let replacement = h;
                replacements.push(replacement);

                let assignment = Node::new(
                    Kind::Binding,
                    Handle::nil(),
                    iter::once(Some(binding))
                        .chain(iter::once(None))
                        .chain(iter::once(Some(new_node))),
                );
                block.push(Some(assignment.id()));
                lazy.insert(assignment.id(), assignment);

                let mut replace = Node::new(Kind::Nil, Handle::nil(), iter::empty());

                let identifier = "replace".to_string();
                let handle = Handle::from_hash(identifier.as_bytes());
                lazy.insert(handle, identifier);
                replace.alternative = true;
                replace.payload = Some(Payload::Identifier(handle));

                let rid = replace.id();
                lazy.insert(replace.id(), replace);

                interpolate.clone_from(Node::new(
                    Kind::Application,
                    Handle::nil(),
                    iter::once(Some(rid)).chain(iter::once(Some(var))),
                ));
            } else {
                let mut binding1 = Node::new(Kind::Nil, Handle::nil(), iter::empty());
                let mut binding2 = Node::new(Kind::Nil, Handle::nil(), iter::empty());
                let mut binding3 = Node::new(Kind::Nil, Handle::nil(), iter::empty());

                let identifier = format!("%{}", counter);
                counter += 1;
                let handle = Handle::from_hash(identifier.as_bytes());
                lazy.insert(handle, identifier);
                binding1.payload = Some(Payload::Identifier(handle));
                binding2.payload = Some(Payload::Identifier(handle));
                binding3.payload = Some(Payload::Identifier(handle));

                let h = binding1.id();
                lazy.insert(binding1.id(), binding1);
                let binding = h;

                let h = binding2.id();
                lazy.insert(binding2.id(), binding2);
                let var = h;

                let h = binding3.id();
                lazy.insert(binding3.id(), binding3);
                let replacement = h;
                replacements.push(replacement);

                let assignment = Node::new(
                    Kind::Binding,
                    Handle::nil(),
                    iter::once(Some(binding))
                        .chain(iter::once(None))
                        .chain(iter::once(Some(clone))),
                );
                block.push(Some(assignment.id()));
                lazy.insert(assignment.id(), assignment);

                let mut replace = Node::new(Kind::Nil, Handle::nil(), iter::empty());

                let identifier = "replace".to_string();
                let handle = Handle::from_hash(identifier.as_bytes());
                lazy.insert(handle, identifier);
                replace.alternative = true;
                replace.payload = Some(Payload::Identifier(handle));

                let rid = replace.id();
                lazy.insert(replace.id(), replace);

                interpolate.clone_from(Node::new(
                    Kind::Application,
                    Handle::nil(),
                    iter::once(Some(rid)).chain(iter::once(Some(var))),
                ));
            }
        }

        let root = root_node.0;
        let thread = nodes.get(root).unwrap().thread;

        let mut quasiquote = Node::new(Kind::Nil, Handle::nil(), iter::empty());

        let identifier = "quasiquote".to_string();
        let handle = Handle::from_hash(identifier.as_bytes());
        lazy.insert(handle, identifier);
        quasiquote.alternative = true;
        quasiquote.payload = Some(Payload::Identifier(handle));

        let qqid = quasiquote.id();
        lazy.insert(quasiquote.id(), quasiquote);

        let quote = Node::new(
            Kind::Application,
            Handle::nil(),
            iter::once(Some(qqid)).chain(iter::once(Some(root))),
        );
        let qid = quote.id();
        lazy.insert(quote.id(), quote);

        let mut format_ast = Node::new(Kind::Nil, Handle::nil(), iter::empty());

        let identifier = "format-ast".to_string();
        let handle = Handle::from_hash(identifier.as_bytes());
        lazy.insert(handle, identifier);
        format_ast.alternative = true;
        format_ast.payload = Some(Payload::Identifier(handle));

        let faid = format_ast.id();
        lazy.insert(format_ast.id(), format_ast);

        let format = Node::new(
            Kind::Application,
            Handle::nil(),
            iter::once(Some(faid))
                .chain(iter::once(Some(qid)))
                .chain(replacements.into_iter().map(Some)),
        );
        let fid = format.id();
        lazy.insert(format.id(), format);

        block.push(Some(fid));

        let mut program = Node::new(Kind::Block, Handle::nil(), block);
        program.thread = thread;
        let root = program.id();
        lazy.insert(program.id(), program);

        root_node.0 = root;
    }
    Ok(None)
}
