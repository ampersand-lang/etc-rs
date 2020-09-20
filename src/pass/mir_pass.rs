use std::iter;

use failure::Fallible;

use crate::assets::{Handle, LazyUpdate, Resources};
use crate::ast::{Kind, Node, RootNode, Visit, VisitResult};

use crate::types::{TypeGroup, TypeId, TypeOrPlaceholder};
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
        root.visit(Visit::Postorder, &nodes, |_, node| {
            min_universe = min_universe.min(node.universe);
            max_universe = max_universe.max(node.universe);
            VisitResult::Recurse
        });

        if min_universe == max_universe {
            continue;
        }

        let mut constants = Vec::new();

        root.visit(Visit::Preorder, &nodes, |_, node| {
            if node.universe == min_universe {
                constants.push(node.id());
                VisitResult::Continue
            } else {
                VisitResult::Recurse
            }
        });

        let mut block = Vec::new();
        let mut counter = 0;

        let mut replacements = Vec::new();

        for handle in constants {
            let constant = nodes.get_mut::<Node>(handle).unwrap();
            let clone = constant.as_ref().clone();
            let constant_type = TypeId {
                group: TypeGroup::None,
                concrete: TypeOrPlaceholder::Typeof(clone.id()),
            };

            let h = clone.id();
            lazy.insert(clone.id(), clone);
            let clone = h;

            let (constant, mut interpolate) = (clone, constant);

            let mut kind = Node::new(Kind::Nil, Handle::nil(), iter::empty());
            let k: u8 = Kind::Nil.into();
            kind.payload = Some(Payload::Integer(k as _));
            let h = kind.id();
            lazy.insert(h, kind);
            let kind = h;

            let mut typ = Node::new(Kind::Nil, Handle::nil(), iter::empty());
            typ.payload = Some(Payload::Type(constant_type));
            let h = typ.id();
            lazy.insert(h, typ);
            let typ = h;

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
                    .chain(iter::once(Some(typ)))
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
        }

        let root = root_node.0;

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

        let program = Node::new(Kind::Block, Handle::nil(), block);
        let root = program.id();
        lazy.insert(program.id(), program);

        root_node.0 = root;
    }
    Ok(None)
}
