use std::iter;

use crate::assets::{Handle, Resources};
use crate::ast::{Kind, Node, NodeId};
use crate::values::Payload;

#[derive(Debug)]
pub struct ReaderMacro {
    reader: String,
    function: NodeId,
}

impl ReaderMacro {
    pub fn id(&self) -> Handle<ReaderMacro> {
        Handle::from_hash(self.reader.as_bytes())
    }

    pub fn function(&self) -> NodeId {
        self.function
    }

    pub fn ident(&self) -> &str {
        &self.reader
    }
}

pub fn init(mut res: Resources<(&mut String, &mut Node, &mut ReaderMacro)>) {
    let readers = vec![
        ReaderMacro {
            reader: ">".to_string(),
            function: make_reader_alternative(&mut res, "ref"),
        },
        ReaderMacro {
            reader: "<".to_string(),
            function: make_reader_alternative(&mut res, "deref"),
        },
        ReaderMacro {
            reader: "^".to_string(),
            function: make_reader_alternative(&mut res, "ptr"),
        },
        ReaderMacro {
            reader: "-".to_string(),
            function: make_reader_alternative(&mut res, "neg"),
        },
        ReaderMacro {
            reader: "!".to_string(),
            function: make_reader_alternative(&mut res, "not"),
        },
        // NOTE: this reader has special support from the grammar
        //       and `&expr` doesn't actually return `($lower expr)`
        ReaderMacro {
            reader: "&".to_string(),
            function: make_reader_alternative(&mut res, "lower"),
        },
    ];
    for reader in readers {
        let handle = reader.id();
        res.insert(handle, reader);
    }
}

fn make_reader_alternative(
    res: &mut Resources<(&mut String, &mut Node, &mut ReaderMacro)>,
    name: &str,
) -> NodeId {
    let name = name.to_string();
    let handle = Handle::from_hash(name.as_bytes());
    res.insert(handle, name);
    let name = handle;
    let payload = Payload::Identifier(name);
    let mut node = Node::new(Kind::Nil, Handle::nil(), iter::empty());
    node.alternative = true;
    node.payload = Some(payload);
    let handle = node.id();
    res.insert(handle, node);
    handle
}
