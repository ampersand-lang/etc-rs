use std::fmt::{self, Debug, Display};

use num_enum::{IntoPrimitive, TryFromPrimitive};
use smallvec::SmallVec;

use crate::assets::{AssetBundle, Handle, Resources};
use crate::lir::{ThreadId, Value};
use crate::scope::ScopeId;
use crate::types::TypeId;
use crate::values::Payload;

pub type NodeId = Handle<Node>;

#[repr(u8)]
#[derive(Debug, Clone, Copy, IntoPrimitive, TryFromPrimitive)]
pub enum Kind {
    Nil,
    Block,
    Function,
    Application,
    Binding,
    Declaration,
    Tuple,
    TupleType,
    Index,
    Dotted,
    Array,
}

#[derive(Debug, Clone, Copy)]
pub enum Visit {
    Preorder,
    Postorder,
}

#[derive(Debug, Clone)]
pub struct Node {
    parent: Option<NodeId>,
    id: NodeId,
    pub kind: Kind,
    pub scope: Option<ScopeId>,
    pub thread: Option<ThreadId>,
    pub value: Option<Value>,
    pub payload: Option<Payload>,
    pub type_of: Option<TypeId>,
    pub is_const: bool,
    pub children: SmallVec<[Option<NodeId>; 4]>,
}

impl Node {
    pub fn new<I: IntoIterator<Item = Option<NodeId>>>(kind: Kind, children: I) -> Self {
        Node {
            parent: None,
            id: NodeId::new(),
            kind,
            scope: None,
            thread: None,
            value: None,
            payload: None,
            type_of: None,
            is_const: false,
            children: children.into_iter().collect(),
        }
    }

    pub fn with_parent<I: IntoIterator<Item = Option<NodeId>>>(
        parent: NodeId,
        kind: Kind,
        children: I,
    ) -> Self {
        Node {
            parent: Some(parent),
            id: NodeId::new(),
            kind,
            scope: None,
            thread: None,
            value: None,
            payload: None,
            type_of: None,
            is_const: false,
            children: children.into_iter().collect(),
        }
    }

    pub fn id(&self) -> NodeId {
        self.id
    }

    pub fn parent(&self) -> Option<NodeId> {
        self.parent
    }

    pub fn visit<A: AssetBundle, F>(&self, visit: Visit, res: &Resources<A>, mut f: F)
    where
        F: FnMut(&Resources<A>, Option<&Self>),
    {
        if let Visit::Preorder = visit {
            f(res, Some(self));
        }
        for child in &self.children {
            let node = child.as_ref().map(|handle| res.get(*handle).unwrap());
            f(res, node.as_ref().map(AsRef::as_ref));
        }
        if let Visit::Postorder = visit {
            f(res, Some(self));
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct RootNode(pub NodeId);

#[derive(Default, Debug, Clone)]
pub struct PrettyConfig {
}

pub struct PrettyPrinter<'res> {
    config: PrettyConfig,
    res: Resources<&'res Node>,
    id: NodeId,
}

impl<'res> PrettyPrinter<'res> {
    pub fn new(config: PrettyConfig, res: Resources<&'res Node>, id: NodeId) -> Self {
        Self {
            config,
            res,
            id,
        }
    }
    
    pub fn with_default(res: Resources<&'res Node>, id: NodeId) -> Self {
        Self {
            config: Default::default(),
            res,
            id,
        }
    }

    pub fn as_ref(&self) -> PrettyPrinterRef<'_, 'res> {
        PrettyPrinterRef {
            config: &self.config,
            res: &self.res,
            id: self.id,
        }
    }
}

impl<'res> Debug for PrettyPrinter<'res> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.as_ref().fmt(f)
    }
}

pub struct PrettyPrinterRef<'a, 'res> {
    config: &'a PrettyConfig,
    res: &'a Resources<&'res Node>,
    id: NodeId,
}

impl<'a, 'res> PrettyPrinterRef<'a, 'res> {
    pub fn new(config: &'a PrettyConfig, res: &'a Resources<&'res Node>, id: NodeId) -> Self {
        Self {
            config,
            res,
            id,
        }
    }
}

impl<'a, 'res> Debug for PrettyPrinterRef<'a, 'res> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let node = self.res.get::<Node>(self.id).unwrap();
        match node.kind {
            Kind::Nil => {
                f.debug_struct("Nil")
                    .field("value", node.payload.as_ref().unwrap())
                    .finish()
            }
            Kind::Block => {
                f.debug_struct("Block")
                    .field("stmts", &node.children.iter().take(node.children.len() - 1).map(|node| PrettyPrinterRef::new(self.config, self.res, node.unwrap())).collect::<Vec<_>>())
                    .field("expr", &node.children.last().and_then(|node| node.map(|node| PrettyPrinterRef::new(self.config, self.res, node))))
                    .finish()
            }
            Kind::Function => {
                f.debug_struct("Function")
                    .field("parameters", &node.children[0].map(|node| PrettyPrinterRef::new(self.config, self.res, node)).unwrap())
                    .field("body", &node.children[1].map(|node| PrettyPrinterRef::new(self.config, self.res, node)).unwrap())
                    .finish()
            }
            Kind::Application => {
                f.debug_struct("Application")
                    .field("function", &node.children.first().and_then(|node| node.map(|node| PrettyPrinterRef::new(self.config, self.res, node))).unwrap())
                    .field("arguments", &node.children.iter().skip(1).map(|node| PrettyPrinterRef::new(self.config, self.res, node.unwrap())).collect::<Vec<_>>())
                    .finish()
            }
            Kind::Binding => {
                f.debug_struct("Binding")
                    .field("pattern", &node.children[0].map(|node| PrettyPrinterRef::new(self.config, self.res, node)).unwrap())
                    .field("type", &node.children[1].map(|node| PrettyPrinterRef::new(self.config, self.res, node)))
                    .field("value", &node.children[2].map(|node| PrettyPrinterRef::new(self.config, self.res, node)).unwrap())
                    .finish()
            }
            Kind::Declaration => {
                f.debug_struct("Declaration")
                    .field("pattern", &node.children[0].map(|node| PrettyPrinterRef::new(self.config, self.res, node)).unwrap())
                    .field("type", &node.children[1].map(|node| PrettyPrinterRef::new(self.config, self.res, node)).unwrap())
                    .finish()
            }
            Kind::Tuple => {
                f.debug_struct("Tuple")
                    .field("fields", &node.children.iter().map(|node| PrettyPrinterRef::new(self.config, self.res, node.unwrap())).collect::<Vec<_>>())
                    .finish()
            }
            Kind::TupleType => {
                f.debug_struct("TupleType")
                    .field("fields", &node.children.iter().map(|node| PrettyPrinterRef::new(self.config, self.res, node.unwrap())).collect::<Vec<_>>())
                    .finish()
            }
            Kind::Index => {
                f.debug_struct("Index")
                    .field("value", &node.children[0].map(|node| PrettyPrinterRef::new(self.config, self.res, node)).unwrap())
                    .field("index", &node.children[1].map(|node| PrettyPrinterRef::new(self.config, self.res, node)).unwrap())
                    .finish()
            }
            Kind::Dotted => {
                f.debug_struct("Dotted")
                    .field("value", &node.children[0].map(|node| PrettyPrinterRef::new(self.config, self.res, node)).unwrap())
                    .field("field", &node.children[1].map(|node| PrettyPrinterRef::new(self.config, self.res, node)).unwrap())
                    .finish()
            }
            Kind::Array => {
                f.debug_struct("Array")
                    .field("elements", &node.children.iter().map(|node| PrettyPrinterRef::new(self.config, self.res, node.unwrap())).collect::<Vec<_>>())
                    .finish()
            }
        }
    }
}
