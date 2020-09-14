use num_enum::{IntoPrimitive, TryFromPrimitive};
use smallvec::SmallVec;

use crate::assets::{AssetBundle, Handle, Resources};
use crate::lir::{ThreadId, Value};
use crate::scope::ScopeId;
use crate::types::TypeId;
use crate::values::ValueId;

pub type NodeId = Handle<Node>;

#[repr(u8)]
#[derive(Debug, Clone, Copy, IntoPrimitive, TryFromPrimitive)]
pub enum Kind {
    Nil,
    Block,
    Function,
    Application,
    Binding,
    Tuple,
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
    pub payload: Option<ValueId>,
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
