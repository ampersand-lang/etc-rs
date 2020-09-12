use smallvec::SmallVec;

use crate::assets::Handle;
use crate::types::TypeId;
use crate::values::Value;

pub type NodeId = Handle<Node>;

#[derive(Debug, Clone, Copy)]
pub enum Kind {
    Nil,
    Block,
    Function,
    Application,
    Binding,
}

#[derive(Debug, Clone)]
pub struct Node {
    pub kind: Kind,
    pub payload: Value,
    pub type_of: Option<TypeId>,
    pub children: SmallVec<[Option<NodeId>; 4]>,
}
