//! The abstract syntax tree used throughout semantical passes of etc.
use std::fmt::{self, Debug};

use num_enum::{IntoPrimitive, TryFromPrimitive};
use smallvec::SmallVec;

use crate::assets::{AssetBundle, Handle, Resources};
use crate::lir::{ThreadId, Value};
use crate::scope::ScopeId;
use crate::types::TypeId;
use crate::values::Payload;
use crate::lexer::Location;

/// A handle to a `Node`.
pub type NodeId = Handle<Node>;

/// Describes what the contents of a `Node` are.
#[repr(u8)]
#[derive(Debug, Clone, Copy, IntoPrimitive, TryFromPrimitive)]
pub enum Kind {
    /// An empty node, containing only metadata and no children.
    ///
    /// # Children
    /// None.
    ///
    /// # Examples
    /// - `ident`
    /// - `42`
    /// - `"some string"`
    Nil,
    /// A compound expression, consisting of many stmts and one or zero expressions.
    ///
    /// # Children
    /// - 0 to n-1: statements
    /// - n: resulting expression (may be None)
    ///
    /// # Examples
    /// ```text
    /// {
    ///   x := 5 + 1;
    ///   x
    /// }
    /// ```
    Block,
    /// A mapping from one or many values to one value.
    ///
    /// # Children
    /// - 0: parameter tuple
    /// - 1: body
    ///
    /// # Examples
    /// ```text
    /// (a; b) => a + b;
    /// (a: float32; b: float32) => a + b;
    /// ```
    Function,
    /// An application of a mapping.
    ///
    /// # Children
    /// - 0: function
    /// - 1 to n: arguments
    ///
    /// # Examples
    /// ```text
    /// f a; # function is f
    /// f a, b, c; # function is f
    /// a + b; # function is +
    /// ```
    Application,
    /// A binding of a value to a variable name.
    ///
    /// # Children
    /// - 0: pattern
    /// - 1: type (may be None)
    /// - 2: value
    ///
    /// # Examples
    /// ```text
    /// x := 1;
    /// y: float32 = 1.0;
    /// f := (n) => match n, [
    ///   0 => 0;
    ///   1 => 1;
    ///   n => (f (n - 1)) + (f (n - 2));
    /// ];
    /// ```
    Binding,
    /// Declaration of a type on another node.
    ///
    /// # Children
    /// - 0: pattern or expression
    /// - 1: type
    ///
    /// # Examples
    /// ```text
    /// 0: s32;
    /// ```
    Declaration,
    /// A structurally-typed collection of values of arbitrary types.
    ///
    /// # Children
    /// - 0 to n: elements of the tuple
    ///
    /// # Examples
    /// ```text
    /// (1; 2; 3);
    /// (a; b; c);
    /// ```
    ///
    /// # Alternative
    /// A tuple preceded by a `$` is a type.
    Tuple,
    /// Index into an array.
    ///
    /// # Children
    /// - 0: value
    /// - 1 to n: indices
    ///
    /// # Examples
    /// ```text
    /// [1; 2; 3]'0;
    /// a: $[sint; 2; 2] = ...;
    /// a'0,0;
    /// ```
    Index,
    /// Accesss to a field of an expression.
    ///
    /// # Children
    /// - 0: value
    /// - 1: field
    ///
    /// # Examples
    /// ```text
    /// Foo := struct (
    ///   i: sint;
    ///   x: float;
    /// );
    /// foo := Foo 1, 2.0;
    /// foo.i;
    /// foo.x;
    /// ```
    Dotted,
    /// A structurally-typed collection of values of the same type.
    ///
    /// # Children
    /// - 0 to n: elements of the array
    ///
    /// # Examples
    /// ```text
    /// a1 := [1; 2; 3];
    /// err := [1; 2.0; "3"]; # error
    /// ```
    ///
    /// # Alternative
    /// An array preceded by a `$` is a type.
    Array,
}

/// Defines how the `Node::visit` function works.
#[derive(Debug, Clone, Copy)]
pub enum Visit {
    /// Visits `self` before visiting any children.
    Preorder,
    /// Visits `self` after visiting all children.
    Postorder,
}

/// The primary abstract syntax tree node.
///
/// All nodes are homogeneous.
#[derive(Debug, Clone)]
pub struct Node {
    /// Parent node handle, if any.
    parent: Option<NodeId>,
    /// This node's handle.
    id: NodeId,
    /// The source code mapping.
    pub location: Handle<Location>,
    /// A flag for alternative nodes.
    ///
    /// See the documentation of `Kind` for more details.
    pub alternative: bool,
    /// Describes what the contents of this node are.
    pub kind: Kind,
    /// The scope, if any.
    pub scope: Option<ScopeId>,
    /// The thread on which this node (only root node) will be executed, if any.
    pub thread: Option<ThreadId>,
    /// The low-level value of this node.
    pub value: Option<Value>,
    /// The high-level payload in an empty node.
    pub payload: Option<Payload>,
    /// The high-level type of this node, if any.
    pub type_of: Option<TypeId>,
    /// Child nodes of this node.
    ///
    /// See the documentation of `Kind` for more details.
    pub children: SmallVec<[Option<NodeId>; 4]>,
}

impl Node {
    /// Creates a new node with a `Kind` and some children.
    pub fn new<I: IntoIterator<Item = Option<NodeId>>>(kind: Kind, location: Handle<Location>, children: I) -> Self {
        Node {
            parent: None,
            id: NodeId::new(),
            location,
            alternative: false,
            kind,
            scope: None,
            thread: None,
            value: None,
            payload: None,
            type_of: None,
            children: children.into_iter().collect(),
        }
    }

    /// Creates a new node with a parent, a `Kind` and some children.
    pub fn with_parent<I: IntoIterator<Item = Option<NodeId>>>(
        parent: NodeId,
        kind: Kind,
        location: Handle<Location>,
        children: I,
    ) -> Self {
        Node {
            parent: Some(parent),
            id: NodeId::new(),
            alternative: false,
            location,
            kind,
            scope: None,
            thread: None,
            value: None,
            payload: None,
            type_of: None,
            children: children.into_iter().collect(),
        }
    }

    /// Gives the unique identifier (handle) to this node.
    pub fn id(&self) -> NodeId {
        self.id
    }

    /// Gives the parent of this node, if any.
    pub fn parent(&self) -> Option<NodeId> {
        self.parent
    }

    /// Visits immutably every child in this node and self.
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

/// An asset containing id to a root node (module).
#[derive(Debug, Clone, Copy)]
pub struct RootNode(pub NodeId);

/// The configuration for pretty-printing a node.
#[derive(Default, Debug, Clone)]
pub struct PrettyConfig {}

/// Wraps a node in a `Debug`- and `Display`-implementing structure, that can also query `NodeId`s
pub struct PrettyPrinter<'res> {
    /// The global configuration for this node.
    config: PrettyConfig,
    /// Immutable access to nodes.
    res: Resources<&'res Node>,
    /// Root node id.
    id: NodeId,
}

impl<'res> PrettyPrinter<'res> {
    /// Initializes a pretty-printer with all the fields.
    pub fn new(config: PrettyConfig, res: Resources<&'res Node>, id: NodeId) -> Self {
        Self { config, res, id }
    }

    /// Initializes a pretty-printer with a default config.
    pub fn with_default(res: Resources<&'res Node>, id: NodeId) -> Self {
        Self {
            config: Default::default(),
            res,
            id,
        }
    }

    #[allow(missing_docs)]
    pub(self) fn as_ref(&self) -> PrettyPrinterRef<'_, 'res> {
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

#[doc(hidden)]
#[allow(missing_docs)]
struct PrettyPrinterRef<'a, 'res> {
    config: &'a PrettyConfig,
    res: &'a Resources<&'res Node>,
    id: NodeId,
}

#[doc(hidden)]
#[allow(missing_docs)]
impl<'a, 'res> PrettyPrinterRef<'a, 'res> {
    pub fn new(config: &'a PrettyConfig, res: &'a Resources<&'res Node>, id: NodeId) -> Self {
        Self { config, res, id }
    }
}

impl<'a, 'res> Debug for PrettyPrinterRef<'a, 'res> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let node = self.res.get::<Node>(self.id).unwrap();
        match node.kind {
            Kind::Nil => f
                .debug_struct("Nil")
                .field("value", node.payload.as_ref().unwrap())
                .finish(),
            Kind::Block => f
                .debug_struct("Block")
                .field(
                    "stmts",
                    &node
                        .children
                        .iter()
                        .take(node.children.len() - 1)
                        .map(|node| PrettyPrinterRef::new(self.config, self.res, node.unwrap()))
                        .collect::<Vec<_>>(),
                )
                .field(
                    "expr",
                    &node.children.last().and_then(|node| {
                        node.map(|node| PrettyPrinterRef::new(self.config, self.res, node))
                    }),
                )
                .finish(),
            Kind::Function => f
                .debug_struct("Function")
                .field(
                    "parameters",
                    &node.children[0]
                        .map(|node| PrettyPrinterRef::new(self.config, self.res, node))
                        .unwrap(),
                )
                .field(
                    "body",
                    &node.children[1]
                        .map(|node| PrettyPrinterRef::new(self.config, self.res, node))
                        .unwrap(),
                )
                .finish(),
            Kind::Application => f
                .debug_struct("Application")
                .field(
                    "function",
                    &node
                        .children
                        .first()
                        .and_then(|node| {
                            node.map(|node| PrettyPrinterRef::new(self.config, self.res, node))
                        })
                        .unwrap(),
                )
                .field(
                    "arguments",
                    &node
                        .children
                        .iter()
                        .skip(1)
                        .map(|node| PrettyPrinterRef::new(self.config, self.res, node.unwrap()))
                        .collect::<Vec<_>>(),
                )
                .finish(),
            Kind::Binding => f
                .debug_struct("Binding")
                .field(
                    "pattern",
                    &node.children[0]
                        .map(|node| PrettyPrinterRef::new(self.config, self.res, node))
                        .unwrap(),
                )
                .field(
                    "type",
                    &node.children[1]
                        .map(|node| PrettyPrinterRef::new(self.config, self.res, node)),
                )
                .field(
                    "value",
                    &node.children[2]
                        .map(|node| PrettyPrinterRef::new(self.config, self.res, node))
                        .unwrap(),
                )
                .finish(),
            Kind::Declaration => f
                .debug_struct("Declaration")
                .field(
                    "pattern",
                    &node.children[0]
                        .map(|node| PrettyPrinterRef::new(self.config, self.res, node))
                        .unwrap(),
                )
                .field(
                    "type",
                    &node.children[1]
                        .map(|node| PrettyPrinterRef::new(self.config, self.res, node))
                        .unwrap(),
                )
                .finish(),
            Kind::Tuple => f
                .debug_struct("Tuple")
                .field(
                    "fields",
                    &node
                        .children
                        .iter()
                        .map(|node| PrettyPrinterRef::new(self.config, self.res, node.unwrap()))
                        .collect::<Vec<_>>(),
                )
                .finish(),
            Kind::Index => f
                .debug_struct("Index")
                .field(
                    "value",
                    &node.children[0]
                        .map(|node| PrettyPrinterRef::new(self.config, self.res, node))
                        .unwrap(),
                )
                .field(
                    "index",
                    &node.children[1]
                        .map(|node| PrettyPrinterRef::new(self.config, self.res, node))
                        .unwrap(),
                )
                .finish(),
            Kind::Dotted => f
                .debug_struct("Dotted")
                .field(
                    "value",
                    &node.children[0]
                        .map(|node| PrettyPrinterRef::new(self.config, self.res, node))
                        .unwrap(),
                )
                .field(
                    "field",
                    &node.children[1]
                        .map(|node| PrettyPrinterRef::new(self.config, self.res, node))
                        .unwrap(),
                )
                .finish(),
            Kind::Array => f
                .debug_struct("Array")
                .field(
                    "elements",
                    &node
                        .children
                        .iter()
                        .map(|node| PrettyPrinterRef::new(self.config, self.res, node.unwrap()))
                        .collect::<Vec<_>>(),
                )
                .finish(),
        }
    }
}
