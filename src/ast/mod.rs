//! The abstract syntax tree used throughout semantical passes of etc.
use std::fmt::{self, Debug, Display};

use hashbrown::HashSet;
use num_enum::{IntoPrimitive, TryFromPrimitive};
use smallvec::SmallVec;

use crate::assets::{AssetBundle, Handle, Resources};
use crate::lexer::Location;
use crate::lir::{ThreadId, Value};
use crate::scope::ScopeId;
use crate::types::{NamedType, TypeId};
use crate::values::{self, Payload};

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
    /// A named argument.
    ///
    /// # Children
    /// - 0: an identifier
    /// - 1: a pattern or expression
    ///
    /// # Examples
    /// ```text
    /// f a: 1;
    /// ```
    Argument,
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
    /// An application or binary expression with an effect handler.
    ///
    /// # Children
    /// - 0: application
    /// - 1 to n: handler declarations
    ///
    /// # Examples
    /// ```text
    /// parse-int "1023" with! _e => yield 0;
    /// ```
    With,
}

/// Defines how the `Node::visit` function works.
#[derive(Debug, Clone, Copy)]
pub enum Visit {
    /// Visits `self` before visiting any children.
    Preorder,
    /// Visits `self` after visiting all children.
    Postorder,
}

/// Shows whether the visitor was run before or after children.
#[derive(Debug, Clone, Copy)]
pub enum Which {
    /// The visitor was run before any children.
    Before,
    /// The visitor was run after all children.
    After,
}

/// Defines the next step in visiting.
#[derive(Debug, Clone, Copy)]
pub enum VisitResult {
    /// Continue recursing into other nodes.
    ///
    /// Ignored if `Visit` was set to `Visit::Postorder`.
    Recurse,
    /// Continue without recursing into other nodes.
    ///
    /// Ignored if `Visit` was set to `Visit::Postorder`.
    Continue,
    /// Completely break the visit function.
    Break,
    /// Repeat recursion at parent, without recursing into `.0`.
    ///
    /// Ignored if `Visit` was set to `Visit::Preorder`.
    Repeat(NodeId),
}

/// Defines the next step in visiting.
#[derive(Debug, Clone, Copy)]
enum PrivateVisitResult {
    /// Continue recursing into other nodes.
    ///
    /// Ignored if `Visit` was set to `Visit::Postorder`.
    Recurse,
    /// Completely break the visit function.
    Break,
    /// Repeat recursion at parent, without recursing into `.0`.
    ///
    /// Ignored if `Visit` was set to `Visit::Preorder`.
    Repeat(NodeId),
}

/// The primary abstract syntax tree node.
///
/// All nodes are homogeneous.
#[derive(Debug)]
pub struct Node {
    /// This node's handle.
    id: NodeId,
    /// A flag that shows if a newnode should be created.
    pub no_newnode: bool,
    /// A flag for nodes that are generic over some number of compile-time variables,
    ///  i.e. variables that get `replace`d in using `format-ast`.
    ///
    /// Example:
    /// ```text
    /// # Here id is a -1 non-explicit function,
    /// #  but because it returns a higher-universe (0) value,
    //  #  its body, i.e. `{a: t} => a`, is set to generic over "t".
    /// id := {t: type} => {a: t} => a;
    /// ```
    pub generic: Option<SmallVec<[Handle<String>; 4]>>,
    /// A flag that represents if this application is a call to a generic function and if it is, which one
    pub generic_call: Option<NodeId>,
    /// The order in which nodes get compiled and optionally executed.
    pub universe: i32,
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
    pub fn new<I: IntoIterator<Item = Option<NodeId>>>(
        kind: Kind,
        location: Handle<Location>,
        children: I,
    ) -> Self {
        Node {
            id: NodeId::new(),
            no_newnode: false,
            generic: None,
            generic_call: None,
            universe: 0,
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

    pub fn clone_from(&mut self, other: Self) {
        let id = self.id;
        *self = Self { id, ..other }
    }

    /// Gives the unique identifier (handle) to this node.
    pub fn id(&self) -> NodeId {
        self.id
    }

    /// Visits immutably every child in this node and self.
    fn private_visit<A: AssetBundle, F>(
        &self,
        parent: Option<&Self>,
        visit: Visit,
        res: &Resources<A>,
        f: &mut F,
    ) -> PrivateVisitResult
    where
        F: FnMut(&Resources<A>, &Self, Option<&Self>) -> VisitResult,
    {
        let mut skip = HashSet::new();
        let mut first = true;
        let mut do_loop = false;
        while first || do_loop {
            first = false;
            if let Visit::Preorder = visit {
                match f(res, self, parent) {
                    VisitResult::Recurse => {}
                    VisitResult::Continue => return PrivateVisitResult::Recurse,
                    VisitResult::Break => return PrivateVisitResult::Break,
                    VisitResult::Repeat(_) => {}
                }
            }
            for child in &self.children {
                if let Some(&handle) = child.as_ref() {
                    if skip.contains(&handle) {
                        continue;
                    }

                    let node = res.get(handle).unwrap();
                    match node.private_visit(Some(self), visit, res, f) {
                        PrivateVisitResult::Recurse => {}
                        PrivateVisitResult::Break => return PrivateVisitResult::Break,
                        PrivateVisitResult::Repeat(node) => {
                            skip.insert(node);
                            do_loop = true;
                        }
                    }
                }
            }
            if let Visit::Postorder = visit {
                match f(res, self, parent) {
                    VisitResult::Recurse => return PrivateVisitResult::Recurse,
                    VisitResult::Continue => return PrivateVisitResult::Recurse,
                    VisitResult::Break => return PrivateVisitResult::Break,
                    VisitResult::Repeat(node) => return PrivateVisitResult::Repeat(node),
                }
            }
        }
        PrivateVisitResult::Recurse
    }

    /// Visits immutably every child in this node and self.
    pub fn visit<A: AssetBundle, F>(&self, visit: Visit, res: &Resources<A>, mut f: F)
    where
        F: FnMut(&Resources<A>, &Self, Option<&Self>) -> VisitResult,
    {
        self.private_visit(None, visit, res, &mut f);
    }

    /// Visits immutably every child in this node and self.
    fn private_visit_twice<A: AssetBundle, F>(
        &self,
        parent: Option<&Self>,
        res: &Resources<A>,
        f: &mut F,
    ) -> PrivateVisitResult
    where
        F: FnMut(&Resources<A>, &Self, Option<&Self>, Which) -> VisitResult,
    {
        match f(res, self, parent, Which::Before) {
            VisitResult::Recurse => {}
            VisitResult::Continue => return PrivateVisitResult::Recurse,
            VisitResult::Break => return PrivateVisitResult::Break,
            VisitResult::Repeat(_) => {}
        }
        for child in &self.children {
            if let Some(&handle) = child.as_ref() {
                let node = res.get(handle).unwrap();
                match node.private_visit_twice(Some(self), res, f) {
                    PrivateVisitResult::Recurse => {}
                    PrivateVisitResult::Break => return PrivateVisitResult::Break,
                    PrivateVisitResult::Repeat(_) => {}
                }
            }
        }
        match f(res, self, parent, Which::After) {
            VisitResult::Recurse => PrivateVisitResult::Recurse,
            VisitResult::Continue => PrivateVisitResult::Recurse,
            VisitResult::Break => PrivateVisitResult::Break,
            VisitResult::Repeat(node) => PrivateVisitResult::Repeat(node),
        }
    }

    /// Visits immutably every child in this node and self.
    pub fn visit_twice<A: AssetBundle, F>(&self, res: &Resources<A>, mut f: F)
    where
        F: FnMut(&Resources<A>, &Self, Option<&Self>, Which) -> VisitResult,
    {
        self.private_visit_twice(None, res, &mut f);
    }

    /// Visits immutably every child in this node and self.
    fn private_visit_twice_with_rev<A: AssetBundle, F>(
        &self,
        parent: Option<&Self>,
        res: &Resources<A>,
        f: &mut F,
    ) -> PrivateVisitResult
    where
        F: FnMut(&Resources<A>, &Self, Option<&Self>, Which) -> (VisitResult, bool),
    {
        let do_rev = match f(res, self, parent, Which::Before) {
            (VisitResult::Recurse, r) => r,
            (VisitResult::Continue, _) => return PrivateVisitResult::Recurse,
            (VisitResult::Break, _) => return PrivateVisitResult::Break,
            (VisitResult::Repeat(_), r) => r,
        };
        if do_rev {
            for child in self.children.iter().rev() {
                if let Some(&handle) = child.as_ref() {
                    let node = res.get(handle).unwrap();
                    match node.private_visit_twice_with_rev(Some(self), res, f) {
                        PrivateVisitResult::Recurse => {}
                        PrivateVisitResult::Break => return PrivateVisitResult::Break,
                        PrivateVisitResult::Repeat(_) => {}
                    }
                }
            }
        } else {
            for child in &self.children {
                if let Some(&handle) = child.as_ref() {
                    let node = res.get(handle).unwrap();
                    match node.private_visit_twice_with_rev(Some(self), res, f) {
                        PrivateVisitResult::Recurse => {}
                        PrivateVisitResult::Break => return PrivateVisitResult::Break,
                        PrivateVisitResult::Repeat(_) => {}
                    }
                }
            }
        }
        match f(res, self, parent, Which::After).0 {
            VisitResult::Recurse => PrivateVisitResult::Recurse,
            VisitResult::Continue => PrivateVisitResult::Recurse,
            VisitResult::Break => PrivateVisitResult::Break,
            VisitResult::Repeat(node) => PrivateVisitResult::Repeat(node),
        }
    }

    /// Visits immutably every child in this node and self.
    pub fn visit_twice_with_rev<A: AssetBundle, F>(&self, res: &Resources<A>, mut f: F)
    where
        F: FnMut(&Resources<A>, &Self, Option<&Self>, Which) -> (VisitResult, bool),
    {
        self.private_visit_twice_with_rev(None, res, &mut f);
    }

    /// Clones self with a closure.
    fn private_clone_with<A: AssetBundle, F>(&self, res: &mut Resources<A>, f: &mut F) -> Self
    where
        F: FnMut(&mut Resources<A>, &Self, SmallVec<[Option<NodeId>; 4]>) -> Self,
    {
        let mut children = SmallVec::new();
        for child in &self.children {
            if let Some(&handle) = child.as_ref() {
                // PERF: clone is inefficient?
                let node = res.get(handle).unwrap().as_ref().clone();
                let node = node.private_clone_with(res, f);
                let handle = node.id();
                res.insert(node.id(), node);
                children.push(Some(handle));
            } else {
                children.push(None);
            }
        }
        f(res, self, children)
    }

    /// Clones self with a closure.
    pub fn clone_with<A: AssetBundle, F>(&self, res: &mut Resources<A>, mut f: F) -> Self
    where
        F: FnMut(&mut Resources<A>, &Self, SmallVec<[Option<NodeId>; 4]>) -> Self,
    {
        self.private_clone_with(res, &mut f)
    }
}

impl Clone for Node {
    /// Clones a node and assigns it a new unique identifier.
    fn clone(&self) -> Self {
        Self {
            id: NodeId::new(),
            no_newnode: self.no_newnode,
            generic: self.generic.clone(),
            generic_call: self.generic_call.clone(),
            universe: self.universe,
            location: self.location,
            alternative: self.alternative,
            kind: self.kind,
            scope: self.scope,
            thread: self.thread,
            value: self.value,
            payload: self.payload,
            type_of: self.type_of,
            children: self.children.clone(),
        }
    }
}

/// An asset containing id to a root node (module).
#[derive(Debug, Clone, Copy)]
pub struct RootNode(pub NodeId, pub bool);

/// The configuration for pretty-printing a node.
#[derive(Default, Debug, Clone, Copy)]
pub struct PrettyConfig {}

/// Wraps a node in a `Debug`- and `Display`-implementing structure, that can also query `NodeId`s
pub struct PrettyPrinter<'res> {
    /// The global configuration for this node.
    config: PrettyConfig,
    /// Immutable access to nodes.
    res: Resources<&'res Node>,
    /// Immutable access to types.
    types: Resources<&'res NamedType>,
    /// Immutable access to types.
    strings: Resources<&'res String>,
    /// Root node id.
    id: NodeId,
}

impl<'res> PrettyPrinter<'res> {
    /// Initializes a pretty-printer with all the fields.
    pub fn new(
        config: PrettyConfig,
        res: Resources<&'res Node>,
        types: Resources<&'res NamedType>,
        strings: Resources<&'res String>,
        id: NodeId,
    ) -> Self {
        Self {
            config,
            res,
            types,
            strings,
            id,
        }
    }

    /// Initializes a pretty-printer with a default config.
    pub fn with_default(
        res: Resources<&'res Node>,
        types: Resources<&'res NamedType>,
        strings: Resources<&'res String>,
        id: NodeId,
    ) -> Self {
        Self::new(Default::default(), res, types, strings, id)
    }

    #[allow(missing_docs)]
    pub(self) fn as_ref(&self) -> PrettyPrinterRef<'_, 'res> {
        PrettyPrinterRef {
            config: self.config,
            res: &self.res,
            types: &self.types,
            strings: &self.strings,
            id: self.id,
        }
    }
}

impl<'res> Debug for PrettyPrinter<'res> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        Debug::fmt(&self.as_ref(), f)
    }
}

impl<'res> Display for PrettyPrinter<'res> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        Display::fmt(&self.as_ref(), f)
    }
}

#[doc(hidden)]
#[allow(missing_docs)]
pub struct PrettyPrinterRef<'a, 'res> {
    config: PrettyConfig,
    res: &'a Resources<&'res Node>,
    types: &'a Resources<&'res NamedType>,
    strings: &'a Resources<&'res String>,
    id: NodeId,
}

#[doc(hidden)]
#[allow(missing_docs)]
impl<'a, 'res> PrettyPrinterRef<'a, 'res> {
    pub fn new(
        config: PrettyConfig,
        res: &'a Resources<&'res Node>,
        types: &'a Resources<&'res NamedType>,
        strings: &'a Resources<&'res String>,
        id: NodeId,
    ) -> Self {
        Self {
            config,
            res,
            types,
            strings,
            id,
        }
    }
}

impl<'a, 'res> Debug for PrettyPrinterRef<'a, 'res> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let node = self.res.get::<Node>(self.id).unwrap();
        match node.kind {
            Kind::Nil => f
                .debug_struct("Nil")
                .field("universe", &node.universe)
                .field("scope", &node.scope)
                .field("value", node.payload.as_ref().unwrap())
                .finish(),
            Kind::Block => f
                .debug_struct("Block")
                .field("universe", &node.universe)
                .field("scope", &node.scope)
                .field(
                    "stmts",
                    &node
                        .children
                        .iter()
                        .take(node.children.len() - 1)
                        .map(|node| {
                            PrettyPrinterRef::new(
                                self.config,
                                self.res,
                                self.types,
                                self.strings,
                                node.unwrap(),
                            )
                        })
                        .collect::<Vec<_>>(),
                )
                .field(
                    "expr",
                    &node.children.last().and_then(|node| {
                        node.map(|node| {
                            PrettyPrinterRef::new(
                                self.config,
                                self.res,
                                self.types,
                                self.strings,
                                node,
                            )
                        })
                    }),
                )
                .finish(),
            Kind::Function => f
                .debug_struct("Function")
                .field("universe", &node.universe)
                .field("scope", &node.scope)
                .field(
                    "parameters",
                    &node.children[0]
                        .map(|node| {
                            PrettyPrinterRef::new(
                                self.config,
                                self.res,
                                self.types,
                                self.strings,
                                node,
                            )
                        })
                        .unwrap(),
                )
                .field(
                    "body",
                    &node.children[1]
                        .map(|node| {
                            PrettyPrinterRef::new(
                                self.config,
                                self.res,
                                self.types,
                                self.strings,
                                node,
                            )
                        })
                        .unwrap(),
                )
                .finish(),
            Kind::Application => f
                .debug_struct("Application")
                .field("universe", &node.universe)
                .field("scope", &node.scope)
                .field(
                    "function",
                    &node
                        .children
                        .first()
                        .and_then(|node| {
                            node.map(|node| {
                                PrettyPrinterRef::new(
                                    self.config,
                                    self.res,
                                    self.types,
                                    self.strings,
                                    node,
                                )
                            })
                        })
                        .unwrap(),
                )
                .field(
                    "arguments",
                    &node
                        .children
                        .iter()
                        .skip(1)
                        .map(|node| {
                            PrettyPrinterRef::new(
                                self.config,
                                self.res,
                                self.types,
                                self.strings,
                                node.unwrap(),
                            )
                        })
                        .collect::<Vec<_>>(),
                )
                .finish(),
            Kind::Binding => f
                .debug_struct("Binding")
                .field("universe", &node.universe)
                .field("scope", &node.scope)
                .field(
                    "pattern",
                    &node.children[0]
                        .map(|node| {
                            PrettyPrinterRef::new(
                                self.config,
                                self.res,
                                self.types,
                                self.strings,
                                node,
                            )
                        })
                        .unwrap(),
                )
                .field(
                    "type",
                    &node.children[1].map(|node| {
                        PrettyPrinterRef::new(self.config, self.res, self.types, self.strings, node)
                    }),
                )
                .field(
                    "value",
                    &node.children[2]
                        .map(|node| {
                            PrettyPrinterRef::new(
                                self.config,
                                self.res,
                                self.types,
                                self.strings,
                                node,
                            )
                        })
                        .unwrap(),
                )
                .finish(),
            Kind::Argument => f
                .debug_struct("Argument")
                .field("universe", &node.universe)
                .field("scope", &node.scope)
                .field(
                    "name",
                    &node.children[0]
                        .map(|node| {
                            PrettyPrinterRef::new(
                                self.config,
                                self.res,
                                self.types,
                                self.strings,
                                node,
                            )
                        })
                        .unwrap(),
                )
                .field(
                    "value",
                    &node.children[1]
                        .map(|node| {
                            PrettyPrinterRef::new(
                                self.config,
                                self.res,
                                self.types,
                                self.strings,
                                node,
                            )
                        })
                        .unwrap(),
                )
                .finish(),
            Kind::Declaration => f
                .debug_struct("Declaration")
                .field("universe", &node.universe)
                .field("scope", &node.scope)
                .field(
                    "pattern",
                    &node.children[0]
                        .map(|node| {
                            PrettyPrinterRef::new(
                                self.config,
                                self.res,
                                self.types,
                                self.strings,
                                node,
                            )
                        })
                        .unwrap(),
                )
                .field(
                    "type",
                    &node.children[1]
                        .map(|node| {
                            PrettyPrinterRef::new(
                                self.config,
                                self.res,
                                self.types,
                                self.strings,
                                node,
                            )
                        })
                        .unwrap(),
                )
                .finish(),
            Kind::Tuple => f
                .debug_struct("Tuple")
                .field("universe", &node.universe)
                .field("scope", &node.scope)
                .field(
                    "fields",
                    &node
                        .children
                        .iter()
                        .map(|node| {
                            PrettyPrinterRef::new(
                                self.config,
                                self.res,
                                self.types,
                                self.strings,
                                node.unwrap(),
                            )
                        })
                        .collect::<Vec<_>>(),
                )
                .finish(),
            Kind::Index => f
                .debug_struct("Index")
                .field("universe", &node.universe)
                .field("scope", &node.scope)
                .field(
                    "value",
                    &node.children[0]
                        .map(|node| {
                            PrettyPrinterRef::new(
                                self.config,
                                self.res,
                                self.types,
                                self.strings,
                                node,
                            )
                        })
                        .unwrap(),
                )
                .field(
                    "index",
                    &node.children[1]
                        .map(|node| {
                            PrettyPrinterRef::new(
                                self.config,
                                self.res,
                                self.types,
                                self.strings,
                                node,
                            )
                        })
                        .unwrap(),
                )
                .finish(),
            Kind::Dotted => f
                .debug_struct("Dotted")
                .field("universe", &node.universe)
                .field("scope", &node.scope)
                .field(
                    "value",
                    &node.children[0]
                        .map(|node| {
                            PrettyPrinterRef::new(
                                self.config,
                                self.res,
                                self.types,
                                self.strings,
                                node,
                            )
                        })
                        .unwrap(),
                )
                .field(
                    "field",
                    &node.children[1]
                        .map(|node| {
                            PrettyPrinterRef::new(
                                self.config,
                                self.res,
                                self.types,
                                self.strings,
                                node,
                            )
                        })
                        .unwrap(),
                )
                .finish(),
            Kind::Array => f
                .debug_struct("Array")
                .field("universe", &node.universe)
                .field("scope", &node.scope)
                .field(
                    "elements",
                    &node
                        .children
                        .iter()
                        .map(|node| {
                            PrettyPrinterRef::new(
                                self.config,
                                self.res,
                                self.types,
                                self.strings,
                                node.unwrap(),
                            )
                        })
                        .collect::<Vec<_>>(),
                )
                .finish(),
            Kind::With => f
                .debug_struct("With")
                .field("universe", &node.universe)
                .field("scope", &node.scope)
                .field(
                    "application",
                    &node
                        .children
                        .first()
                        .and_then(|node| {
                            node.map(|node| {
                                PrettyPrinterRef::new(
                                    self.config,
                                    self.res,
                                    self.types,
                                    self.strings,
                                    node,
                                )
                            })
                        })
                        .unwrap(),
                )
                .field(
                    "handlers",
                    &node
                        .children
                        .iter()
                        .skip(1)
                        .map(|node| {
                            PrettyPrinterRef::new(
                                self.config,
                                self.res,
                                self.types,
                                self.strings,
                                node.unwrap(),
                            )
                        })
                        .collect::<Vec<_>>(),
                )
                .finish(),
        }
    }
}

impl<'a, 'res> Display for PrettyPrinterRef<'a, 'res> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let node = self.res.get::<Node>(self.id).unwrap();
        if node.alternative {
            write!(f, "$")?;
        }
        match node.kind {
            Kind::Nil => write!(
                f,
                "{}",
                values::PrettyPrinterRef::new(
                    Default::default(),
                    self.types,
                    self.strings,
                    node.payload.unwrap(),
                ),
            ),
            Kind::Block => {
                write!(f, "( ")?;
                for stmt in node
                    .children
                    .iter()
                    .take(node.children.len().saturating_sub(1))
                {
                    write!(
                        f,
                        "{}; ",
                        PrettyPrinterRef::new(
                            self.config,
                            self.res,
                            self.types,
                            self.strings,
                            stmt.unwrap()
                        )
                    )?;
                }
                let expr = node.children.last().unwrap();
                if let Some(expr) = expr {
                    write!(
                        f,
                        "{} ",
                        PrettyPrinterRef::new(
                            self.config,
                            self.res,
                            self.types,
                            self.strings,
                            *expr
                        )
                    )?;
                }
                write!(f, ")")
            }
            Kind::Function => write!(
                f,
                "{} => {}",
                node.children[0]
                    .map(|node| PrettyPrinterRef::new(
                        self.config,
                        self.res,
                        self.types,
                        self.strings,
                        node
                    ))
                    .unwrap(),
                node.children[1]
                    .map(|node| PrettyPrinterRef::new(
                        self.config,
                        self.res,
                        self.types,
                        self.strings,
                        node
                    ))
                    .unwrap(),
            ),
            Kind::Application => {
                write!(
                    f,
                    "{} ",
                    &node
                        .children
                        .first()
                        .and_then(|node| {
                            node.map(|node| {
                                PrettyPrinterRef::new(
                                    self.config,
                                    self.res,
                                    self.types,
                                    self.strings,
                                    node,
                                )
                            })
                        })
                        .unwrap(),
                )?;
                for (i, argument) in node.children.iter().enumerate().skip(1) {
                    if i > 1 {
                        write!(f, ", ")?;
                    }
                    let argument = PrettyPrinterRef::new(
                        self.config,
                        self.res,
                        self.types,
                        self.strings,
                        argument.unwrap(),
                    );
                    write!(f, "{}", argument)?;
                }
                Ok(())
            }
            Kind::Binding => write!(
                f,
                "{} := {}",
                node.children[0]
                    .map(|node| PrettyPrinterRef::new(
                        self.config,
                        self.res,
                        self.types,
                        self.strings,
                        node
                    ))
                    .unwrap(),
                node.children[2]
                    .map(|node| PrettyPrinterRef::new(
                        self.config,
                        self.res,
                        self.types,
                        self.strings,
                        node
                    ))
                    .unwrap(),
            ),
            Kind::Argument => write!(
                f,
                "{}: {}",
                node.children[0]
                    .map(|node| PrettyPrinterRef::new(
                        self.config,
                        self.res,
                        self.types,
                        self.strings,
                        node
                    ))
                    .unwrap(),
                node.children[1]
                    .map(|node| PrettyPrinterRef::new(
                        self.config,
                        self.res,
                        self.types,
                        self.strings,
                        node
                    ))
                    .unwrap(),
            ),
            Kind::Declaration => write!(
                f,
                "{}: {}",
                node.children[0]
                    .map(|node| PrettyPrinterRef::new(
                        self.config,
                        self.res,
                        self.types,
                        self.strings,
                        node
                    ))
                    .unwrap(),
                node.children[1]
                    .map(|node| PrettyPrinterRef::new(
                        self.config,
                        self.res,
                        self.types,
                        self.strings,
                        node
                    ))
                    .unwrap(),
            ),
            Kind::Tuple => {
                write!(f, "{{")?;
                for stmt in node
                    .children
                    .iter()
                    .take(node.children.len().saturating_sub(1))
                {
                    write!(
                        f,
                        "{}; ",
                        PrettyPrinterRef::new(
                            self.config,
                            self.res,
                            self.types,
                            self.strings,
                            stmt.unwrap()
                        )
                    )?;
                }
                let expr = node.children.last().map(Option::as_ref).flatten();
                if let Some(expr) = expr {
                    write!(
                        f,
                        "{}",
                        PrettyPrinterRef::new(
                            self.config,
                            self.res,
                            self.types,
                            self.strings,
                            *expr
                        )
                    )?;
                }
                write!(f, "}}")
            }
            Kind::Index => todo!(),
            Kind::Dotted => todo!(),
            Kind::Array => todo!(),
            Kind::With => {
                write!(
                    f,
                    "{}",
                    &node
                        .children
                        .first()
                        .and_then(|node| {
                            node.map(|node| {
                                PrettyPrinterRef::new(
                                    self.config,
                                    self.res,
                                    self.types,
                                    self.strings,
                                    node,
                                )
                            })
                        })
                        .unwrap(),
                )?;
                for argument in &node.children {
                    write!(f, " with! ")?;
                    let argument = PrettyPrinterRef::new(
                        self.config,
                        self.res,
                        self.types,
                        self.strings,
                        argument.unwrap(),
                    );
                    write!(f, "{}", argument)?;
                }
                Ok(())
            }
        }
    }
}
