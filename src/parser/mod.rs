//! Includes helper primitives for defining recursive-descent parsers easily.
use std::iter;

use either::Either;
use failure::{Fail, Fallible};
use smallvec::SmallVec;

use crate::assets::{Handle, Resources};
use crate::ast::{Kind, Node, NodeId};
use crate::lexer::{Lexer, Location, Side, TokenKind, TokenValue};
use crate::values::Payload;

pub mod grammar;

/// Returned from parsers if a token was expected, but the lexer returned `None`.
#[derive(Debug, Fail)]
#[fail(display = "unexpected end of file")]
pub struct UnexpectedEof;

/// Returned from parsers if a token was expected, but a different token was returned from the lexer.
#[derive(Debug, Fail)]
#[fail(display = "unexpected token: `{}` at {}", _0, _1)]
pub struct UnexpectedToken(TokenKind, Location);

/// The current parsing state.
pub struct State<'a, 'res> {
    /// A n-peekable lexer wrapper.
    pub(crate) lexer: Lexer<'a, 'res>,
    /// Some resources.
    pub(crate) nodes: Resources<&'res mut Node>,
}

impl<'a, 'res> State<'a, 'res> {
    pub fn new(lexer: Lexer<'a, 'res>, nodes: Resources<&'res mut Node>) -> Self {
        Self { lexer, nodes }
    }

    pub fn location(&mut self) -> Option<Handle<Location>> {
        self.lexer.peek().map(|tok| tok.location)
    }
}

/// Repeat a parser for as long as it returns a valid result.
///
/// `{}` in EBNF.
#[inline]
pub fn repeat<T>(
    f: impl Fn(&mut State) -> Fallible<T>,
) -> impl Fn(&mut State) -> Fallible<SmallVec<[T; 4]>> {
    move |state| {
        let mut array = SmallVec::new();
        loop {
            let lexer = state.lexer.data.clone();
            match f(state) {
                Ok(elem) => array.push(elem),
                Err(_) => {
                    state.lexer.data = lexer;
                    return Ok(array);
                }
            }
        }
    }
}

/// Returns the `Some` variant for a parser if it returned a positive value, or `None` if it didn't parse.
///
/// `[]` in EBNF.
#[inline]
pub fn optional<T>(
    f: impl Fn(&mut State) -> Fallible<T>,
) -> impl Fn(&mut State) -> Fallible<Option<T>> {
    move |state| {
        let lexer = state.lexer.data.clone();
        Ok(f(state)
            .map_err(|_| {
                state.lexer.data = lexer;
            })
            .ok())
    }
}

/// A conjunction of two parsers.
///
/// `,` in EBNF.
#[inline]
pub fn and<T, U>(
    a: impl Fn(&mut State) -> Fallible<T>,
    b: impl Fn(&mut State) -> Fallible<U>,
) -> impl Fn(&mut State) -> Fallible<(T, U)> {
    move |state| a(state).and_then(|a| b(state).map(|b| (a, b)))
}

/// A conjunction of three parsers.
///
/// `,` in EBNF.
#[inline]
pub fn and3<T, U, V>(
    a: impl Fn(&mut State) -> Fallible<T>,
    b: impl Fn(&mut State) -> Fallible<U>,
    c: impl Fn(&mut State) -> Fallible<V>,
) -> impl Fn(&mut State) -> Fallible<(T, U, V)> {
    move |state| {
        a(state)
            .and_then(|a| b(state).map(|b| (a, b)))
            .and_then(|(a, b)| c(state).map(|c| (a, b, c)))
    }
}

/// A conjunction of four parsers.
///
/// `,` in EBNF.
#[inline]
pub fn and4<T, U, V, W>(
    a: impl Fn(&mut State) -> Fallible<T>,
    b: impl Fn(&mut State) -> Fallible<U>,
    c: impl Fn(&mut State) -> Fallible<V>,
    d: impl Fn(&mut State) -> Fallible<W>,
) -> impl Fn(&mut State) -> Fallible<(T, U, V, W)> {
    move |state| {
        a(state)
            .and_then(|a| b(state).map(|b| (a, b)))
            .and_then(|(a, b)| c(state).map(|c| (a, b, c)))
            .and_then(|(a, b, c)| d(state).map(|d| (a, b, c, d)))
    }
}

/// A conjunction of five parsers.
///
/// `,` in EBNF.
#[inline]
pub fn and5<T, U, V, W, X>(
    a: impl Fn(&mut State) -> Fallible<T>,
    b: impl Fn(&mut State) -> Fallible<U>,
    c: impl Fn(&mut State) -> Fallible<V>,
    d: impl Fn(&mut State) -> Fallible<W>,
    e: impl Fn(&mut State) -> Fallible<X>,
) -> impl Fn(&mut State) -> Fallible<(T, U, V, W, X)> {
    move |state| {
        a(state)
            .and_then(|a| b(state).map(|b| (a, b)))
            .and_then(|(a, b)| c(state).map(|c| (a, b, c)))
            .and_then(|(a, b, c)| d(state).map(|d| (a, b, c, d)))
            .and_then(|(a, b, c, d)| e(state).map(|e| (a, b, c, d, e)))
    }
}

/// A disjunction of two parsers, that may return different results.
///
/// `|` in EBNF.
#[inline]
pub fn or<T, U>(
    a: impl Fn(&mut State) -> Fallible<T>,
    b: impl Fn(&mut State) -> Fallible<U>,
) -> impl Fn(&mut State) -> Fallible<Either<T, U>> {
    move |state| {
        let lexer = state.lexer.data.clone();
        a(state).map(Either::Left).or_else(|_| {
            state.lexer.data = lexer;
            b(state).map(Either::Right)
        })
    }
}

/// A disjunction of three parsers, that may return different results.
///
/// `|` in EBNF.
#[inline]
pub fn or3<T, U, V>(
    a: impl Fn(&mut State) -> Fallible<T>,
    b: impl Fn(&mut State) -> Fallible<U>,
    c: impl Fn(&mut State) -> Fallible<V>,
) -> impl Fn(&mut State) -> Fallible<Either<Either<T, U>, V>> {
    move |state| {
        let lexer = state.lexer.data.clone();
        a(state)
            .map(Either::Left)
            .or_else(|_| {
                state.lexer.data = lexer.clone();
                b(state).map(Either::Right)
            })
            .map(Either::Left)
            .or_else(|_| {
                state.lexer.data = lexer;
                c(state).map(Either::Right)
            })
    }
}

/// A disjunction of seven parsers.
///
/// `|` in EBNF.
#[inline]
pub fn or7<T: Clone>(
    a: impl Fn(&mut State) -> Fallible<T>,
    b: impl Fn(&mut State) -> Fallible<T>,
    c: impl Fn(&mut State) -> Fallible<T>,
    d: impl Fn(&mut State) -> Fallible<T>,
    e: impl Fn(&mut State) -> Fallible<T>,
    f: impl Fn(&mut State) -> Fallible<T>,
    g: impl Fn(&mut State) -> Fallible<T>,
) -> impl Fn(&mut State) -> Fallible<T> {
    move |state| {
        let lexer = state.lexer.data.clone();
        a(state)
            .or_else(|_| {
                state.lexer.data = lexer.clone();
                b(state)
            })
            .or_else(|_| {
                state.lexer.data = lexer.clone();
                c(state)
            })
            .or_else(|_| {
                state.lexer.data = lexer.clone();
                d(state)
            })
            .or_else(|_| {
                state.lexer.data = lexer.clone();
                e(state)
            })
            .or_else(|_| {
                state.lexer.data = lexer.clone();
                f(state)
            })
            .or_else(|_| {
                state.lexer.data = lexer.clone();
                g(state)
            })
    }
}

/// A grouped tree.
///
/// Helper for `sep(Left) , ?any parser? , sep(Right)`.
#[inline]
pub fn grouped<T>(
    sep: impl Fn(Side) -> TokenKind,
    f: impl Fn(&mut State) -> Fallible<T>,
) -> impl Fn(&mut State) -> Fallible<T> {
    move |state| {
        and3(literal(sep(Side::Left)), &f, literal(sep(Side::Right)))(state).map(|(_, t, _)| t)
    }
}

/// A literal, like a punctuation mark.
#[inline]
pub fn literal(lit: TokenKind) -> impl Fn(&mut State) -> Fallible<()> {
    move |state| {
        state
            .lexer
            .next()
            .ok_or_else(|| From::from(UnexpectedEof))
            .and_then(|tok| {
                let tok = tok?;
                if tok.kind == lit {
                    Ok(())
                } else {
                    Err(From::from(UnexpectedToken(
                        tok.kind,
                        state
                            .lexer
                            .res
                            .get::<Location>(tok.location)
                            .unwrap()
                            .as_ref()
                            .clone(),
                    )))
                }
            })
    }
}

/// A literal with a value.
#[inline]
pub fn atom(lit: TokenKind) -> impl Fn(&mut State) -> Fallible<NodeId> {
    move |state| {
        state
            .lexer
            .next()
            .ok_or_else(|| From::from(UnexpectedEof))
            .and_then(|tok| {
                let tok = tok?;
                if tok.kind == lit {
                    let payload = match tok.value {
                        TokenValue::None => panic!("atom is not an atom"),
                        TokenValue::Integer(int) => Payload::Integer(int),
                        TokenValue::Real(real) => Payload::Float(real),
                        TokenValue::Identifier(ident) => Payload::Identifier(ident),
                        TokenValue::String(string) => Payload::String(string),
                    };
                    let mut node = Node::new(Kind::Nil, tok.location, iter::empty());
                    node.payload = Some(payload);
                    let handle = node.id();
                    state.nodes.insert(handle, node);
                    Ok(handle)
                } else {
                    Err(From::from(UnexpectedToken(
                        tok.kind,
                        state
                            .lexer
                            .res
                            .get::<Location>(tok.location)
                            .unwrap()
                            .as_ref()
                            .clone(),
                    )))
                }
            })
    }
}
