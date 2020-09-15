use std::iter;

use either::Either;
use failure::{Fail, Fallible};
use peekmore_asref::PeekMoreIterator;
use smallvec::SmallVec;

use crate::assets::{Handle, Resources};
use crate::ast::{Kind, Node, NodeId};
use crate::lexer::{Lexer, Location, Side, TokenKind, TokenValue};
use crate::values::Payload;

pub mod grammar;

#[derive(Debug, Fail)]
#[fail(display = "unexpected end of file")]
pub struct UnexpectedEof;

#[derive(Debug, Fail)]
#[fail(display = "unexpected token: `{}` at {}", _0, _1)]
pub struct UnexpectedToken(TokenKind, Location);

pub struct State<'a, 'res> {
    pub(self) lexer: PeekMoreIterator<Lexer<'a, 'res>>,
    pub(self) nodes: Resources<&'res mut Node>,
}

#[inline]
pub fn repeat<T>(
    f: impl Fn(&mut State) -> Fallible<T>,
) -> impl Fn(&mut State) -> Fallible<SmallVec<[T; 4]>> {
    move |state| {
        let mut array = SmallVec::new();
        loop {
            let lexer = state.lexer.as_ref().data.clone();
            match f(state) {
                Ok(elem) => array.push(elem),
                Err(_) => {
                    state.lexer.as_mut().data = lexer;
                    return Ok(array)
                }
            }
        }
    }
}

#[inline]
pub fn optional<T>(
    f: impl Fn(&mut State) -> Fallible<T>,
) -> impl Fn(&mut State) -> Fallible<Option<T>> {
    move |state| {
        let lexer = state.lexer.as_ref().data.clone();
        Ok(f(state).map_err(|_| {
            state.lexer.as_mut().data = lexer;
        }).ok())
    }
}

#[inline]
pub fn and<T, U>(
    a: impl Fn(&mut State) -> Fallible<T>,
    b: impl Fn(&mut State) -> Fallible<U>,
) -> impl Fn(&mut State) -> Fallible<(T, U)> {
    move |state| a(state).and_then(|a| b(state).map(|b| (a, b)))
}

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

#[inline]
pub fn or<T, U>(
    a: impl Fn(&mut State) -> Fallible<T>,
    b: impl Fn(&mut State) -> Fallible<U>,
) -> impl Fn(&mut State) -> Fallible<Either<T, U>> {
    move |state| {
        let lexer = state.lexer.as_ref().data.clone();
        a(state)
            .map(Either::Left)
            .or_else(|_| {
                state.lexer.as_mut().data = lexer;
                b(state).map(Either::Right)
            })
    }
}

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
        let lexer = state.lexer.as_ref().data.clone();
        a(state)
            .or_else(|_| {
                state.lexer.as_mut().data = lexer.clone();
                b(state)
            })
            .or_else(|_| {
                state.lexer.as_mut().data = lexer.clone();
                c(state)
            })
            .or_else(|_| {
                state.lexer.as_mut().data = lexer.clone();
                d(state)
            })
            .or_else(|_| {
                state.lexer.as_mut().data = lexer.clone();
                e(state)
            })
            .or_else(|_| {
                state.lexer.as_mut().data = lexer.clone();
                f(state)
            })
            .or_else(|_| {
                state.lexer.as_mut().data = lexer.clone();
                g(state)
            })
    }
}

#[inline]
pub fn grouped<T>(
    sep: impl Fn(Side) -> TokenKind,
    f: impl Fn(&mut State) -> Fallible<T>,
) -> impl Fn(&mut State) -> Fallible<T> {
    move |state| {
        and3(literal(sep(Side::Left)), &f, literal(sep(Side::Right)))(state).map(|(_, t, _)| t)
    }
}

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
                    Err(From::from(UnexpectedToken(tok.kind, state.lexer.as_ref().res.get::<Location>(tok.location).unwrap().as_ref().clone())))
                }
            })
    }
}

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
                    let mut node = Node::new(Kind::Nil, iter::empty());
                    node.payload = Some(payload);
                    let id = NodeId::new();
                    state.nodes.insert(id, node);
                    Ok(id)
                } else {
                    Err(From::from(UnexpectedToken(tok.kind, state.lexer.as_ref().res.get::<Location>(tok.location).unwrap().as_ref().clone())))
                }
            })
    }
}
