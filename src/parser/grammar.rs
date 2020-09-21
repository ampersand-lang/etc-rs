//! Contains the ampersand recursive-descent parser.
use std::iter;

use failure::{Error, Fallible};

use crate::error::MultiError;

use crate::assets::Handle;
use crate::ast::{Kind, Node, NodeId};
use crate::lexer::{Location, Side, TokenKind};

use super::*;

/// Parses a program from a `State`, or returns an error, possibly a multi-error.
pub fn parse(state: &mut State) -> Fallible<NodeId> {
    fn inner(state: &mut State, errors: &mut Vec<Error>) -> Option<NodeId> {
        // root never returns an error
        let result = root(state).unwrap();
        if let Some(tok) = state.lexer.peek() {
            let kind = tok.kind;
            let location = tok.location;
            errors.push(From::from(UnexpectedToken(
                kind,
                state
                    .lexer
                    .res
                    .get::<Location>(location)
                    .unwrap()
                    .as_ref()
                    .clone(),
            )));

            let mut depth = 0_usize;
            let mut tok = match state.lexer.next().unwrap() {
                Ok(tok) => tok,
                Err(err) => {
                    errors.push(err);
                    return None;
                }
            };
            while depth != 0 || tok.kind != TokenKind::Semicolon {
                match tok.kind {
                    TokenKind::Paren(Side::Left) => depth += 1,
                    TokenKind::Bracket(Side::Left) => depth += 1,
                    TokenKind::Curly(Side::Left) => depth += 1,
                    TokenKind::Paren(Side::Right) => depth -= 1,
                    TokenKind::Bracket(Side::Right) => depth -= 1,
                    TokenKind::Curly(Side::Right) => depth -= 1,
                    _ => {}
                }
                if let Some(t) = state.lexer.next() {
                    tok = match t {
                        Ok(tok) => tok,
                        Err(err) => {
                            errors.push(err);
                            return None;
                        }
                    }
                } else {
                    return None;
                }
            }
            inner(state, errors)
        } else if let Some(err) = state.lexer.next_err() {
            errors.push(err);
            None
        } else {
            Some(result)
        }
    }

    let mut errors = Vec::new();
    let result = inner(state, &mut errors);
    if errors.is_empty() {
        Ok(result.unwrap())
    } else {
        Err(From::from(MultiError::from(errors)))
    }
}

fn root(state: &mut State) -> Fallible<NodeId> {
    let location = state.location().unwrap_or_else(Handle::nil);
    let (body, rest) = and(repeat(stmt), optional(expr))(state)?;
    let body = body.into_iter().map(Some).chain(iter::once(rest));
    let node = Node::new(Kind::Block, location, body);
    let handle = node.id();
    state.nodes.insert(handle, node);
    Ok(handle)
}

fn stmt(state: &mut State) -> Fallible<NodeId> {
    let (expr, _) = and(expr, literal(TokenKind::Semicolon))(state)?;
    Ok(expr)
}

fn expr(state: &mut State) -> Fallible<NodeId> {
    Ok(or(binding, declaration)(state)?.into_inner())
}

fn binding(state: &mut State) -> Fallible<NodeId> {
    let location = state.location().unwrap_or_else(Handle::nil);
    let (name, _, typ, _, value) = and5(
        alternative,
        literal(TokenKind::Colon),
        optional(binary),
        literal(TokenKind::Equals),
        binary,
    )(state)?;
    let ident = state.nodes.get(name).unwrap();
    let ident = match ident.payload.unwrap() {
        Payload::Identifier(ident) => ident,
        _ => panic!("patterns are not implemented yet"),
    };
    let ident = state.lexer.res.get(ident).unwrap();
    let node = Node::with_name(
        Kind::Binding,
        location,
        iter::once(Some(name))
            .chain(iter::once(typ))
            .chain(iter::once(Some(value))),
        ident.as_bytes(),
    );
    let handle = node.id();
    state.nodes.insert(handle, node);
    Ok(handle)
}

fn declaration(state: &mut State) -> Fallible<NodeId> {
    Ok(or(
        |state| {
            let location = state.location().unwrap_or_else(Handle::nil);
            and3(alternative, literal(TokenKind::Colon), binary)(state).map(|(name, _, typ)| {
                let node = Node::new(
                    Kind::Declaration,
                    location,
                    iter::once(Some(name)).chain(iter::once(Some(typ))),
                );
                let handle = node.id();
                state.nodes.insert(handle, node);
                handle
            })
        },
        binary,
    )(state)?
    .into_inner())
}

fn binary(state: &mut State) -> Fallible<NodeId> {
    Ok(or(
        |state| {
            let location = state.location().unwrap_or_else(Handle::nil);
            and3(index, index, application)(state).map(|(a, op, b)| {
                let node = Node::new(
                    Kind::Application,
                    location,
                    iter::once(Some(op))
                        .chain(iter::once(Some(a)))
                        .chain(iter::once(Some(b))),
                );
                let handle = node.id();
                state.nodes.insert(handle, node);
                handle
            })
        },
        application,
    )(state)?
    .into_inner())
}

fn application(state: &mut State) -> Fallible<NodeId> {
    Ok(or(
        |state| {
            let location = state.location().unwrap_or_else(Handle::nil);
            and3(
                alternative,
                argument,
                repeat(and(literal(TokenKind::Comma), argument)),
            )(state)
            .map(|(func, first, rest)| {
                let node = Node::new(
                    Kind::Application,
                    location,
                    iter::once(Some(func))
                        .chain(iter::once(Some(first)))
                        .chain(rest.into_iter().map(|(_, arg)| Some(arg))),
                );
                let handle = node.id();
                state.nodes.insert(handle, node);
                handle
            })
        },
        argument,
    )(state)?
    .into_inner())
}

fn argument(state: &mut State) -> Fallible<NodeId> {
    Ok(or(
        |state| {
            let location = state.location().unwrap_or_else(Handle::nil);
            and3(alternative, literal(TokenKind::Colon), function)(state).map(|(name, _, value)| {
                let node = Node::new(
                    Kind::Argument,
                    location,
                    iter::once(Some(name)).chain(iter::once(Some(value))),
                );
                let handle = node.id();
                state.nodes.insert(handle, node);
                handle
            })
        },
        function,
    )(state)?
    .into_inner())
}

fn function(state: &mut State) -> Fallible<NodeId> {
    Ok(or(
        |state| {
            let location = state.location().unwrap_or_else(Handle::nil);
            and3(index, literal(TokenKind::EqualsArrow), binary)(state).map(|(args, _, body)| {
                let node = Node::new(
                    Kind::Function,
                    location,
                    iter::once(Some(args)).chain(iter::once(Some(body))),
                );
                let handle = node.id();
                state.nodes.insert(handle, node);
                handle
            })
        },
        index,
    )(state)?
    .into_inner())
}

fn index(state: &mut State) -> Fallible<NodeId> {
    let location = state.location().unwrap_or_else(Handle::nil);
    and(repeat(and(dotted, literal(TokenKind::SingleQuote))), dotted)(state).map(|(a, i)| {
        let mut last = None;
        if !a.is_empty() {
            for (&(array, _), &(index, _)) in a.iter().zip(&a[1..]) {
                let node = Node::new(
                    Kind::Index,
                    location,
                    iter::once(Some(last.unwrap_or(array))).chain(iter::once(Some(index))),
                );
                let handle = node.id();
                state.nodes.insert(handle, node);
                last = Some(handle);
            }
        }
        last.map(|last| {
            let node = Node::new(
                Kind::Index,
                location,
                iter::once(Some(last)).chain(iter::once(Some(i))),
            );
            let handle = node.id();
            state.nodes.insert(handle, node);
            handle
        })
        .unwrap_or(i)
    })
}

fn dotted(state: &mut State) -> Fallible<NodeId> {
    let location = state.location().unwrap_or_else(Handle::nil);
    and(
        repeat(and(alternative, literal(TokenKind::Dot))),
        alternative,
    )(state)
    .map(|(left, field)| {
        let mut last = None;
        if !left.is_empty() {
            for (&(left, _), &(field, _)) in left.iter().zip(&left[1..]) {
                let node = Node::new(
                    Kind::Dotted,
                    location,
                    iter::once(Some(last.unwrap_or(left))).chain(iter::once(Some(field))),
                );
                let handle = node.id();
                state.nodes.insert(handle, node);
                last = Some(handle);
            }
        }
        last.map(|last| {
            let node = Node::new(
                Kind::Dotted,
                location,
                iter::once(Some(last)).chain(iter::once(Some(field))),
            );
            let handle = node.id();
            state.nodes.insert(handle, node);
            handle
        })
        .unwrap_or(field)
    })
}

fn alternative(state: &mut State) -> Fallible<NodeId> {
    and(optional(literal(TokenKind::Dollar)), atomic)(state).map(|(alt, handle)| {
        state.nodes.get_mut::<Node>(handle).unwrap().alternative = alt.is_some();
        handle
    })
}

fn atomic(state: &mut State) -> Fallible<NodeId> {
    or7(
        move |state| {
            let location = state.location().unwrap_or_else(Handle::nil);
            Ok(grouped(
                TokenKind::Paren,
                optional(and(
                    declaration,
                    repeat(and(literal(TokenKind::Semicolon), declaration)),
                )),
            )(state)?
            .map(|(first, rest)| {
                let node = Node::new(
                    Kind::Tuple,
                    location,
                    iter::once(Some(first)).chain(rest.into_iter().map(|(_, x)| Some(x))),
                );
                let handle = node.id();
                state.nodes.insert(handle, node);
                handle
            })
            .unwrap_or_else(|| {
                let node = Node::new(Kind::Tuple, location, iter::empty());
                let handle = node.id();
                state.nodes.insert(handle, node);
                handle
            }))
        },
        move |state| {
            let location = state.location().unwrap_or_else(Handle::nil);
            Ok(grouped(
                TokenKind::Bracket,
                optional(and(
                    declaration,
                    repeat(and(literal(TokenKind::Semicolon), declaration)),
                )),
            )(state)?
            .map(|(first, rest)| {
                let node = Node::new(
                    Kind::Array,
                    location,
                    iter::once(Some(first)).chain(rest.into_iter().map(|(_, x)| Some(x))),
                );
                let handle = node.id();
                state.nodes.insert(handle, node);
                handle
            })
            .unwrap_or_else(|| {
                let node = Node::new(Kind::Array, location, iter::empty());
                let handle = node.id();
                state.nodes.insert(handle, node);
                handle
            }))
        },
        grouped(TokenKind::Curly, root),
        atom(TokenKind::Integer),
        atom(TokenKind::Real),
        atom(TokenKind::Identifier),
        atom(TokenKind::String),
    )(state)
}

#[cfg(test)]
mod tests {
    use crate::assets::*;
    use crate::ast::Node;
    use crate::lexer::*;
    use crate::parser::State;

    use super::*;

    #[test]
    fn parse_test() {
        let world = World::new();
        world.init_asset::<Node>();
        world.init_asset::<String>();
        world.init_asset::<Location>();

        parse(&mut State {
            lexer: Lexer::new(
                "parse",
                "",
                world.resources::<(&mut String, &mut Location)>(),
            ),
            nodes: world.resources::<&mut Node>(),
        })
        .unwrap();

        parse(&mut State {
            lexer: Lexer::new(
                "parse",
                "5",
                world.resources::<(&mut String, &mut Location)>(),
            ),
            nodes: world.resources::<&mut Node>(),
        })
        .unwrap();

        parse(&mut State {
            lexer: Lexer::new(
                "parse",
                "x := f 5, a, a => a + 1; x",
                world.resources::<(&mut String, &mut Location)>(),
            ),
            nodes: world.resources::<&mut Node>(),
        })
        .unwrap();
    }
}
