use std::fmt::{self, Display};
use std::iter::Peekable;
use std::str;

use failure::{Fail, Fallible};

use crate::assets::{Handle, Resources};

mod private {
    pub trait Seal {}

    impl Seal for char {}
}

pub trait CharExt: private::Seal {
    fn is_ident_begin(self) -> bool;
    fn is_ident_cont(self) -> bool;
}

impl CharExt for char {
    fn is_ident_begin(self) -> bool {
        self.is_alphabetic()
            || self.is_ascii_punctuation()
                && !matches!(
                    self,
                    '"' | '#'
                        | '\''
                        | '('
                        | ')'
                        | ','
                        | '.'
                        | ':'
                        | ';'
                        | '='
                        | '['
                        | ']'
                        | '{'
                        | '}'
                )
    }

    fn is_ident_cont(self) -> bool {
        self.is_alphanumeric()
            || self.is_ascii_punctuation()
                && !matches!(
                    self,
                    '"' | '#'
                        | '\''
                        | '('
                        | ')'
                        | ','
                        | '.'
                        | ':'
                        | ';'
                        | '='
                        | '['
                        | ']'
                        | '{'
                        | '}'
                )
    }
}

#[derive(Debug, Fail)]
#[fail(display = "lexer error at: {}", location)]
pub struct LexerError {
    location: Location,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Location {
    pub filename: String,
    pub line: usize,
    pub column: usize,
}

impl Display for Location {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}:{}", self.filename, self.line, self.column)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Side {
    Left,
    Right,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenKind {
    Integer,
    Real,
    Identifier,
    String,
    Semicolon,
    Comma,
    Colon,
    Equals,
    EqualsArrow,
    Dot,
    SingleQuote,
    Paren(Side),
    Bracket(Side),
    Curly(Side),
}

impl TokenKind {
    pub fn from_str(lit: &str) -> Self {
        match lit {
            ";" => Self::Semicolon,
            "," => Self::Comma,
            ":" => Self::Colon,
            "=" => Self::Equals,
            "=>" => Self::EqualsArrow,
            "." => Self::Dot,
            "'" => Self::SingleQuote,
            "(" => Self::Paren(Side::Left),
            ")" => Self::Paren(Side::Right),
            "[" => Self::Bracket(Side::Left),
            "]" => Self::Bracket(Side::Right),
            "{" => Self::Curly(Side::Left),
            "}" => Self::Curly(Side::Right),
            _ => panic!("{:?} is not a literal token", lit),
        }
    }
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let symbol = match self {
            Self::Integer => "<int>",
            Self::Real => "<real>",
            Self::Identifier => "<ident>",
            Self::String => "<string>",
            Self::Semicolon => ";",
            Self::Comma => ",",
            Self::Colon => ":",
            Self::Equals => "=",
            Self::EqualsArrow => "=>",
            Self::Dot => ".",
            Self::SingleQuote => "'",
            Self::Paren(Side::Left) => "(",
            Self::Paren(Side::Right) => ")",
            Self::Bracket(Side::Left) => "[",
            Self::Bracket(Side::Right) => "]",
            Self::Curly(Side::Left) => "{",
            Self::Curly(Side::Right) => "}",
        };
        write!(f, "{}", symbol)
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TokenValue {
    None,
    Integer(u64),
    Real(f64),
    Identifier(Handle<String>),
    String(Handle<String>),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Token {
    pub location: Handle<Location>,
    pub kind: TokenKind,
    pub value: TokenValue,
}

#[derive(Clone)]
pub struct LexerData<'a> {
    src: Peekable<str::Chars<'a>>,
    filename: &'a str,
    line: usize,
    column: usize,
}

pub struct Lexer<'a, 'res> {
    pub(crate) res: Resources<(&'res mut String, &'res mut Location)>,
    pub(crate) data: LexerData<'a>,
}

impl<'a, 'res> Lexer<'a, 'res> {
    pub fn new(
        filename: &'a str,
        src: &'a str,
        res: Resources<(&'res mut String, &'res mut Location)>,
    ) -> Self {
        Self {
            res,
            data: LexerData {
                src: src.chars().peekable(),
                filename,
                line: 0,
                column: 0,
            },
        }
    }
}

impl<'a, 'res> Iterator for Lexer<'a, 'res> {
    type Item = Fallible<Token>;

    fn next(&mut self) -> Option<Self::Item> {
        let location = Location {
            line: self.data.line,
            column: self.data.column,
            filename: self.data.filename.to_string(),
        };
        let handle = Handle::from_hash(&format!("{}", location));
        self.res.insert(handle, location.clone());
        while let Some(&ch) = self.data.src.peek() {
            if ch.is_whitespace() {
                match ch {
                    '\n' => {
                        self.data.line += 1;
                        self.data.column = 0;
                    }
                    _ => self.data.column += 1,
                }
                self.data.src.next();
            } else {
                break;
            }
        }
        self.data.column += 1;
        match self.data.src.next()? {
            ';' => Some(Ok(Token {
                location: handle,
                kind: TokenKind::Semicolon,
                value: TokenValue::None,
            })),
            ',' => Some(Ok(Token {
                location: handle,
                kind: TokenKind::Comma,
                value: TokenValue::None,
            })),
            ':' => Some(Ok(Token {
                location: handle,
                kind: TokenKind::Colon,
                value: TokenValue::None,
            })),
            '=' => match self.data.src.peek() {
                Some(&'>') => {
                    self.data.column += 1;
                    self.data.src.next();
                    Some(Ok(Token {
                        location: handle,
                        kind: TokenKind::EqualsArrow,
                        value: TokenValue::None,
                    }))
                }
                _ => Some(Ok(Token {
                    location: handle,
                    kind: TokenKind::Equals,
                    value: TokenValue::None,
                })),
            },
            '.' => Some(Ok(Token {
                location: handle,
                kind: TokenKind::Dot,
                value: TokenValue::None,
            })),
            '\'' => Some(Ok(Token {
                location: handle,
                kind: TokenKind::SingleQuote,
                value: TokenValue::None,
            })),
            '(' => Some(Ok(Token {
                location: handle,
                kind: TokenKind::Paren(Side::Left),
                value: TokenValue::None,
            })),
            ')' => Some(Ok(Token {
                location: handle,
                kind: TokenKind::Paren(Side::Right),
                value: TokenValue::None,
            })),
            '[' => Some(Ok(Token {
                location: handle,
                kind: TokenKind::Bracket(Side::Left),
                value: TokenValue::None,
            })),
            ']' => Some(Ok(Token {
                location: handle,
                kind: TokenKind::Bracket(Side::Right),
                value: TokenValue::None,
            })),
            '{' => Some(Ok(Token {
                location: handle,
                kind: TokenKind::Curly(Side::Left),
                value: TokenValue::None,
            })),
            '}' => Some(Ok(Token {
                location: handle,
                kind: TokenKind::Curly(Side::Right),
                value: TokenValue::None,
            })),
            // TODO: real numbers
            first @ '0'..='9' => {
                let mut int = first.to_digit(10).unwrap() as u64;
                while let Some(&ch) = self.data.src.peek() {
                    match ch {
                        next @ '0'..='9' => {
                            self.data.column += 1;
                            self.data.src.next();
                            int *= 10;
                            int += next.to_digit(10).unwrap() as u64;
                        }
                        x if x.is_whitespace() => break,
                        '"' | '#' | ';' | ',' | ':' | '=' | '.' | '\'' | '(' | ')' | '[' | ']'
                        | '{' | '}' => break,
                        _ => return Some(Err(From::from(LexerError { location }))),
                    }
                }
                Some(Ok(Token {
                    location: handle,
                    kind: TokenKind::Integer,
                    value: TokenValue::Integer(int),
                }))
            }
            first if first.is_ident_begin() => {
                let mut ident = String::new();
                ident.push(first);
                while let Some(&ch) = self.data.src.peek() {
                    match ch {
                        next if next.is_ident_cont() => {
                            self.data.column += 1;
                            self.data.src.next();
                            ident.push(next);
                        }
                        x if x.is_whitespace() => break,
                        '"' | '#' | ';' | ',' | ':' | '=' | '.' | '\'' | '(' | ')' | '[' | ']'
                        | '{' | '}' => break,
                        _ => return Some(Err(From::from(LexerError { location }))),
                    }
                }
                let id_handle = Handle::from_hash(&ident);
                self.res.insert(id_handle, ident);
                Some(Ok(Token {
                    location: handle,
                    kind: TokenKind::Identifier,
                    value: TokenValue::Identifier(id_handle),
                }))
            }
            _ => Some(Err(From::from(LexerError { location }))),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::assets::*;
    use crate::ast::Node;
    use super::*;
    
    #[test]
    fn lex() {
        let world = World::new();
        world.init_asset::<Node>();
        world.init_asset::<String>();
        world.init_asset::<Location>();

        for elem in Lexer::new("lex", "", world.resources::<(&mut String, &mut Location)>()) {
            assert!(elem.is_ok());
        }
        
        for elem in Lexer::new("lex", "5", world.resources::<(&mut String, &mut Location)>()) {
            assert!(elem.is_ok());
        }
        
        for elem in Lexer::new("lex", "x := f 5, a, a => a + 1", world.resources::<(&mut String, &mut Location)>()) {
            assert!(elem.is_ok());
        }
    }
}
