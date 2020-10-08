//! Lexical analysis for ampersand.
use std::fmt::{self, Display};
use std::iter::Peekable;
use std::str;

use failure::{Error, Fail, Fallible};

use crate::assets::{Handle, Resources};

mod private {
    pub trait Seal {}

    impl Seal for char {}
}

/// An extension trait for characters.
pub trait CharExt: private::Seal {
    /// A predicate for checking if the character may be the beginning of an identifier.
    ///
    /// Returns true if the character contains the Alphabetic Unicode property, or is an ASCII punctuation mark, but not one of the following characters:
    /// '$', '"', '#', '\'', '(', ')', ',', '.', ':', ';', '=', '[', ']', '{', '}'
    fn is_ident_begin(self) -> bool;

    /// A predicate for checking if the character may be a character of an identifier, but not the first.
    ///
    /// Returns true if the character contains the Alphabetic Unicode property, Numeric Unicode property, or is an ASCII punctuation mark, but not one of the following characters:
    /// '$', '"', '#', '\'', '(', ')', ',', '.', ':', ';', '=', '[', ']', '{', '}'
    fn is_ident_cont(self) -> bool;
}

impl CharExt for char {
    fn is_ident_begin(self) -> bool {
        self.is_alphabetic()
            || self.is_ascii_punctuation()
                && !matches!(
                    self,
                    '$' | '&'
                        | '@'
                        | '"'
                        | '#'
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
                    '$' | '&'
                        | '@'
                        | '"'
                        | '#'
                        | '\''
                        | '('
                        | ')'
                        | ','
                        | '.'
                        | ':'
                        | ';'
                        | '['
                        | ']'
                        | '{'
                        | '}'
                )
    }
}

/// An error type for signaling an invalid character, or an invalid sequence of characters.
///
/// Valid characters are ASCII punctuation marks, all whitespaces, characters containing the Alphabetic or Numeric Unicode property.
#[derive(Debug, Fail)]
#[fail(display = "lexer error at: {}", location)]
pub struct LexerError {
    location: Location,
}

/// The source code location mapping.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Location {
    /// The displayable name of the file in which this location is.
    pub filename: String,
    /// The line number at which this location is located.
    ///
    /// Starts from 0.
    pub line: usize,
    /// The column number at which this location is located.
    ///
    /// Starts from 0.
    pub column: usize,
}

impl Display for Location {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}:{}", self.filename, self.line, self.column)
    }
}

/// The side of a grouping token.
///
/// Grouping tokens are: '(', ')', '[', ']', '{', '}'.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Side {
    Left,
    Right,
}

/// The kind of this token.
///
/// Specifies what this token is.
#[allow(missing_docs)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenKind {
    True,
    False,
    Struct,
    Enum,
    Union,
    Tagged,
    Class,
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
    Dollar,
    Ampersand,
    At,
    SingleQuote,
    With,
    Paren(Side),
    Bracket(Side),
    Curly(Side),
}

impl TokenKind {
    /// Constructs a token kind from a string. Valid only for punctuation marks.
    pub fn from_str(lit: &str) -> Self {
        match lit {
            ";" => Self::Semicolon,
            "," => Self::Comma,
            ":" => Self::Colon,
            "=" => Self::Equals,
            "=>" => Self::EqualsArrow,
            "." => Self::Dot,
            "'" => Self::SingleQuote,
            "$" => Self::Dollar,
            "&" => Self::Ampersand,
            "@" => Self::Curly(Side::Right),
            "(" => Self::Paren(Side::Left),
            ")" => Self::Paren(Side::Right),
            "[" => Self::Bracket(Side::Left),
            "]" => Self::Bracket(Side::Right),
            "{" => Self::Curly(Side::Left),
            "}" => Self::Curly(Side::Right),
            "with!" => Self::With,
            "true" => Self::True,
            "false" => Self::False,
            "struct" => Self::Struct,
            "enum" => Self::Enum,
            "union" => Self::Union,
            "tagged" => Self::Tagged,
            "class" => Self::Class,
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
            Self::Dollar => "$",
            Self::Ampersand => "&",
            Self::At => "@",
            Self::With => "with!",
            Self::True => "true",
            Self::False => "false",
            Self::Struct => "struct",
            Self::Enum => "enum",
            Self::Union => "union",
            Self::Tagged => "tagged",
            Self::Class => "class",
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

/// The value of a token.
///
/// Represents what this token will equate to during parsing.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TokenValue {
    /// No value.
    ///
    /// This is chosen for all punctuation marks.
    None,
    /// A boolean 1-bit value.
    ///
    /// True, or False.
    Bool(bool),
    /// An unsigned integer.
    ///
    /// Negative numbers are not single tokens, they are expressions.
    Integer(u64),
    /// A non-negative real number.
    ///
    /// Negative numbers are not single tokens, they are expressions.
    Real(f64),
    /// Any unicode identifier.
    Identifier(Handle<String>),
    /// Any unicode "-delimited string.
    String(Handle<String>),
}

/// The actual token structure.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Token {
    /// Handle to a source code location.
    pub location: Handle<Location>,
    /// Kind of this token.
    pub kind: TokenKind,
    /// The payload of this token, if any.
    pub value: TokenValue,
}

/// Cloneable lexer data. Specifies at which point in the source file the lexer is currently at.
#[derive(Clone)]
pub struct LexerData<'a> {
    next: Option<Token>,
    src: Peekable<str::Chars<'a>>,
    filename: &'a str,
    line: usize,
    column: usize,
}

/// Non-cloneable lexer.
pub struct Lexer<'a, 'res> {
    pub(crate) res: Resources<(&'res mut String, &'res mut Location)>,
    pub(crate) data: LexerData<'a>,
    pub(crate) errors: Vec<Error>,
}

impl<'a, 'res> Lexer<'a, 'res> {
    /// Creates a new lexer with a filename, source code and some resources.
    pub fn new(
        filename: &'a str,
        src: &'a str,
        res: Resources<(&'res mut String, &'res mut Location)>,
    ) -> Self {
        Self {
            res,
            data: LexerData {
                next: None,
                src: src.chars().peekable(),
                filename,
                line: 0,
                column: 0,
            },
            errors: Vec::new(),
        }
    }

    pub fn next_err(&mut self) -> Option<Error> {
        if self.errors.is_empty() {
            None
        } else {
            Some(self.errors.remove(0))
        }
    }

    pub fn peek(&mut self) -> Option<Token> {
        match self.data.next {
            None => {
                self.data.next = match self.next() {
                    Some(Ok(next)) => Some(next),
                    Some(Err(err)) => {
                        self.errors.push(err);
                        None
                    }
                    None => None,
                };
                self.data.next
            }
            next @ Some(_) => next,
        }
    }
}

impl<'a, 'res> Iterator for Lexer<'a, 'res> {
    type Item = Fallible<Token>;

    fn next(&mut self) -> Option<Self::Item> {
        if !self.errors.is_empty() {
            return Some(Err(self.errors.remove(0)));
        }

        self.data.next.take().map(Ok).or_else(|| {
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
                    Some(&'=') => {
                        let mut ident = String::new();
                        ident.push('=');
                        while let Some(&ch) = self.data.src.peek() {
                            match ch {
                                next if next.is_ident_cont() => {
                                    self.data.column += 1;
                                    self.data.src.next();
                                    ident.push(next);
                                }
                                x if x.is_whitespace() => break,
                                '"' | '#' | ';' | ',' | ':' | '.' | '\'' | '(' | ')' | '['
                                | ']' | '{' | '}' => break,
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
                '$' => Some(Ok(Token {
                    location: handle,
                    kind: TokenKind::Dollar,
                    value: TokenValue::None,
                })),
                '&' => Some(Ok(Token {
                    location: handle,
                    kind: TokenKind::Ampersand,
                    value: TokenValue::None,
                })),
                '@' => Some(Ok(Token {
                    location: handle,
                    kind: TokenKind::At,
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
                '"' => {
                    enum State {
                        Normal,
                        Backslash,
                    }

                    let mut string = String::new();
                    let mut state = State::Normal;
                    while let Some(ch) = self.data.src.next() {
                        match state {
                            State::Normal => match ch {
                                '"' => break,
                                '\\' => {
                                    self.data.column += 1;
                                    state = State::Backslash;
                                }
                                '\n' => {
                                    self.data.line += 1;
                                    self.data.column = 0;
                                }
                                next => {
                                    self.data.column += 1;
                                    string.push(next);
                                }
                            },
                            State::Backslash => match ch {
                                '\\' => {
                                    self.data.column += 1;
                                    state = State::Normal;
                                    string.push('\\');
                                }
                                'e' => {
                                    self.data.column += 1;
                                    state = State::Normal;
                                    string.push('\x1b');
                                }
                                'n' => {
                                    self.data.column += 1;
                                    state = State::Normal;
                                    string.push('\n');
                                }
                                't' => {
                                    self.data.column += 1;
                                    state = State::Normal;
                                    string.push('\t');
                                }
                                'r' => {
                                    self.data.column += 1;
                                    state = State::Normal;
                                    string.push('\r');
                                }
                                _ => return Some(Err(From::from(LexerError { location }))),
                            },
                        }
                    }
                    let str_handle = Handle::from_hash(&string);
                    self.res.insert(str_handle, string);
                    Some(Ok(Token {
                        location: handle,
                        kind: TokenKind::String,
                        value: TokenValue::String(str_handle),
                    }))
                }
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
                            '@' | '"' | '#' | ';' | ',' | ':' | '=' | '.' | '\'' | '(' | ')'
                            | '[' | ']' | '{' | '}' => break,
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
                            '@' | '"' | '#' | ';' | ',' | ':' | '.' | '\'' | '(' | ')' | '['
                            | ']' | '{' | '}' => break,
                            _ => return Some(Err(From::from(LexerError { location }))),
                        }
                    }
                    if ident == "with!" {
                        Some(Ok(Token {
                            location: handle,
                            kind: TokenKind::With,
                            value: TokenValue::None,
                        }))
                    } else if ident == "true" {
                        Some(Ok(Token {
                            location: handle,
                            kind: TokenKind::True,
                            value: TokenValue::Bool(true),
                        }))
                    } else if ident == "false" {
                        Some(Ok(Token {
                            location: handle,
                            kind: TokenKind::True,
                            value: TokenValue::Bool(false),
                        }))
                    } else if ident == "struct" {
                        Some(Ok(Token {
                            location: handle,
                            kind: TokenKind::Struct,
                            value: TokenValue::None,
                        }))
                    } else if ident == "enum" {
                        Some(Ok(Token {
                            location: handle,
                            kind: TokenKind::Enum,
                            value: TokenValue::None,
                        }))
                    } else if ident == "union" {
                        Some(Ok(Token {
                            location: handle,
                            kind: TokenKind::Union,
                            value: TokenValue::None,
                        }))
                    } else if ident == "tagged" {
                        Some(Ok(Token {
                            location: handle,
                            kind: TokenKind::Tagged,
                            value: TokenValue::None,
                        }))
                    } else if ident == "class" {
                        Some(Ok(Token {
                            location: handle,
                            kind: TokenKind::Class,
                            value: TokenValue::None,
                        }))
                    } else {
                        let id_handle = Handle::from_hash(&ident);
                        self.res.insert(id_handle, ident);
                        Some(Ok(Token {
                            location: handle,
                            kind: TokenKind::Identifier,
                            value: TokenValue::Identifier(id_handle),
                        }))
                    }
                }
                _ => Some(Err(From::from(LexerError { location }))),
            }
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::assets::*;
    use crate::ast::Node;

    #[test]
    fn lex() {
        let world = World::new();
        world.init_asset::<Node>();
        world.init_asset::<String>();
        world.init_asset::<Location>();

        for elem in Lexer::new("lex", "", world.resources::<(&mut String, &mut Location)>()) {
            assert!(elem.is_ok());
        }

        for elem in Lexer::new(
            "lex",
            "5",
            world.resources::<(&mut String, &mut Location)>(),
        ) {
            assert!(elem.is_ok());
        }

        for elem in Lexer::new(
            "lex",
            "x := f 5, a, a => a + 1",
            world.resources::<(&mut String, &mut Location)>(),
        ) {
            assert!(elem.is_ok());
        }
    }
}
