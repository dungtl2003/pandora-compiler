use core::fmt;
use std::fmt::{Display, Formatter};

use crate::{
    kw::{self, Keyword},
    span_encoding::{Span, DUMMY_SP},
    symbol::Symbol,
};

use super::ident::Ident;
use BinOpToken::*;
use TokenKind::*;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.kind)
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum TokenKind {
    /* Expression-operator symbols. */
    /// `=`
    Eq,
    /// `<`
    Lt,
    /// `<=`
    Le,
    /// `==`
    EqEq,
    /// `!=`
    Ne,
    /// `>=`
    Ge,
    /// `>`
    Gt,
    /// `&&`
    AndAnd,
    /// `||`
    OrOr,
    /// `!`
    Not,
    /// `~`
    Tilde,
    BinOp(BinOpToken),
    BinOpEq(BinOpToken),

    /* Structural symbols */
    /// `.`
    Dot,
    /// `,`
    Comma,
    /// `;`
    Semicolon,
    /// `:`
    Colon,
    /// `::`
    PathSep,
    /// `->`
    RArrow,
    /// `?`
    Question,
    /// An opening delimiter (e.g., `{`).
    OpenDelim(Delimiter),
    /// A closing delimiter (e.g., `}`).
    CloseDelim(Delimiter),

    /* Literals */
    Literal(Lit),

    Ident(Symbol, IdentIsRaw),

    /// A doc comment token.
    /// `Symbol` is the data of doc's comment excluding its "quotes" (`///`, `/**`, etc)
    DocComment(CommentKind, Option<DocStyle>, Symbol),

    /// End Of File.
    Eof,
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Eq => write!(f, "="),
            Lt => write!(f, "<"),
            Le => write!(f, "<="),
            EqEq => write!(f, "=="),
            Ne => write!(f, "!="),
            Ge => write!(f, ">="),
            Gt => write!(f, ">"),
            AndAnd => write!(f, "&&"),
            OrOr => write!(f, "||"),
            Not => write!(f, "!"),
            Tilde => write!(f, "~"),
            BinOp(op) => write!(f, "{}", op),
            BinOpEq(op) => write!(f, "{}=", op),
            Dot => write!(f, "."),
            Comma => write!(f, ","),
            Semicolon => write!(f, ";"),
            Colon => write!(f, ":"),
            PathSep => write!(f, "::"),
            RArrow => write!(f, "->"),
            Question => write!(f, "?"),
            OpenDelim(delim) => {
                let s = match delim {
                    Delimiter::Parenthesis => "(",
                    Delimiter::Brace => "{",
                    Delimiter::Bracket => "[",
                };
                write!(f, "{}", s)
            }
            CloseDelim(delim) => {
                let s = match delim {
                    Delimiter::Parenthesis => ")",
                    Delimiter::Brace => "}",
                    Delimiter::Bracket => "]",
                };
                write!(f, "{}", s)
            }
            Literal(lit) => write!(f, "{}", lit.to_ty_str()),
            Ident(_name, is_raw) => {
                if *is_raw == IdentIsRaw::Yes {
                    return write!(f, "raw identifier");
                } else {
                    return write!(f, "identifier");
                }
            }
            DocComment(_, _, _) => write!(f, "doc comment"),
            Eof => write!(f, "<EOF>"),
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum BinOpToken {
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Caret,
    And,
    Or,
    Shl,
    Shr,
}

impl Display for BinOpToken {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Plus => write!(f, "+"),
            Minus => write!(f, "-"),
            Star => write!(f, "*"),
            Slash => write!(f, "/"),
            Percent => write!(f, "%"),
            Caret => write!(f, "^"),
            And => write!(f, "&"),
            Or => write!(f, "|"),
            Shl => write!(f, "<<"),
            Shr => write!(f, ">>"),
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Delimiter {
    /// `( ... )`
    Parenthesis,
    /// `{ ... }`
    Brace,
    /// `[ ... ]`
    Bracket,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum CommentKind {
    Line,
    Block,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct Lit {
    pub kind: LitKind,
    pub symbol: Symbol,
}

impl Lit {
    pub fn to_ty_str(&self) -> &'static str {
        self.kind.to_ty_str()
    }
}

impl Display for Lit {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{:?}", self.kind)
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum LitKind {
    Bool,
    Char,
    Int,
    Float,
    Str,
    RawStr(u8), // raw string delimited by `n` hash symbols

    Err,
}

impl LitKind {
    pub fn to_ty_str(&self) -> &'static str {
        match self {
            LitKind::Bool => "bool",
            LitKind::Char => "char",
            LitKind::Int => "int",
            LitKind::Float => "float",
            LitKind::Str | LitKind::RawStr(_) => "str",
            LitKind::Err => unreachable!(),
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum IdentIsRaw {
    Yes,
    No,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum DocStyle {
    Inner,
    Outer,
}

impl Token {
    pub fn new(kind: TokenKind, span: Span) -> Token {
        Token { kind, span }
    }

    pub fn is_kind(&self, kind: TokenKind) -> bool {
        self.kind == kind
    }

    pub fn can_begin_operator(&self) -> bool {
        match self.kind {
            BinOp(_) | AndAnd | OrOr | Not | Tilde => true,
            _ => false,
        }
    }

    pub fn is_open_delim(&self, delim: Delimiter) -> bool {
        match self.kind {
            OpenDelim(d) => d == delim,
            _ => false,
        }
    }

    pub fn is_close_delim(&self, delim: Delimiter) -> bool {
        match self.kind {
            CloseDelim(d) => d == delim,
            _ => false,
        }
    }

    pub fn glue(&self, joint: &Token) -> Option<Token> {
        let kind = match self.kind {
            Eq => match joint.kind {
                Eq => EqEq,
                _ => return None,
            },
            Not => match joint.kind {
                Eq => Ne,
                _ => return None,
            },
            Lt => match joint.kind {
                Eq => Le,
                Lt => BinOp(Shl),
                Le => BinOpEq(Shl),
                _ => return None,
            },
            Gt => match joint.kind {
                Eq => Ge,
                Gt => BinOp(Shr),
                Ge => BinOpEq(Shr),
                _ => return None,
            },
            BinOp(op) => match joint.kind {
                Eq => BinOpEq(op),
                BinOp(And) if op == And => AndAnd,
                BinOp(Or) if op == Or => OrOr,
                Gt if op == Minus => RArrow,
                _ => return None,
            },
            Colon => match joint.kind {
                Colon => PathSep,
                _ => return None,
            },
            Le | EqEq | Ne | Ge | AndAnd | OrOr | Tilde | BinOpEq(_) | Dot | Comma | Semicolon
            | Question | OpenDelim(_) | CloseDelim(_) | Literal(_) | Ident(..) | DocComment(..)
            | PathSep | RArrow | Eof => return None,
        };

        Some(Token {
            kind,
            span: Span {
                offset: self.span.offset,
                length: self.span.length + joint.span.length,
            },
        })
    }

    pub fn is_punct(&self) -> bool {
        match self.kind {
            Eq | Lt | Le | EqEq | Ne | Ge | Gt | AndAnd | OrOr | Not | Tilde | BinOp(_)
            | BinOpEq(_) | Dot | Comma | Semicolon | Colon | Question => true,

            PathSep | OpenDelim(..) | CloseDelim(..) | Literal(..) | DocComment(..) | Ident(..)
            | Eof | RArrow => false,
        }
    }

    pub fn can_begin_expr(&self) -> bool {
        match self.kind {
            Ident(name, is_raw)              =>
                ident_can_begin_expr(name, self.span, is_raw), // value name or keyword
            OpenDelim(Delimiter::Parenthesis) | OpenDelim(Delimiter::Bracket) | // block | array
            Literal(..)                       | // literal
            Not                               | // operator not
            BinOp(Minus)                      => true, // unary minus
            _ => false,
        }
    }

    /// Returns `true` if the token is a non-raw identifier for which `pred` holds.
    pub fn is_non_raw_ident_where(&self, pred: impl FnOnce(Ident) -> bool) -> bool {
        match self.ident() {
            Some((id, IdentIsRaw::No)) => pred(id),
            _ => false,
        }
    }

    /// Returns an identifier if this token is an identifier.
    pub fn ident(&self) -> Option<(Ident, IdentIsRaw)> {
        match self.kind {
            Ident(name, is_raw) => Some((
                Ident {
                    name,
                    span: self.span,
                },
                is_raw,
            )),
            _ => None,
        }
    }

    /// Some token that will be thrown away later.
    pub fn dummy() -> Self {
        Token::new(TokenKind::Question, DUMMY_SP)
    }

    pub fn is_keyword(&self, keyword: Keyword) -> bool {
        self.is_non_raw_ident_where(|ident| {
            let res = kw::from_str(ident.name.as_str());
            if res.is_err() {
                return false;
            }

            keyword == res.unwrap()
        })
    }

    pub fn is_ident(&self) -> bool {
        matches!(self.kind, Ident(..))
    }
}

pub fn ident_can_begin_expr(name: Symbol, span: Span, is_raw: IdentIsRaw) -> bool {
    let ident_token = Token::new(Ident(name, is_raw), span);

    is_raw == IdentIsRaw::Yes
        || ident_token.is_non_raw_ident_where(|ident| match kw::from_str(ident.name.as_str()) {
            Ok(keyword) => match keyword {
                Keyword::True | Keyword::False => true,
                _ => false,
            },
            Err(_) => true,
        })
}
