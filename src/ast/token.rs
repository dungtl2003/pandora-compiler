use core::fmt;
use std::fmt::{Display, Formatter};

use crate::{
    kw::{self, Keyword},
    span_encoding::{Span, DUMMY_SP},
    symbol::Symbol,
};

use super::{ident::Ident, BinOpKind};
use BinOpToken::*;
use TokenKind::*;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.kind)
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

    pub fn to_ast_binop_kind(&self) -> Option<BinOpKind> {
        match self.kind {
            BinOp(Plus) => Some(BinOpKind::Add),
            BinOp(Minus) => Some(BinOpKind::Sub),
            BinOp(Star) => Some(BinOpKind::Mul),
            BinOp(Slash) => Some(BinOpKind::Div),
            BinOp(Percent) => Some(BinOpKind::Mod),
            EqEq => Some(BinOpKind::Eq),
            Ne => Some(BinOpKind::Ne),
            Lt => Some(BinOpKind::Lt),
            Le => Some(BinOpKind::Le),
            Gt => Some(BinOpKind::Gt),
            Ge => Some(BinOpKind::Ge),
            AndAnd => Some(BinOpKind::And),
            OrOr => Some(BinOpKind::Or),
            BinOp(And) => Some(BinOpKind::BitAnd),
            BinOp(Or) => Some(BinOpKind::BitOr),
            BinOp(Caret) => Some(BinOpKind::BitXor),
            BinOp(Shl) => Some(BinOpKind::Shl),
            BinOp(Shr) => Some(BinOpKind::Shr),
            _ => None,
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
            OpenDelim(Delimiter::Parenthesis)                     | // block
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
                    scope_id: None,
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

impl TokenKind {
    pub fn break_two_token_op(&self, n: u32) -> Option<(TokenKind, TokenKind)> {
        assert!(n == 1 || n == 2);
        Some(match (self, n) {
            (Le, 1) => (Lt, Eq),
            (EqEq, 1) => (Eq, Eq),
            (Ne, 1) => (Not, Eq),
            (Ge, 1) => (Gt, Eq),
            (AndAnd, 1) => (BinOp(And), BinOp(And)),
            (OrOr, 1) => (BinOp(Or), BinOp(Or)),
            (BinOp(Shl), 1) => (Lt, Lt),
            (BinOp(Shr), 1) => (Gt, Gt),
            (BinOpEq(Plus), 1) => (BinOp(Plus), Eq),
            (BinOpEq(Minus), 1) => (BinOp(Minus), Eq),
            (BinOpEq(Star), 1) => (BinOp(Star), Eq),
            (BinOpEq(Slash), 1) => (BinOp(Slash), Eq),
            (BinOpEq(Percent), 1) => (BinOp(Percent), Eq),
            (BinOpEq(Caret), 1) => (BinOp(Caret), Eq),
            (BinOpEq(And), 1) => (BinOp(And), Eq),
            (BinOpEq(Or), 1) => (BinOp(Or), Eq),
            (BinOpEq(Shl), 1) => (Lt, Le),         // `<` + `<=`
            (BinOpEq(Shl), 2) => (BinOp(Shl), Eq), // `<<` + `=`
            (BinOpEq(Shr), 1) => (Gt, Ge),         // `>` + `>=`
            (BinOpEq(Shr), 2) => (BinOp(Shr), Eq), // `>>` + `=`
            _ => return None,
        })
    }
}

pub fn ident_can_begin_expr(name: Symbol, span: Span, is_raw: IdentIsRaw) -> bool {
    let ident_token = Token::new(Ident(name, is_raw), span);

    !ident_token.is_non_raw_ident_where(|ident| kw::is_keyword(ident.name))
}
