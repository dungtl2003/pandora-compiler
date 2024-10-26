type BytePos = u32;

#[derive(Debug, PartialEq, Eq)]
pub struct Token {
    kind: TokenKind,
    len: u32,
}

impl Token {
    pub fn new(kind: TokenKind, len: u32) -> Token {
        Token { kind, len }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum TokenKind {
    /* Structure symbols */
    /// ,
    Comma,
    /// .
    Dot,
    /// ;
    Semicolon,
    /// '
    SingleQuote,
    OpenDelim(Delimiter),
    CloseDelim(Delimiter),

    /* Expression-operator symbols */
    /// `!`
    Not,
    /// `!=`
    NotEqual,
    /// `=`
    Assign,
    /// `==`
    Equal,
    /// `>`
    Greater,
    /// `>=`
    GreaterEqual,
    /// `<`
    Less,
    /// `<=`
    LessEqual,
    /// `~`
    Tilde,
    BinOp(BinOpToken),

    // Literal
    Literal(LiteralKind),

    // Identifier
    Ident,

    // Raw identifier
    RawIdent,

    // Comments
    Comment {
        kind: CommentKind,
        doc_style: Option<DocStyle>,
    },

    Whitespace,

    // Unknown token's kind.
    Unknown,

    /// End of input.
    Eof,
}

#[derive(Debug, PartialEq, Eq)]
pub enum DocStyle {
    Inner,
    Outer,
}

#[derive(Debug, PartialEq, Eq)]
pub enum CommentKind {
    Line,
    Block { is_terminated: bool },
}

#[derive(Debug, PartialEq, Eq)]
pub enum BinOpToken {
    /// `+`
    Plus,
    /// `-`
    Minus,
    /// `*`
    Star,
    /// `/`
    Slash,
    /// `%`
    Percent,
    /// "^"
    Caret,
    /// `&`
    And,
    /// `|`
    Or,
    /// `<<`
    Shl,
    /// `>>`
    Shr,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Delimiter {
    /// `( ... )`
    Parenthesis,
    /// `{ ... }`
    Brace,
    /// `[ ... ]`
    Bracket,
}

#[derive(Debug, PartialEq, Eq)]
pub enum LiteralKind {
    Str,
    RawStr,
    Number {
        base: Base,
        empty_digit: bool,
        empty_exponent: bool,
    },
    Char,
}

/// Base of numeric literal encoding according to its prefix.
#[derive(Debug, PartialEq, Eq)]
pub enum Base {
    /// Literal starts with "0b".
    Binary = 2,
    /// Literal starts with "0o".
    Octal = 8,
    /// Literal doesn't contain a prefix.
    Decimal = 10,
    /// Literal starts with "0h".
    Hexadecimal = 16,
}
