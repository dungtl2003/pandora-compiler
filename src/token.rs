#[derive(Debug, PartialEq, Eq)]
pub struct Token {
    kind: TokenKind,
    len: u32,
}

impl Token {
    pub fn new(kind: TokenKind, len: u32) -> Token {
        Token { kind, len }
    }

    pub fn kind(&self) -> TokenKind {
        self.kind
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TokenKind {
    /* one/two-char symbol */
    /// :
    Colon,
    /// ,
    Comma,
    /// .
    Dot,
    /// ;
    Semicolon,
    /// ?
    Question,
    /// (
    OpenParen,
    /// )
    CloseParen,
    /// {
    OpenBrace,
    /// }
    CloseBrace,
    /// [
    OpenBracket,
    /// ]
    CloseBracket,
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
    /// `&&`
    AndAnd,
    /// `|`
    Or,
    /// `||`
    OrOr,
    /// `<<`
    Shl,
    /// `>>`
    Shr,

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

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum DocStyle {
    Inner,
    Outer,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum CommentKind {
    Line,
    Block { terminated: bool },
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum LiteralKind {
    /// `"abc"`, `"ab`, `"ab\"`, `"ab\""`.
    Str { terminated: bool },
    /// `r#"abc"#`, `r###"ab"##c"###`, `r###"ab"######`, None means invalid.
    RawStr { n_hashes: Option<u8> },
    /// `1_000`, `0b1101`, `0o657`, `0h1af9`.
    Number {
        base: Base,
        empty_digit: bool,
        empty_exponent: bool,
    },
    // Although kind can be Char but it can be many symbols (error). Ex: 'abc' -> error.
    /// `'a'`, `'\''`, `'\\'`, `'abc'`, `'ab`.
    Char { terminated: bool },
}

pub enum RawStrError {
    /// Non `#` symbol between `#` and `"`.
    InvalidStarter { bad_char: char },
    /// The string was not terminated. Ex: `r###"abc"##`.
    /// `possible_terminator_offset` is where we think you should add more `#` symbols.
    NoTerminator {
        expected: u32,
        found: u32,
        possible_terminator_offset: Option<u32>,
    },
    /// When there are more than 255 hashes.
    TooManyHashes { found: u32 },
}

/// Base of numeric literal encoding according to its prefix.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
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
