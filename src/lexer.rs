mod cursor;
mod token;
mod unescape;

pub use cursor::Cursor;
pub use token::{Base, DocStyle, LiteralKind, RawStrError, Token, TokenKind};
pub use unescape::{unescape_char, unescape_str, EscapeError};
use unicode_xid::UnicodeXID;

pub fn tokenize(source: &str) -> Vec<Token> {
    let mut tokens: Vec<Token> = Vec::new();
    let mut cursor = Cursor::new(source);

    loop {
        let token = cursor.advance_token();
        let is_last_token = token.kind == TokenKind::Eof;
        tokens.push(token);

        if is_last_token {
            return tokens;
        }
    }
}

fn is_valid_emoji(c: char) -> bool {
    // Ensure the character is a valid emoji with an emoji presentation.
    emojis::get(c.to_string().as_str()).is_some()
}

fn is_whitespace(c: char) -> bool {
    // This is Pattern_White_Space.
    //
    // Note that this set is stable (ie, it doesn't change with different
    // Unicode versions), so it's ok to just hard-code the values.

    matches!(
        c,
        // Usual ASCII suspects
        '\u{0009}'   // \t
        | '\u{000A}' // \n
        | '\u{000B}' // vertical tab
        | '\u{000C}' // form feed
        | '\u{000D}' // \r
        | '\u{0020}' // space

        // NEXT LINE from latin1
        | '\u{0085}'

        // Bidi markers
        | '\u{200E}' // LEFT-TO-RIGHT MARK
        | '\u{200F}' // RIGHT-TO-LEFT MARK

        // Dedicated whitespace characters from Unicode
        | '\u{2028}' // LINE SEPARATOR
        | '\u{2029}' // PARAGRAPH SEPARATOR
    )
}

fn is_id_start(c: char) -> bool {
    // This is XID_Start OR '_' OR emoji (which formally is not a XID_Start).
    c == '_' || UnicodeXID::is_xid_start(c) || is_valid_emoji(c)
}

fn is_id_continue(c: char) -> bool {
    // This is XID_Continue OR emoji (which formally is not a XID_Continue).
    UnicodeXID::is_xid_continue(c) || is_valid_emoji(c)
}

impl Cursor<'_> {
    /// Parses a token from the input string.
    pub fn advance_token(&mut self) -> Token {
        self.reset_bytes_eaten();

        let first_char = match self.eat() {
            Some(c) => c,
            None => return Token::new(TokenKind::Eof, 0),
        };

        let kind = match first_char {
            c if is_whitespace(c) => self.whitespace(),

            ':' => TokenKind::Colon,
            ',' => TokenKind::Comma,
            '.' => TokenKind::Dot,
            ';' => TokenKind::Semicolon,
            '?' => TokenKind::Question,

            '(' => TokenKind::OpenParen,
            ')' => TokenKind::CloseParen,
            '[' => TokenKind::OpenBracket,
            ']' => TokenKind::CloseBracket,
            '{' => TokenKind::OpenBrace,
            '}' => TokenKind::CloseBrace,

            '!' => self.not_or_not_equal(),
            '=' => self.assign_or_equal(),
            '>' => self.greater_or_ge_or_shift_right(),
            '<' => self.less_or_le_or_shift_left(),

            '~' => TokenKind::Tilde,
            '+' => TokenKind::Plus,
            '-' => TokenKind::Minus,
            '*' => TokenKind::Star,
            '%' => TokenKind::Percent,
            '^' => TokenKind::Caret,
            '&' => self.and_or_and_and(),
            '|' => self.or_or_or_or(),

            // Slash, comment or block comment.
            '/' => match self.first() {
                '/' => self.line_comment(),
                '*' => self.block_comment(),
                _ => TokenKind::Slash,
            },

            '0'..='9' => self.number(),

            // Raw identifier, Identifier, Raw double quote string
            'r' => match (self.first(), self.second()) {
                ('#', c1) if is_id_start(c1) => self.raw_identifier(),
                ('#', _) => {
                    let res = self.raw_double_quote_string();
                    TokenKind::Literal(LiteralKind::RawStr { n_hashes: res.ok() })
                }
                _ => self.identifier(),
            },

            '\'' => self.single_quote_string(),
            '"' => self.double_quote_string(),

            c if is_id_start(c) => self.identifier(),

            _ => TokenKind::Unknown,
        };

        Token::new(kind, self.bytes_eaten())
    }

    /// 🗿🗿🗿
    fn or_or_or_or(&mut self) -> TokenKind {
        debug_assert!(self.prev() == '|');

        match self.first() {
            '|' => {
                self.eat();
                TokenKind::OrOr
            }
            _ => TokenKind::Or,
        }
    }

    fn and_or_and_and(&mut self) -> TokenKind {
        debug_assert!(self.prev() == '&');

        match self.first() {
            '&' => {
                self.eat();
                TokenKind::AndAnd
            }
            _ => TokenKind::And,
        }
    }

    fn less_or_le_or_shift_left(&mut self) -> TokenKind {
        debug_assert!(self.prev() == '<');

        match self.first() {
            '=' => {
                self.eat();
                TokenKind::Le
            }
            '<' => {
                self.eat();
                TokenKind::Shl
            }
            _ => TokenKind::Le,
        }
    }

    fn greater_or_ge_or_shift_right(&mut self) -> TokenKind {
        debug_assert!(self.prev() == '>');

        match self.first() {
            '=' => {
                self.eat();
                TokenKind::Ge
            }
            '>' => {
                self.eat();
                TokenKind::Shr
            }
            _ => TokenKind::Gt,
        }
    }

    fn assign_or_equal(&mut self) -> TokenKind {
        debug_assert!(self.prev() == '=');

        match self.first() {
            '=' => {
                self.eat();
                TokenKind::Eq
            }
            _ => TokenKind::Eq,
        }
    }

    fn not_or_not_equal(&mut self) -> TokenKind {
        debug_assert!(self.prev() == '!');
        match self.first() {
            '=' => {
                self.eat();
                TokenKind::BangEq
            }
            _ => TokenKind::Bang,
        }
    }

    // String because this will not guarantee that there is only 1 symbol.
    fn single_quote_string(&mut self) -> TokenKind {
        debug_assert!(self.prev() == '\'');

        // If it only contains 1 symbol.
        if self.first() != '\\' && self.second() == '\'' {
            self.eat();
            self.eat();
            return TokenKind::Literal(LiteralKind::Char { terminated: true });
        }

        // This can contains more than 1 symbol.
        let mut terminated = false;
        while !self.is_eof() {
            match self.first() {
                // Eats twice because \ will take the character after it.
                '\\' => {
                    self.eat();
                    self.eat();
                }
                '\'' => {
                    terminated = true;
                    self.eat();
                    break;
                }
                _ => {
                    self.eat();
                }
            }
        }

        TokenKind::Literal(LiteralKind::Char { terminated })
    }

    fn whitespace(&mut self) -> TokenKind {
        debug_assert!(is_whitespace(self.prev()));
        self.eat_while(|ch| is_whitespace(ch));
        TokenKind::Whitespace
    }

    fn line_comment(&mut self) -> TokenKind {
        debug_assert!(self.prev() == '/' && self.first() == '/');

        // Skip '/'.
        self.eat();

        let doc_style = match self.first() {
            '!' => Some(DocStyle::Inner),
            '@' => Some(DocStyle::Outer),
            _ => None,
        };

        self.eat_while(|ch| ch != '\n');

        TokenKind::LineComment { doc_style }
    }

    fn block_comment(&mut self) -> TokenKind {
        debug_assert!(self.prev() == '/' && self.first() == '*');

        // Skip '*'.
        self.eat();

        let doc_style = match self.first() {
            '!' => Some(DocStyle::Inner),
            '@' => Some(DocStyle::Outer),
            _ => None,
        };

        // Number of "/*" that do not have "*/" left.
        let mut depths = 1;
        while !self.is_eof() && depths != 0 {
            if self.prev() == '/' && self.first() == '*' {
                depths += 1;
            } else if self.prev() == '*' && self.first() == '/' {
                depths -= 1;
            }

            self.eat();
        }

        TokenKind::BlockComment {
            terminated: depths == 0,
            doc_style,
        }
    }

    fn number(&mut self) -> TokenKind {
        debug_assert!('0' <= self.prev() && self.prev() <= '9');

        if self.prev() == '0' {
            match self.first() {
                'b' => {
                    self.eat();
                    return TokenKind::Literal(LiteralKind::Number {
                        base: Base::Binary,
                        empty_digit: !self.eat_binary_digits(),
                        empty_exponent: false,
                    });
                }
                'o' => {
                    self.eat();
                    return TokenKind::Literal(LiteralKind::Number {
                        base: Base::Octal,
                        empty_digit: !self.eat_octal_digits(),
                        empty_exponent: false,
                    });
                }
                'h' => {
                    self.eat();
                    return TokenKind::Literal(LiteralKind::Number {
                        base: Base::Hexadecimal,
                        empty_digit: !self.eat_hexa_digits(),
                        empty_exponent: false,
                    });
                }

                // Not a base prefix, eats all digits
                '0'..='9' | '_' => {
                    self.eat_decimal_digits();
                }

                'e' | 'E' => {}

                // Just 0.
                _ => {
                    return TokenKind::Literal(LiteralKind::Number {
                        base: Base::Decimal,
                        empty_digit: false,
                        empty_exponent: false,
                    })
                }
            }
        } else {
            self.eat_decimal_digits();
        }

        // Only Decimal base here, and the part before `.` or `e|E` has been eaten.
        match self.first() {
            // After '.' cannot be id_start because we might add method for primary type in the
            // future.
            // Funnily enough, method's name can be an emoji.
            '.' if !is_id_start(self.second()) && !is_valid_emoji(self.second()) => {
                self.eat();
                self.eat_decimal_digits();

                match self.first() {
                    'e' | 'E' => {
                        self.eat();
                        return TokenKind::Literal(LiteralKind::Number {
                            base: Base::Decimal,
                            empty_digit: false,
                            empty_exponent: !self.eat_exponent(),
                        });
                    }
                    _ => {}
                }
            }
            'e' | 'E' => {
                self.eat();
                return TokenKind::Literal(LiteralKind::Number {
                    base: Base::Decimal,
                    empty_digit: false,
                    empty_exponent: !self.eat_exponent(),
                });
            }
            _ => {
                return TokenKind::Literal(LiteralKind::Number {
                    base: Base::Decimal,
                    empty_digit: false,
                    empty_exponent: false,
                })
            }
        }

        // Just a normal number.
        return TokenKind::Literal(LiteralKind::Number {
            base: Base::Decimal,
            empty_digit: false,
            empty_exponent: false,
        });
    }

    /// Eats all _, 0, 1 and return `true` if there is atleast 1 digit, return `false`
    /// otherwise.
    fn eat_binary_digits(&mut self) -> bool {
        let mut has_digits = false;

        self.eat_while(|ch| ch == '_');

        if self.first() == '0' || self.first() == '1' {
            has_digits = true;
            self.eat_while(|ch| matches!(ch, '_' | '0' | '1'));
        }

        has_digits
    }

    /// Eats all _, 0-7 and return `true` if there is atleast 1 digit, return `false`
    /// otherwise.
    fn eat_octal_digits(&mut self) -> bool {
        let mut has_digits = false;

        self.eat_while(|ch| ch == '_');

        if self.first() >= '0' && self.first() <= '7' {
            has_digits = true;
            self.eat_while(|ch| matches!(ch, '_' | '0'..='7'));
        }

        has_digits
    }

    /// Eats all _, 0-9 and return `true` if there is atleast 1 digit, return `false`
    /// otherwise.
    fn eat_decimal_digits(&mut self) -> bool {
        let mut has_digits = false;

        self.eat_while(|ch| ch == '_');

        if self.first().is_ascii_digit() {
            has_digits = true;
            self.eat_while(|ch| ch == '_' || ch.is_ascii_digit());
        }

        has_digits
    }

    /// Eats all _, hexa digits, hexa letter and return `true` if there is atleast 1 digit or
    /// letter, return `false` otherwise.
    fn eat_hexa_digits(&mut self) -> bool {
        let mut has_digits = false;

        self.eat_while(|ch| ch == '_');

        if self.first().is_ascii_hexdigit() {
            has_digits = true;
            self.eat_while(|ch| ch == '_' || ch.is_ascii_hexdigit());
        }

        has_digits
    }

    /// Eats the exponent part, return `true` if there is atleast 1 digit, return `false`
    /// otherwise.
    fn eat_exponent(&mut self) -> bool {
        debug_assert!(matches!(self.prev(), 'e' | 'E'));

        if matches!(self.first(), '+' | '-') {
            self.eat();
        }

        self.eat_decimal_digits()
    }

    fn double_quote_string(&mut self) -> TokenKind {
        debug_assert!(self.prev() == '"');

        let mut terminated = false;
        while !self.is_eof() {
            match self.first() {
                // This will eat the character after it.
                '\\' => {
                    self.eat();
                    self.eat();
                }
                '"' => {
                    self.eat();
                    terminated = true;
                    break;
                }
                _ => {
                    self.eat();
                }
            }
        }

        TokenKind::Literal(LiteralKind::Str { terminated })
    }

    fn identifier(&mut self) -> TokenKind {
        // The first symbol is already eaten and checked so this must be true.
        debug_assert!(is_id_start(self.prev()));

        self.eat_while(|ch| is_id_continue(ch));

        TokenKind::Ident
    }

    fn raw_double_quote_string(&mut self) -> Result<u8, RawStrError> {
        debug_assert!(self.prev() == 'r' && self.first() == '#');

        let start_pos = self.bytes_eaten();
        let mut start_hashes = 0;

        while self.first() == '#' {
            self.eat();
            start_hashes += 1;
        }

        match self.first() {
            '"' => {}
            bad_char => return Err(RawStrError::InvalidStarter { bad_char }),
        }

        let mut possible_terminator_offset: Option<u32> = None;
        let mut max_end_hashes: u32 = 0;
        let mut maybe_end_hashes: u32;

        loop {
            self.eat_while(|ch| ch != '"');
            if self.is_eof() {
                return Err(RawStrError::NoTerminator {
                    expected: start_hashes,
                    found: max_end_hashes,
                    possible_terminator_offset,
                });
            }

            self.eat();
            match self.first() {
                '#' => {
                    maybe_end_hashes = 0;
                    while self.first() == '#' && maybe_end_hashes < start_hashes {
                        self.eat();
                        maybe_end_hashes += 1;
                    }

                    if maybe_end_hashes == start_hashes {
                        if maybe_end_hashes > 255 {
                            return Err(RawStrError::TooManyHashes {
                                found: maybe_end_hashes,
                            });
                        }

                        return Ok(start_hashes as u8);
                    }

                    // end < start
                    if maybe_end_hashes > max_end_hashes {
                        max_end_hashes = maybe_end_hashes;
                        possible_terminator_offset =
                            Some(self.bytes_eaten() - start_pos + 1 - max_end_hashes);
                    }
                }
                _ => {}
            }
        }
    }

    fn raw_identifier(&mut self) -> TokenKind {
        debug_assert!(self.prev() == 'r' && self.first() == '#' && is_id_start(self.second()));

        // eat `#`
        self.eat();
        self.eat_while(|ch| is_id_continue(ch));

        TokenKind::RawIdent
    }

    fn eat_while(&mut self, predicate: impl Fn(char) -> bool) {
        while !self.is_eof() && predicate(self.first()) {
            self.eat();
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn basic() {
        let source = r#####"
/*@ this is main function */
fun main() {
    let x: number = 9; // create x
    let y: number = 8;
    let z: number = x + y;
    sysout("x + y = ${x + y}");
}
"#####;
        let mut tokens_iter = tokenize(&source).into_iter();
        assert_eq!(
            tokens_iter.next(),
            Some(Token::new(TokenKind::Whitespace, 1))
        ); //
        assert_eq!(
            tokens_iter.next(),
            Some(Token::new(
                TokenKind::BlockComment {
                    terminated: true,
                    doc_style: Some(DocStyle::Outer)
                },
                28
            ))
        ); // /*@ this is main function */
        assert_eq!(
            tokens_iter.next(),
            Some(Token::new(TokenKind::Whitespace, 1))
        ); //
        assert_eq!(tokens_iter.next(), Some(Token::new(TokenKind::Ident, 3))); // fun
        assert_eq!(
            tokens_iter.next(),
            Some(Token::new(TokenKind::Whitespace, 1))
        ); //
        assert_eq!(tokens_iter.next(), Some(Token::new(TokenKind::Ident, 4))); // main
        assert_eq!(
            tokens_iter.next(),
            Some(Token::new(TokenKind::OpenParen, 1))
        ); // (
        assert_eq!(
            tokens_iter.next(),
            Some(Token::new(TokenKind::CloseParen, 1))
        ); // )
        assert_eq!(
            tokens_iter.next(),
            Some(Token::new(TokenKind::Whitespace, 1))
        ); //
        assert_eq!(
            tokens_iter.next(),
            Some(Token::new(TokenKind::OpenBrace, 1))
        ); // {
        assert_eq!(
            tokens_iter.next(),
            Some(Token::new(TokenKind::Whitespace, 5))
        ); //
        assert_eq!(tokens_iter.next(), Some(Token::new(TokenKind::Ident, 3))); // let
        assert_eq!(
            tokens_iter.next(),
            Some(Token::new(TokenKind::Whitespace, 1))
        ); //
        assert_eq!(tokens_iter.next(), Some(Token::new(TokenKind::Ident, 1))); // x
        assert_eq!(tokens_iter.next(), Some(Token::new(TokenKind::Colon, 1))); // :
        assert_eq!(
            tokens_iter.next(),
            Some(Token::new(TokenKind::Whitespace, 1))
        ); //
        assert_eq!(tokens_iter.next(), Some(Token::new(TokenKind::Ident, 6))); // number
        assert_eq!(
            tokens_iter.next(),
            Some(Token::new(TokenKind::Whitespace, 1))
        ); //
        assert_eq!(tokens_iter.next(), Some(Token::new(TokenKind::Eq, 1))); // =
        assert_eq!(
            tokens_iter.next(),
            Some(Token::new(TokenKind::Whitespace, 1))
        ); //
        assert_eq!(
            tokens_iter.next(),
            Some(Token::new(
                TokenKind::Literal(LiteralKind::Number {
                    base: Base::Decimal,
                    empty_digit: false,
                    empty_exponent: false
                }),
                1
            ))
        ); // 9
        assert_eq!(
            tokens_iter.next(),
            Some(Token::new(TokenKind::Semicolon, 1))
        ); // ;
        assert_eq!(
            tokens_iter.next(),
            Some(Token::new(TokenKind::Whitespace, 1))
        ); //

        assert_eq!(
            tokens_iter.next(),
            Some(Token::new(TokenKind::LineComment { doc_style: None }, 11))
        ); // // create x
        assert_eq!(
            tokens_iter.next(),
            Some(Token::new(TokenKind::Whitespace, 5))
        ); //
        assert_eq!(tokens_iter.next(), Some(Token::new(TokenKind::Ident, 3))); // let
        assert_eq!(
            tokens_iter.next(),
            Some(Token::new(TokenKind::Whitespace, 1))
        ); //
        assert_eq!(tokens_iter.next(), Some(Token::new(TokenKind::Ident, 1))); // y
        assert_eq!(tokens_iter.next(), Some(Token::new(TokenKind::Colon, 1))); // :
        assert_eq!(
            tokens_iter.next(),
            Some(Token::new(TokenKind::Whitespace, 1))
        ); //
        assert_eq!(tokens_iter.next(), Some(Token::new(TokenKind::Ident, 6))); // number
        assert_eq!(
            tokens_iter.next(),
            Some(Token::new(TokenKind::Whitespace, 1))
        ); //
        assert_eq!(tokens_iter.next(), Some(Token::new(TokenKind::Eq, 1))); // =
        assert_eq!(
            tokens_iter.next(),
            Some(Token::new(TokenKind::Whitespace, 1))
        ); //
        assert_eq!(
            tokens_iter.next(),
            Some(Token::new(
                TokenKind::Literal(LiteralKind::Number {
                    base: Base::Decimal,
                    empty_digit: false,
                    empty_exponent: false
                }),
                1
            ))
        ); // 8
        assert_eq!(
            tokens_iter.next(),
            Some(Token::new(TokenKind::Semicolon, 1))
        ); // ;
        assert_eq!(
            tokens_iter.next(),
            Some(Token::new(TokenKind::Whitespace, 5))
        ); //
        assert_eq!(tokens_iter.next(), Some(Token::new(TokenKind::Ident, 3))); // let
        assert_eq!(
            tokens_iter.next(),
            Some(Token::new(TokenKind::Whitespace, 1))
        ); //
        assert_eq!(tokens_iter.next(), Some(Token::new(TokenKind::Ident, 1))); // z
        assert_eq!(tokens_iter.next(), Some(Token::new(TokenKind::Colon, 1))); // :
        assert_eq!(
            tokens_iter.next(),
            Some(Token::new(TokenKind::Whitespace, 1))
        ); //
        assert_eq!(tokens_iter.next(), Some(Token::new(TokenKind::Ident, 6))); // number
        assert_eq!(
            tokens_iter.next(),
            Some(Token::new(TokenKind::Whitespace, 1))
        ); //
        assert_eq!(tokens_iter.next(), Some(Token::new(TokenKind::Eq, 1))); // =
        assert_eq!(
            tokens_iter.next(),
            Some(Token::new(TokenKind::Whitespace, 1))
        ); //
        assert_eq!(tokens_iter.next(), Some(Token::new(TokenKind::Ident, 1))); // x
        assert_eq!(
            tokens_iter.next(),
            Some(Token::new(TokenKind::Whitespace, 1))
        ); //
        assert_eq!(tokens_iter.next(), Some(Token::new(TokenKind::Plus, 1))); // +
        assert_eq!(
            tokens_iter.next(),
            Some(Token::new(TokenKind::Whitespace, 1))
        ); //
        assert_eq!(tokens_iter.next(), Some(Token::new(TokenKind::Ident, 1))); // y
        assert_eq!(
            tokens_iter.next(),
            Some(Token::new(TokenKind::Semicolon, 1))
        ); // ;
        assert_eq!(
            tokens_iter.next(),
            Some(Token::new(TokenKind::Whitespace, 5))
        ); //
        assert_eq!(tokens_iter.next(), Some(Token::new(TokenKind::Ident, 6))); // sysout
        assert_eq!(
            tokens_iter.next(),
            Some(Token::new(TokenKind::OpenParen, 1))
        ); // (
        assert_eq!(
            tokens_iter.next(),
            Some(Token::new(
                TokenKind::Literal(LiteralKind::Str { terminated: true }),
                18
            ))
        ); // "x + y = ${x + y}"
        assert_eq!(
            tokens_iter.next(),
            Some(Token::new(TokenKind::CloseParen, 1))
        ); // )
        assert_eq!(
            tokens_iter.next(),
            Some(Token::new(TokenKind::Semicolon, 1))
        ); // ;
        assert_eq!(
            tokens_iter.next(),
            Some(Token::new(TokenKind::Whitespace, 1))
        ); //
        assert_eq!(
            tokens_iter.next(),
            Some(Token::new(TokenKind::CloseBrace, 1))
        ); // )
        assert_eq!(
            tokens_iter.next(),
            Some(Token::new(TokenKind::Whitespace, 1))
        ); //
        assert_eq!(tokens_iter.next(), Some(Token::new(TokenKind::Eof, 0))); //
        assert_eq!(tokens_iter.next(), None); //
    }

    #[test]
    fn tokenize_raw_double_quote_string() {
        let source = r#####"
r#"abc"#
r###"a"##b"###
r##"a"####
r####"a"#"ab"###"##
"#####;
        let mut cursor = Cursor::new(&source);

        // r#"abc"#
        assert_eq!(cursor.advance_token(), Token::new(TokenKind::Whitespace, 1));
        assert_eq!(
            cursor.advance_token(),
            Token::new(
                TokenKind::Literal(LiteralKind::RawStr { n_hashes: Some(1) }),
                8
            )
        );

        // r###"a"##b"###
        assert_eq!(cursor.advance_token(), Token::new(TokenKind::Whitespace, 1));
        assert_eq!(
            cursor.advance_token(),
            Token::new(
                TokenKind::Literal(LiteralKind::RawStr { n_hashes: Some(3) }),
                14
            )
        );

        // r##"a"####
        assert_eq!(cursor.advance_token(), Token::new(TokenKind::Whitespace, 1));
        assert_eq!(
            cursor.advance_token(),
            Token::new(
                TokenKind::Literal(LiteralKind::RawStr { n_hashes: Some(2) }),
                8
            )
        );
        assert_eq!(cursor.advance_token(), Token::new(TokenKind::Unknown, 1));
        assert_eq!(cursor.advance_token(), Token::new(TokenKind::Unknown, 1));

        // r####"a"#"ab"###"##
        assert_eq!(cursor.advance_token(), Token::new(TokenKind::Whitespace, 1));
        assert_eq!(
            cursor.advance_token(),
            Token::new(
                TokenKind::Literal(LiteralKind::RawStr { n_hashes: None }),
                20
            )
        );
    }

    #[test]
    fn tokenize_double_quote_string() {
        let source = r#"
"ab🤨"
"ac\"f"
"ab\"a
"#;
        let mut cursor = Cursor::new(&source);

        // "ab🤨"
        assert_eq!(cursor.advance_token(), Token::new(TokenKind::Whitespace, 1));
        assert_eq!(
            cursor.advance_token(),
            Token::new(TokenKind::Literal(LiteralKind::Str { terminated: true }), 8)
        );

        // "ac\"f"
        assert_eq!(cursor.advance_token(), Token::new(TokenKind::Whitespace, 1));
        assert_eq!(
            cursor.advance_token(),
            Token::new(TokenKind::Literal(LiteralKind::Str { terminated: true }), 7)
        );

        // "ab\"a
        assert_eq!(cursor.advance_token(), Token::new(TokenKind::Whitespace, 1));
        assert_eq!(
            cursor.advance_token(),
            Token::new(
                TokenKind::Literal(LiteralKind::Str { terminated: false }),
                7
            )
        );
    }

    #[test]
    fn tokenize_single_quote_string() {
        let source = r#"
'a'
'ab🤨'
'ac\'f'
'ab\'a
"#;
        let mut cursor = Cursor::new(&source);

        // 'a'
        assert_eq!(cursor.advance_token(), Token::new(TokenKind::Whitespace, 1));
        assert_eq!(
            cursor.advance_token(),
            Token::new(
                TokenKind::Literal(LiteralKind::Char { terminated: true }),
                3
            )
        );

        // Although more than 1 symbol but still count as char (for now).
        // 'ab🤨'
        assert_eq!(cursor.advance_token(), Token::new(TokenKind::Whitespace, 1));
        assert_eq!(
            cursor.advance_token(),
            Token::new(
                TokenKind::Literal(LiteralKind::Char { terminated: true }),
                8
            )
        );

        // 'ac\'f'
        assert_eq!(cursor.advance_token(), Token::new(TokenKind::Whitespace, 1));
        assert_eq!(
            cursor.advance_token(),
            Token::new(
                TokenKind::Literal(LiteralKind::Char { terminated: true }),
                7
            )
        );

        // 'ab\'a
        assert_eq!(cursor.advance_token(), Token::new(TokenKind::Whitespace, 1));
        assert_eq!(
            cursor.advance_token(),
            Token::new(
                TokenKind::Literal(LiteralKind::Char { terminated: false }),
                7
            )
        );
    }

    #[test]
    fn tokenize_block_comment() {
        let source = r#"
/* This is
a block
comment. */
/*! Inner
*/
/*@
Outer
*/
/* /* /*
 * Multidepths
 */ abc */ xyz*/
/* /* /*
 * Not terminated
 */ abc xyz*/
"#;
        let mut cursor = Cursor::new(&source);

        // /* This is
        // a block
        // comment. */
        assert_eq!(cursor.advance_token(), Token::new(TokenKind::Whitespace, 1));
        assert_eq!(
            cursor.advance_token(),
            Token::new(
                TokenKind::BlockComment {
                    terminated: true,
                    doc_style: None
                },
                30
            )
        );

        // /*! Inner
        // */
        assert_eq!(cursor.advance_token(), Token::new(TokenKind::Whitespace, 1));
        assert_eq!(
            cursor.advance_token(),
            Token::new(
                TokenKind::BlockComment {
                    terminated: true,
                    doc_style: Some(DocStyle::Inner),
                },
                12
            )
        );

        // /*@
        // Outer
        // */
        assert_eq!(cursor.advance_token(), Token::new(TokenKind::Whitespace, 1));
        assert_eq!(
            cursor.advance_token(),
            Token::new(
                TokenKind::BlockComment {
                    terminated: true,
                    doc_style: Some(DocStyle::Outer),
                },
                12
            )
        );

        // /* /* /*
        //  * Multidepths
        //  */ abc */ xyz*/
        assert_eq!(cursor.advance_token(), Token::new(TokenKind::Whitespace, 1));
        assert_eq!(
            cursor.advance_token(),
            Token::new(
                TokenKind::BlockComment {
                    terminated: true,
                    doc_style: None,
                },
                40
            )
        );

        // /* /* /*
        //  * Not terminated
        //  */ abc xyz*/
        assert_eq!(cursor.advance_token(), Token::new(TokenKind::Whitespace, 1));
        assert_eq!(
            cursor.advance_token(),
            Token::new(
                TokenKind::BlockComment {
                    terminated: false,
                    doc_style: None,
                },
                41
            )
        );
    }

    #[test]
    fn tokenize_line_comment() {
        let source = r#"
// Normal comment.
//! Inner doc comment.
//!!!!Still inner.
//@ Outer doc comment.
//@@@@Still outer.
"#;
        let mut cursor = Cursor::new(&source);

        // // Normal comment.
        assert_eq!(cursor.advance_token(), Token::new(TokenKind::Whitespace, 1));
        assert_eq!(
            cursor.advance_token(),
            Token::new(TokenKind::LineComment { doc_style: None }, 18,)
        );

        // //! Inner doc comment.
        assert_eq!(cursor.advance_token(), Token::new(TokenKind::Whitespace, 1));
        assert_eq!(
            cursor.advance_token(),
            Token::new(
                TokenKind::LineComment {
                    doc_style: Some(DocStyle::Inner),
                },
                22,
            )
        );

        // //!!!!Still inner.
        assert_eq!(cursor.advance_token(), Token::new(TokenKind::Whitespace, 1));
        assert_eq!(
            cursor.advance_token(),
            Token::new(
                TokenKind::LineComment {
                    doc_style: Some(DocStyle::Inner),
                },
                18,
            )
        );

        // //@ Outer doc comment.
        assert_eq!(cursor.advance_token(), Token::new(TokenKind::Whitespace, 1));
        assert_eq!(
            cursor.advance_token(),
            Token::new(
                TokenKind::LineComment {
                    doc_style: Some(DocStyle::Outer),
                },
                22,
            )
        );

        // //@@@@Still outer.
        assert_eq!(cursor.advance_token(), Token::new(TokenKind::Whitespace, 1));
        assert_eq!(
            cursor.advance_token(),
            Token::new(
                TokenKind::LineComment {
                    doc_style: Some(DocStyle::Outer),
                },
                18,
            )
        );
    }

    #[test]
    fn tokenize_raw_identifier() {
        let source = r#"
r#true
r#_
r#my_name_jeff_123
r#🥰🫢👻
"#;

        let mut cursor = Cursor::new(&source);

        //r#true
        assert_eq!(cursor.advance_token(), Token::new(TokenKind::Whitespace, 1));
        assert_eq!(cursor.advance_token(), Token::new(TokenKind::RawIdent, 6));

        //r#_
        assert_eq!(cursor.advance_token(), Token::new(TokenKind::Whitespace, 1));
        assert_eq!(cursor.advance_token(), Token::new(TokenKind::RawIdent, 3));

        //r#my_name_jeff_123
        assert_eq!(cursor.advance_token(), Token::new(TokenKind::Whitespace, 1));
        assert_eq!(cursor.advance_token(), Token::new(TokenKind::RawIdent, 18));

        //r#🥰🫢👻
        assert_eq!(cursor.advance_token(), Token::new(TokenKind::Whitespace, 1));
        assert_eq!(cursor.advance_token(), Token::new(TokenKind::RawIdent, 14));
    }

    #[test]
    fn tokenize_identifier() {
        let source = r#"
x
_
_p_
customer_id_is_1
🥰🫢👻
"#;
        let mut cursor = Cursor::new(&source);

        //x
        assert_eq!(cursor.advance_token(), Token::new(TokenKind::Whitespace, 1));
        assert_eq!(cursor.advance_token(), Token::new(TokenKind::Ident, 1));

        //_
        assert_eq!(cursor.advance_token(), Token::new(TokenKind::Whitespace, 1));
        assert_eq!(cursor.advance_token(), Token::new(TokenKind::Ident, 1));

        //_p_
        assert_eq!(cursor.advance_token(), Token::new(TokenKind::Whitespace, 1));
        assert_eq!(cursor.advance_token(), Token::new(TokenKind::Ident, 3));

        //customer_id_is_1
        assert_eq!(cursor.advance_token(), Token::new(TokenKind::Whitespace, 1));
        assert_eq!(cursor.advance_token(), Token::new(TokenKind::Ident, 16));

        //🥰🫢👻
        assert_eq!(cursor.advance_token(), Token::new(TokenKind::Whitespace, 1));
        assert_eq!(cursor.advance_token(), Token::new(TokenKind::Ident, 12));
    }

    #[test]
    fn tokenize_binary_number() {
        let source = r#"
0b1011_1101_0010
0b____
0b
0b_1___0_____
"#;

        let mut cursor = Cursor::new(&source);

        //0b1011_1101_0010
        assert_eq!(cursor.advance_token(), Token::new(TokenKind::Whitespace, 1));
        assert_eq!(
            cursor.advance_token(),
            Token::new(
                TokenKind::Literal(LiteralKind::Number {
                    base: Base::Binary,
                    empty_digit: false,
                    empty_exponent: false
                }),
                16
            )
        );

        //0b____
        assert_eq!(cursor.advance_token(), Token::new(TokenKind::Whitespace, 1));
        assert_eq!(
            cursor.advance_token(),
            Token::new(
                TokenKind::Literal(LiteralKind::Number {
                    base: Base::Binary,
                    empty_digit: true,
                    empty_exponent: false
                }),
                6
            )
        );

        //0b
        assert_eq!(cursor.advance_token(), Token::new(TokenKind::Whitespace, 1));
        assert_eq!(
            cursor.advance_token(),
            Token::new(
                TokenKind::Literal(LiteralKind::Number {
                    base: Base::Binary,
                    empty_digit: true,
                    empty_exponent: false
                }),
                2
            )
        );

        //0b_1___0_____
        assert_eq!(cursor.advance_token(), Token::new(TokenKind::Whitespace, 1));
        assert_eq!(
            cursor.advance_token(),
            Token::new(
                TokenKind::Literal(LiteralKind::Number {
                    base: Base::Binary,
                    empty_digit: false,
                    empty_exponent: false
                }),
                13
            )
        );
    }

    #[test]
    fn tokenize_octal_number() {
        let source = r#"
0o670_561_1
0o
0o_____
0o__6___7__
"#;
        let mut cursor = Cursor::new(&source);

        //0o670_561_1
        assert_eq!(cursor.advance_token(), Token::new(TokenKind::Whitespace, 1));
        assert_eq!(
            cursor.advance_token(),
            Token::new(
                TokenKind::Literal(LiteralKind::Number {
                    base: Base::Octal,
                    empty_digit: false,
                    empty_exponent: false
                }),
                11
            )
        );

        //0o
        assert_eq!(cursor.advance_token(), Token::new(TokenKind::Whitespace, 1));
        assert_eq!(
            cursor.advance_token(),
            Token::new(
                TokenKind::Literal(LiteralKind::Number {
                    base: Base::Octal,
                    empty_digit: true,
                    empty_exponent: false
                }),
                2
            )
        );

        //0o_____
        assert_eq!(cursor.advance_token(), Token::new(TokenKind::Whitespace, 1));
        assert_eq!(
            cursor.advance_token(),
            Token::new(
                TokenKind::Literal(LiteralKind::Number {
                    base: Base::Octal,
                    empty_digit: true,
                    empty_exponent: false
                }),
                7
            )
        );

        //0o__6___7__
        assert_eq!(cursor.advance_token(), Token::new(TokenKind::Whitespace, 1));
        assert_eq!(
            cursor.advance_token(),
            Token::new(
                TokenKind::Literal(LiteralKind::Number {
                    base: Base::Octal,
                    empty_digit: false,
                    empty_exponent: false
                }),
                11
            )
        );
    }

    #[test]
    fn tokenize_hexa_number() {
        let source = r#"
0h0e_Ab_4f_53
0h
0h______
0h__1___a__
"#;

        let mut cursor = Cursor::new(&source);

        //0h0e_Ab_4f_53
        assert_eq!(cursor.advance_token(), Token::new(TokenKind::Whitespace, 1));
        assert_eq!(
            cursor.advance_token(),
            Token::new(
                TokenKind::Literal(LiteralKind::Number {
                    base: Base::Hexadecimal,
                    empty_digit: false,
                    empty_exponent: false
                }),
                13
            )
        );

        //0h
        assert_eq!(cursor.advance_token(), Token::new(TokenKind::Whitespace, 1));
        assert_eq!(
            cursor.advance_token(),
            Token::new(
                TokenKind::Literal(LiteralKind::Number {
                    base: Base::Hexadecimal,
                    empty_digit: true,
                    empty_exponent: false
                }),
                2
            )
        );

        //0h______
        assert_eq!(cursor.advance_token(), Token::new(TokenKind::Whitespace, 1));
        assert_eq!(
            cursor.advance_token(),
            Token::new(
                TokenKind::Literal(LiteralKind::Number {
                    base: Base::Hexadecimal,
                    empty_digit: true,
                    empty_exponent: false
                }),
                8
            )
        );

        //0h__1___a__
        assert_eq!(cursor.advance_token(), Token::new(TokenKind::Whitespace, 1));
        assert_eq!(
            cursor.advance_token(),
            Token::new(
                TokenKind::Literal(LiteralKind::Number {
                    base: Base::Hexadecimal,
                    empty_digit: false,
                    empty_exponent: false
                }),
                11,
            )
        );
    }

    #[test]
    fn tokenize_decimal_number() {
        let source = r#"
1_000_000
3.
3.141_592
2E5__
2e+__5
2.3__e-_5_
2e_
3.e
"#;

        let mut cursor = Cursor::new(&source);

        //1_000_000
        assert_eq!(cursor.advance_token(), Token::new(TokenKind::Whitespace, 1));
        assert_eq!(
            cursor.advance_token(),
            Token::new(
                TokenKind::Literal(LiteralKind::Number {
                    base: Base::Decimal,
                    empty_digit: false,
                    empty_exponent: false
                }),
                9,
            )
        );

        // 3.
        assert_eq!(cursor.advance_token(), Token::new(TokenKind::Whitespace, 1));
        assert_eq!(
            cursor.advance_token(),
            Token::new(
                TokenKind::Literal(LiteralKind::Number {
                    base: Base::Decimal,
                    empty_digit: false,
                    empty_exponent: false
                }),
                2,
            )
        );

        //3.141_592
        assert_eq!(cursor.advance_token(), Token::new(TokenKind::Whitespace, 1));
        assert_eq!(
            cursor.advance_token(),
            Token::new(
                TokenKind::Literal(LiteralKind::Number {
                    base: Base::Decimal,
                    empty_digit: false,
                    empty_exponent: false
                }),
                9,
            )
        );

        //2E5__
        assert_eq!(cursor.advance_token(), Token::new(TokenKind::Whitespace, 1));
        assert_eq!(
            cursor.advance_token(),
            Token::new(
                TokenKind::Literal(LiteralKind::Number {
                    base: Base::Decimal,
                    empty_digit: false,
                    empty_exponent: false
                }),
                5,
            )
        );

        //2e+__5
        assert_eq!(cursor.advance_token(), Token::new(TokenKind::Whitespace, 1));
        assert_eq!(
            cursor.advance_token(),
            Token::new(
                TokenKind::Literal(LiteralKind::Number {
                    base: Base::Decimal,
                    empty_digit: false,
                    empty_exponent: false
                }),
                6,
            )
        );

        //2.3__e-_5_
        assert_eq!(cursor.advance_token(), Token::new(TokenKind::Whitespace, 1));
        assert_eq!(
            cursor.advance_token(),
            Token::new(
                TokenKind::Literal(LiteralKind::Number {
                    base: Base::Decimal,
                    empty_digit: false,
                    empty_exponent: false
                }),
                10,
            )
        );

        //2e_
        assert_eq!(cursor.advance_token(), Token::new(TokenKind::Whitespace, 1));
        assert_eq!(
            cursor.advance_token(),
            Token::new(
                TokenKind::Literal(LiteralKind::Number {
                    base: Base::Decimal,
                    empty_digit: false,
                    empty_exponent: true
                }),
                3,
            )
        );

        // case 3.e means number is 3
        assert_eq!(cursor.advance_token(), Token::new(TokenKind::Whitespace, 1));
        assert_eq!(
            cursor.advance_token(),
            Token::new(
                TokenKind::Literal(LiteralKind::Number {
                    base: Base::Decimal,
                    empty_digit: false,
                    empty_exponent: false
                }),
                1,
            )
        );
    }
}
