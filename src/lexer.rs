use unicode_xid::UnicodeXID;

mod cursor;

use crate::token::{
    Base, BinOpToken, CommentKind, Delimiter, DocStyle, LiteralKind, Token, TokenKind,
};
use cursor::Cursor;

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
    fn advance_token(&mut self) -> Token {
        self.reset_bytes_eaten();

        let first_char = match self.eat() {
            Some(c) => c,
            None => return Token::new(TokenKind::Eof, 0),
        };

        let kind = match first_char {
            c if is_whitespace(c) => self.whitespace(),

            ',' => TokenKind::Comma,
            '.' => TokenKind::Dot,
            ';' => TokenKind::Semicolon,

            '(' => TokenKind::OpenDelim(Delimiter::Parenthesis),
            ')' => TokenKind::CloseDelim(Delimiter::Parenthesis),
            '[' => TokenKind::OpenDelim(Delimiter::Bracket),
            ']' => TokenKind::CloseDelim(Delimiter::Bracket),
            '{' => TokenKind::OpenDelim(Delimiter::Brace),
            '}' => TokenKind::CloseDelim(Delimiter::Brace),

            // Not or Not equal
            '!' => match self.first() {
                '=' => {
                    self.eat();
                    TokenKind::NotEqual
                }
                _ => TokenKind::Not,
            },

            // Assign or Equal
            '=' => match self.first() {
                '=' => {
                    self.eat();
                    TokenKind::Equal
                }
                _ => TokenKind::Assign,
            },

            // Greater, Greater or equal, Shift right
            '>' => match self.first() {
                '=' => {
                    self.eat();
                    TokenKind::GreaterEqual
                }
                '>' => {
                    self.eat();
                    TokenKind::BinOp(BinOpToken::Shr)
                }
                _ => TokenKind::Greater,
            },

            // Less, Less or equal, Shift left
            '<' => match self.first() {
                '=' => {
                    self.eat();
                    TokenKind::LessEqual
                }
                '<' => {
                    self.eat();
                    TokenKind::BinOp(BinOpToken::Shl)
                }
                _ => TokenKind::Less,
            },

            '~' => TokenKind::Tilde,
            '+' => TokenKind::BinOp(BinOpToken::Plus),
            '-' => TokenKind::BinOp(BinOpToken::Minus),
            '*' => TokenKind::BinOp(BinOpToken::Star),
            '%' => TokenKind::BinOp(BinOpToken::Percent),
            '^' => TokenKind::BinOp(BinOpToken::Caret),
            '&' => TokenKind::BinOp(BinOpToken::And),
            '|' => TokenKind::BinOp(BinOpToken::Or),

            // Slash, comment or block comment.
            '/' => match self.first() {
                '/' => self.line_comment(),
                '*' => self.block_comment(),
                _ => TokenKind::BinOp(BinOpToken::Slash),
            },

            '0'..='9' => self.number(),

            // Raw identifier, Identifier, Raw double quote string
            'r' => match (self.first(), self.second()) {
                ('#', c1) if is_id_start(c1) => self.raw_identifier(),
                ('#', _) => self.raw_double_quote_string(),
                _ => self.identifier(),
            },

            '\"' => self.double_quote_string(),

            c if is_id_start(c) => self.identifier(),

            _ => TokenKind::Unknown,
        };

        Token::new(kind, self.bytes_eaten())
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

        TokenKind::Comment {
            kind: CommentKind::Line,
            doc_style,
        }
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

        TokenKind::Comment {
            kind: CommentKind::Block {
                is_terminated: depths == 0,
            },
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
                        empty_digit: !self.eat_decimal_digits(),
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
            self.eat_while(|ch| ch == '_' || ch == '0' || ch == '1');
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
            self.eat_while(|ch| ch == '_' || (ch >= '0' && ch <= '7'));
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
        debug_assert!(self.prev() == 'e' || self.prev() == 'E');

        if self.first() == '+' || self.first() == '-' {
            self.eat();
        }

        self.eat_decimal_digits()
    }

    fn double_quote_string(&mut self) -> TokenKind {
        todo!();
    }

    fn identifier(&mut self) -> TokenKind {
        // The first symbol is already eaten and checked so this must be true.
        debug_assert!(is_id_start(self.prev()));

        self.eat_while(|ch| is_id_continue(ch));

        TokenKind::Ident
    }

    fn raw_double_quote_string(&mut self) -> TokenKind {
        todo!();
    }

    fn raw_identifier(&mut self) -> TokenKind {
        debug_assert!(self.prev() == 'r' && self.first() == '#' && is_id_start(self.second()));

        // eat `#`
        self.eat();
        self.eat_while(|ch| is_id_continue(ch));

        TokenKind::RawIdent
    }

    fn eat_while(&mut self, predicate: impl Fn(char) -> bool) {
        while predicate(self.first()) {
            self.eat();
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

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
                TokenKind::Comment {
                    kind: CommentKind::Block {
                        is_terminated: true,
                    },
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
                TokenKind::Comment {
                    kind: CommentKind::Block {
                        is_terminated: true
                    },
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
                TokenKind::Comment {
                    kind: CommentKind::Block {
                        is_terminated: true
                    },
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
                TokenKind::Comment {
                    kind: CommentKind::Block {
                        is_terminated: true
                    },
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
                TokenKind::Comment {
                    kind: CommentKind::Block {
                        is_terminated: false
                    },
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
            Token::new(
                TokenKind::Comment {
                    kind: CommentKind::Line,
                    doc_style: None,
                },
                18,
            )
        );

        // //! Inner doc comment.
        assert_eq!(cursor.advance_token(), Token::new(TokenKind::Whitespace, 1));
        assert_eq!(
            cursor.advance_token(),
            Token::new(
                TokenKind::Comment {
                    kind: CommentKind::Line,
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
                TokenKind::Comment {
                    kind: CommentKind::Line,
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
                TokenKind::Comment {
                    kind: CommentKind::Line,
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
                TokenKind::Comment {
                    kind: CommentKind::Line,
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
