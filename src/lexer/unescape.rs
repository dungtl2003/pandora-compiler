use std::str::Chars;

/// Errors and warnings that can occur during string unescaping. They mostly
/// relate to malformed escape sequences, but there are a few that are about
/// other problems.
#[derive(Debug, PartialEq, Eq)]
pub enum EscapeError {
    /// Expected 1 char, but 0 were found.
    ZeroChars,
    /// Expected 1 char, but more than 1 were found.
    MoreThanOneChar,

    /// Escaped '\' character without continuation.
    LoneSlash,
    /// Invalid escape character (e.g. '\z').
    InvalidEscape,
    /// Raw '\r' encountered.
    BareCarriageReturn,
    /// Unescaped character that was expected to be escaped (e.g. raw '\t').
    EscapeOnlyChar,
}

/// Takes a contents of a char literal (without quotes), and returns an
/// unescaped char or an error.
pub fn unescape_char(src: &str) -> Result<char, EscapeError> {
    let mut chars = src.chars();

    let c = chars.next().ok_or(EscapeError::ZeroChars)?;
    let res = match c {
        '\\' => scan_escape(&mut chars),
        '\n' | '\t' | '\'' => Err(EscapeError::EscapeOnlyChar),
        '\r' => Err(EscapeError::BareCarriageReturn),
        ch => Ok(ch),
    }?;
    if chars.next().is_some() {
        return Err(EscapeError::MoreThanOneChar);
    }
    Ok(res)
}

pub fn unescape_str(src: &str) -> Result<String, EscapeError> {
    let mut chars = src.chars().peekable();
    let mut result = String::new();

    while let Some(c1) = chars.next() {
        match c1 {
            '\"' => return Err(EscapeError::EscapeOnlyChar),
            '\\' => match chars.next().ok_or(EscapeError::LoneSlash)? {
                '\n' | '\t' | '\r' => {}
                '"' => result.push('"'),
                'n' => result.push('\n'),
                'r' => result.push('\r'),
                't' => result.push('\t'),
                '\\' => result.push('\\'),
                '\'' => result.push('\''),
                ch => result.push(ch),
            },
            '\n' => {
                while let Some(c2) = chars.peek() {
                    match c2 {
                        ' ' | '\t' | '\r' => {
                            chars.next();
                        }
                        _ => break,
                    }
                }
            }
            ch => result.push(ch),
        };
    }

    Ok(result)
}

fn scan_escape<T: From<char>>(chars: &mut Chars<'_>) -> Result<T, EscapeError> {
    // Previous character was '\\', unescape what follows.
    let res: char = match chars.next().ok_or(EscapeError::LoneSlash)? {
        '"' => '"',
        'n' => '\n',
        'r' => '\r',
        't' => '\t',
        '\\' => '\\',
        '\'' => '\'',
        _ => return Err(EscapeError::InvalidEscape),
    };
    Ok(T::from(res))
}
