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
        '\r' | '\n' | '\t' | '\'' => Err(EscapeError::EscapeOnlyChar),
        ch => Ok(ch),
    }?;
    if chars.next().is_some() {
        return Err(EscapeError::MoreThanOneChar);
    }
    Ok(res)
}

pub fn unescape_str(src: &str) -> Result<String, EscapeError> {
    let mut chars = src.chars();
    let mut content = String::new();

    while let Some(c) = chars.next() {
        match c {
            '"' => return Err(EscapeError::EscapeOnlyChar),
            '\\' => match chars.clone().next() {
                Some('\n') => {
                    skip_whitespace(&mut chars);
                }
                _ => {
                    let res = scan_escape(&mut chars);
                    match res {
                        Err(e) => return Err(e),
                        Ok(ch) => content.push(ch),
                    }
                }
            },
            ch => content.push(ch),
        };
    }

    Ok(content)
}

fn skip_whitespace(chars: &mut Chars<'_>) {
    while let Some(c) = chars.next() {
        match c {
            c if super::is_whitespace(c) => {}
            _ => return,
        }
    }
}

fn scan_escape<T: From<char>>(chars: &mut Chars<'_>) -> Result<T, EscapeError> {
    // Previous character was '\\', unescape what follows.
    let res: char = match chars.next().ok_or(EscapeError::LoneSlash)? {
        '"' => '"',
        'n' => '\n',
        'r' => '\r',
        't' => '\t',
        '0' => '\0',
        '\\' => '\\',
        '\'' => '\'',
        _ => return Err(EscapeError::InvalidEscape),
    };
    Ok(T::from(res))
}
