use std::{ops::Range, str::Chars};

/// Errors and warnings that can occur during string unescaping. They mostly
/// relate to malformed escape sequences, but there are a few that are about
/// other problems.
#[derive(Debug, PartialEq, Eq, Clone)]
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

pub enum Mode {
    Char,
    Str,
    RawStr,
}

pub fn unescape_unicode<F>(src: &str, mode: Mode, callback: &mut F)
where
    F: FnMut(Range<usize>, Result<char, EscapeError>),
{
    match mode {
        Mode::Char => {
            let mut chars = src.chars();
            let res = unescape_char(&mut chars);
            callback(0..(src.len() - chars.as_str().len()), res);
        }
        Mode::Str => unescape_str(src, callback),
        Mode::RawStr => {}
    }
}

/// Takes a contents of a char literal (without quotes), and returns an
/// unescaped char or an error.
pub fn unescape_char(chars: &mut Chars<'_>) -> Result<char, EscapeError> {
    let c = chars.next().ok_or(EscapeError::ZeroChars)?;
    let res = match c {
        '\\' => scan_escape(chars),
        '\r' | '\n' | '\t' | '\'' => Err(EscapeError::EscapeOnlyChar),
        ch => Ok(ch),
    }?;
    if chars.next().is_some() {
        return Err(EscapeError::MoreThanOneChar);
    }
    Ok(res)
}

pub fn unescape_str<F>(src: &str, callback: &mut F)
where
    F: FnMut(Range<usize>, Result<char, EscapeError>),
{
    let mut chars = src.chars();

    while let Some(c) = chars.next() {
        let start = src.len() - chars.as_str().len() - c.len_utf8();

        let res = match c {
            '"' => Err(EscapeError::EscapeOnlyChar),
            '\\' => match chars.clone().next() {
                Some('\n') => {
                    skip_whitespace(&mut chars);
                    continue;
                }
                _ => scan_escape(&mut chars),
            },
            ch => Ok(ch),
        };

        let end = src.len() - chars.as_str().len();
        callback(start..end, res);
    }
}

fn skip_whitespace(chars: &mut Chars<'_>) {
    let tail = chars.as_str();
    let first_non_space = tail
        .bytes()
        .position(|b| b != b' ' && b != b'\t' && b != b'\n' && b != b'\r')
        .unwrap_or(tail.len());

    let tail = &tail[first_non_space..];
    *chars = tail.chars();
}

fn scan_escape(chars: &mut Chars<'_>) -> Result<char, EscapeError> {
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
    Ok(res)
}
