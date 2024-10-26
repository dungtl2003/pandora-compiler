use std::str::Chars;

const EOF_CHAR: char = '\0';

pub struct Cursor<'a> {
    chars: Chars<'a>,
    prev: char,
    len_remaining: usize,
}

impl<'a> Cursor<'a> {
    pub fn new(input: &'a str) -> Cursor<'a> {
        Cursor {
            chars: input.chars(),
            prev: EOF_CHAR,
            len_remaining: input.len(),
        }
    }

    /// Return the last eaten symbol
    pub fn prev(&self) -> char {
        self.prev
    }

    /// Peek the next symbol without consuming it. If the requested position doesn't exist, it will
    /// return EOF_CHAR. But this does not mean actual end of file (maybe the end of current read
    /// chunk for example). It should be checked with `is_eof` method.
    pub fn first(&self) -> char {
        self.chars.clone().next().unwrap_or(EOF_CHAR)
    }

    /// Peek the second symbol from the input stream without consuming it.
    pub fn second(&self) -> char {
        let mut iter = self.chars.clone();
        iter.next();
        iter.next().unwrap_or(EOF_CHAR)
    }

    /// Consume the next symbol from the input stream.
    pub fn eat(&mut self) -> Option<char> {
        let c = self.chars.next()?;
        self.prev = c;
        Some(c)
    }

    /// Check to see if there is any symbols left to consume.
    pub fn is_eof(&self) -> bool {
        self.chars.as_str().is_empty()
    }

    /// Return the amount of already eaten bytes.
    pub fn bytes_eaten(&self) -> u32 {
        (self.len_remaining - self.chars.as_str().len()) as u32
    }

    /// Reset the number of eaten bytes to 0.
    pub fn reset_bytes_eaten(&mut self) {
        self.len_remaining = self.chars.as_str().len();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn basic() {
        let input = "hi🥳";
        let mut cursor = Cursor::new(&input);

        // Not eat any symbol yet.
        assert_eq!(cursor.bytes_eaten(), 0);
        assert_eq!(cursor.is_eof(), false);

        // Peek should not change cursor position
        assert_eq!(cursor.first(), 'h');
        assert_eq!(cursor.second(), 'i');
        assert_eq!(cursor.bytes_eaten(), 0);

        // Eat a symbol
        assert_eq!(cursor.eat(), Some('h'));
        assert_eq!(cursor.bytes_eaten(), 1);
        assert_eq!(cursor.is_eof(), false);

        // Cursor can read any unicode symbol
        assert_eq!(cursor.first(), 'i');
        assert_eq!(cursor.second(), '🥳');

        // Check `bytes_eaten` method behavior
        assert_eq!(cursor.eat(), Some('i'));
        assert_eq!(cursor.bytes_eaten(), 2);
        cursor.reset_bytes_eaten();
        assert_eq!(cursor.bytes_eaten(), 0);

        // Check end of input stream case
        assert_eq!(cursor.eat(), Some('🥳'));
        assert_eq!(cursor.bytes_eaten(), 4); // unicode takes 4 bytes

        assert_eq!(cursor.first(), EOF_CHAR);
        assert_eq!(cursor.second(), EOF_CHAR);
        assert_eq!(cursor.eat(), None);
        assert_eq!(cursor.is_eof(), true);
        assert_eq!(cursor.bytes_eaten(), 4);
    }
}
