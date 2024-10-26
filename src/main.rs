mod error_handler;
#[path = "keyword.rs"]
mod kw;
mod lexer;
mod token;

fn main() {
    // hell
    let s = r###"abcd\432134146{}"##\/^;"""""""""""""""###;
    let r#true = "";
    let x = 0xA_________E5;
    let y = 2E_______________1;
    let x = 2.1_E5;
    let x = 0xA__a_;
    let n = 1_000_000E+3;
    let x = 123.1_;

    //🥰🫢👻
    let s = "🥰🫢👻";
    let mut cs = s.chars();

    assert_eq!(
        emojis::get(cs.next().unwrap().to_string().as_str()).unwrap(),
        "🥰"
    );

    //assert_eq!(emojis::get("1").unwrap(), "1");
}

fn is_valid_emoji(c: char) -> bool {
    //!!! hello
    // Ensure the character is a valid emoji with an emoji presentation.
    true
}

/// Whitespace(4, 0, 3)
/// Comment(8, 4, 11)
/// Whitespace(4, 12, 15)
/// Kw(3, 16, 18)
/// Whitespace(1, 19, 19)
/// Ident(1, 20, 20)
/// Whitespace(1, 21, 21)
/// Assign(1, 22, 22)
/// Whitespace(1, 23, 23)
/// Literal()
fn fuck() {}
