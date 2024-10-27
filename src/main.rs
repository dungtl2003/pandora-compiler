mod error_handler;
#[path = "keyword.rs"]
mod kw;
mod lexer;
mod token;

use std::fs;

fn main() {
    let data = fs::read_to_string("src/dummy_code.box").expect("unable to read file");
    let tokens = lexer::tokenize(&data);

    for token in tokens.iter() {
        println!("{token:?}");
    }
}
