mod ast;
mod error_handler;
mod interner;
#[path = "keyword.rs"]
mod kw;
mod lexer;
mod parse;
mod session_global;
mod visitor;

use crate::error_handler::*;
use miette::NamedSource;
use std::fs;
use std::sync::Arc;

fn main() {
    let file_path = "src/trash/".to_string();
    let file_names = vec![
        "unterminated_block_comment.box",
        "number_literal_error.box",
        "unterminated_raw_str.box",
        "too_many_hashes_raw_str.box",
        "unterminated_char.box",
        "unescape_error.box",
    ];

    for file_name in file_names {
        println!("================ BEGIN ================");
        let data = Arc::new(
            fs::read_to_string(file_path.clone() + file_name).expect("unable to read file"),
        );
        let source = NamedSource::new(file_name, Arc::clone(&data));
        let emitter = ErrorEmitter {
            file: Arc::new(source),
        };
        let tokens = parse::lexer::tokenize(&data, emitter);

        for token in tokens.iter() {
            println!("{token:?}");
        }
        println!("================ END ================");
    }
}
