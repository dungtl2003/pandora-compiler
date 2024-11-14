mod ast;
mod error_handler;
mod interner;
#[path = "keyword.rs"]
mod kw;
mod lexer;
mod parse;
mod session_global;
mod span_encoding;
mod visitor;

use crate::error_handler::*;
use miette::NamedSource;
use std::fs;
use std::sync::Arc;

fn main() {
    let file_path = "src/trash/".to_string();
    let file_names = vec![
        //"expr.box",
        //"test.box",
        //"main.box",
        //"unterminated_block_comment.box",
        //"number_literal_error.box",
        //"unterminated_raw_str.box",
        //"too_many_hashes_raw_str.box",
        //"unterminated_char.box",
        //"unescape_error.box",
    ];

    for file_name in file_names {
        println!("================ BEGIN ================");
        let data = Arc::new(
            fs::read_to_string(file_path.clone() + file_name).expect("unable to read file"),
        );
        let source = NamedSource::new(file_name, Arc::clone(&data));
        let emitter = ErrorHandler {
            file: Arc::new(source),
        };
        let tokens = parse::lexer::lex_token_tree(&data, emitter).unwrap();
        let mut parser = parse::parser::Parser::new(tokens);
        let expr = parser.parse_expr().unwrap();
        let mut printer = ast::pretty_print::Printer::new();
        printer.print_expr(&expr);
        println!("{}", printer.output);
        //ast::pprint(&tokens);
        //let tokens = lexer::tokenize(&data);
        //for token in tokens.iter() {
        //    println!("{token:?}");
        //}
        //
        println!("================ END ================");
    }
}
