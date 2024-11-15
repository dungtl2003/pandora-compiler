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
        //"path.box",
        "expr.box",
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
        //print_lex_2(&data,emitter);
        //print_lex_3(&data, emitter);
        print_parse_expr(&data, emitter);
        //print_parse_path(&data, emitter);
        println!("================ END ================");
    }
}

fn print_lex_2(data: &str, emitter: ErrorHandler) {
    let tokens = parse::lexer::tokenize(data, emitter);
    for token in tokens {
        println!("{:?}", token);
    }
}

fn print_lex_3(data: &str, emitter: ErrorHandler) {
    let tokens = parse::lexer::lex_token_tree(&data, emitter).unwrap();
    crate::ast::pprint(&tokens);
}

fn print_parse_path(data: &str, emitter: ErrorHandler) {
    let session = session_global::SessionGlobal::new();
    let tokens = parse::lexer::lex_token_tree(&data, emitter).unwrap();
    let mut parser = parse::parser::Parser::new(tokens, session);
    let path = parser.parse_path().unwrap();
    let mut printer = ast::pretty_print::Printer::new();
    printer.print_path(&path);
    println!("{}", printer.output);
}

fn print_parse_expr(data: &str, emitter: ErrorHandler) {
    let session = session_global::SessionGlobal::new();
    let tokens = parse::lexer::lex_token_tree(&data, emitter).unwrap();
    let mut parser = parse::parser::Parser::new(tokens, session);
    let expr = parser.parse_expr().unwrap();
    let mut printer = ast::pretty_print::Printer::new();
    printer.print_expr(&expr);
    println!("{}", printer.output);
}
