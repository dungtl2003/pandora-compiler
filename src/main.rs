mod ast;
mod error_handler;
mod interpreter;
#[path = "keyword.rs"]
mod kw;
mod lexer;
mod libs;
mod parse;
mod session;
mod span_encoding;
mod symbol;
mod visitor;

use crate::error_handler::*;
use miette::NamedSource;
use std::fs;
use std::sync::Arc;

fn main() {
    let file_path = "src/example/".to_string();
    let mut arr: [i32; 5] = [0; 5];
    let file_names = vec![
        //"program.box",
        //"var_decl.box",
        //"path.box",
        //"expr.box",
        //"test.box",
        //"if_stmt.box",
        //"while_stmt.box",
        //"function.box",
        //"for_loop.box",
        //"bubble_sort.box",
        "matrix.box",
        //"array.box",
        //"unterminated_block_comment.box",
        //"number_literal_error.box",
        //"unterminated_raw_str.box",
        //"too_many_hashes_raw_str.box",
        //"unterminated_char.box",
        //"unescape_error.box",
        //"if_stmt.box",
        //"fun_item.box",
        //"class_item.box",
    ];

    for file_name in file_names {
        println!("================ BEGIN ================");
        let data = Arc::new(
            fs::read_to_string(file_path.clone() + file_name).expect("unable to read file"),
        );
        let file = Arc::new(NamedSource::new(file_name, data.clone()));
        let session = session::Session::new(file);

        //print_lex_2(&data, &session);
        //print_lex_3(&data, &session);
        //print_parse_stmts(&data, &session);
        interpret_file(&data, &session);

        println!("================ END ================");
    }
}

fn interpret_file(data: &str, session: &session::Session) {
    let tokens = parse::lexer::lex_token_tree(&data, &session).unwrap();
    let ast = parse::parser::parse(tokens, &session).unwrap();
    interpreter::interpret(&ast, &session).unwrap();
}

fn print_lex_2<'sess>(data: &str, session: &'sess session::Session) {
    let tokens = parse::lexer::tokenize(data, session);
    for token in tokens {
        println!("{:?}", token);
    }
}

fn print_lex_3<'sess>(data: &str, session: &'sess session::Session) {
    let tokens = parse::lexer::lex_token_tree(&data, session).unwrap();
    crate::ast::pprint(&tokens);
}

fn print_parse_stmts<'sess>(data: &str, session: &'sess session::Session) {
    let tokens = parse::lexer::lex_token_tree(&data, session).unwrap();
    let ast = parse::parser::parse(tokens, session).unwrap();
    let mut printer = ast::pretty_print::Printer::new();
    printer.print_stmts(&ast.stmts);
    println!("{}", printer.output);
}
