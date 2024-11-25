mod ast;
mod error_handler;
mod ir;
#[path = "keyword.rs"]
mod kw;
mod lexer;
mod parse;
mod sem;
mod session_global;
mod span_encoding;
mod symbol;
mod visitor;

use crate::error_handler::*;
use ast::Ast;
use miette::NamedSource;
use std::fs;
use std::sync::Arc;

fn main() {
    let file_path = "src/trash/".to_string();
    let file_names = vec![
        //"program.box",
        //"var_decl.box",
        //"path.box",
        //"expr.box",
        "test.box",
        //"main.box",
        //"unterminated_block_comment.box",
        //"number_literal_error.box",
        //"unterminated_raw_str.box",
        //"too_many_hashes_raw_str.box",
        //"unterminated_char.box",
        //"unescape_error.box",
        //"if_stmt.box",
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
        let session = session_global::SessionGlobal::new();
        //type_check(&data, emitter, &session);

        //print_lex_2(&data, emitter, &session);
        //print_lex_3(&data, emitter, &session);
        //print_parse_path(&data, emitter);
        print_parse_stmts(&data, emitter, &session);
        println!("================ END ================");
    }
}

fn type_check<'sess>(
    data: &str,
    emitter: ErrorHandler,
    session: &'sess session_global::SessionGlobal,
) {
    let tokens = parse::lexer::lex_token_tree(&data, emitter, session).unwrap();
    let mut ast = parse::parser::parse(tokens, session).unwrap();
    let sematic_resolver = sem::SematicResolver::new();
    let context_manager = sematic_resolver.parse(&mut ast).unwrap();
    println!("{}", context_manager);
}

fn print_lex_2<'sess>(
    data: &str,
    emitter: ErrorHandler,
    session: &'sess session_global::SessionGlobal,
) {
    let tokens = parse::lexer::tokenize(data, emitter, session);
    for token in tokens {
        println!("{:?}", token);
    }
}

fn print_lex_3<'sess>(
    data: &str,
    emitter: ErrorHandler,
    session: &'sess session_global::SessionGlobal,
) {
    let tokens = parse::lexer::lex_token_tree(&data, emitter, session).unwrap();
    crate::ast::pprint(&tokens);
}

fn print_parse_stmts<'sess>(
    data: &str,
    emitter: ErrorHandler,
    session: &'sess session_global::SessionGlobal,
) {
    let tokens = parse::lexer::lex_token_tree(&data, emitter, session).unwrap();
    let ast = parse::parser::parse(tokens, session).unwrap();
    let mut printer = ast::pretty_print::Printer::new();
    printer.print_stmts(&ast.stmts);
    println!("{}", printer.output);
}

fn parse_stmts<'sess>(
    data: &str,
    emitter: ErrorHandler,
    session: &'sess session_global::SessionGlobal,
) -> Ast {
    let tokens = parse::lexer::lex_token_tree(&data, emitter, session).unwrap();
    parse::parser::parse(tokens, session).unwrap()
}
