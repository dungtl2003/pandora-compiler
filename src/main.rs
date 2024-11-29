mod ast;
mod error_handler;
//mod ir;
mod ir_rust;
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
use miette::NamedSource;
use std::fs;
use std::sync::Arc;
use crate::session_global::SourceFile;

fn main() {
    let file_path = "src/trash/".to_string();
    let file_names = vec![
        //"program.box",
        //"var_decl.box",
        //"path.box",
        //"expr.box",
        //"test.box",
        //"main.box",
        //"unterminated_block_comment.box",
        //"number_literal_error.box",
        //"unterminated_raw_str.box",
        //"too_many_hashes_raw_str.box",
        //"unterminated_char.box",
        //"unescape_error.box",
        //"if_stmt.box",
        "fun_item.box",
        //"class_item.box",
    ];

    for file_name in file_names {
        println!("================ BEGIN ================");
        let data = Arc::new(
            fs::read_to_string(file_path.clone() + file_name).expect("unable to read file"),
        );
        let source = NamedSource::new(file_name, Arc::clone(&data));
        // let emitter = ErrorHandler {
        //     file: Arc::new(source),
        // };
        let session = session_global::SessionGlobal::new(Arc::new(source));

        print_parse_stmts(&data, &session);

        //print_lex_2(&data, emitter, &session);
        //print_lex_3(&data, emitter, &session);
        //print_parse_stmts(&data, emitter, &session);
        //type_check(&data, emitter, &session);
        // generate_rust_ir(&data, emitter, &session);

        //==============
        // let tokens = parse::lexer::lex_token_tree(&data, emitter).unwrap();
        // let mut parser = parse::parser::Parser::new(tokens);
        // // let expr = parser.parse_expr().unwrap();
        // let stmt = parser.parse_stmt().unwrap();
        // let mut printer = ast::pretty_print::Printer::new();
        // printer.print_stmt(&stmt);
        // println!("{}", printer.output);

        //ast::pprint(&tokens);
        // let tokens = lexer::tokenize(&data);
        // for token in tokens.iter() {
        //    println!("{token:?}");
        // }
        //
        // print_parse_items(&data, Arc::new(source), &session);



        // print_lex_3(&data,emitter,&session);
        println!("================ END ================");
    }
}

fn generate_rust_ir<'sess>(
    data: &str,
    session: &'sess session_global::SessionGlobal,
) {
    let tokens = parse::lexer::lex_token_tree(&data, session).unwrap();
    let mut ast = parse::parser::parse(tokens, session).unwrap();
    let sematic_resolver = sem::SematicResolver::new();
    let context_manager = sematic_resolver.parse(&mut ast).unwrap();
    let mut ir_generator = ir_rust::IrRustGenerator::new(&context_manager);
    ir_generator.generate_code(&ast);
}

fn type_check<'sess>(
    data: &str,
    session: &'sess session_global::SessionGlobal,
) {
    let tokens = parse::lexer::lex_token_tree(&data, session).unwrap();
    let mut ast = parse::parser::parse(tokens, session).unwrap();
    let sematic_resolver = sem::SematicResolver::new();
    let context_manager = sematic_resolver.parse(&mut ast).unwrap();
    println!("{}", context_manager);
}

fn print_lex_2<'sess>(
    data: &str,
    session: &'sess session_global::SessionGlobal,
) {
    let tokens = parse::lexer::tokenize(data, session);
    for token in tokens {
        println!("{:?}", token);
    }
}

fn print_lex_3<'sess>(
    data: &str,
    session: &'sess session_global::SessionGlobal,
) {
    let tokens = parse::lexer::lex_token_tree(&data, session).unwrap();
    crate::ast::pprint(&tokens);
}

fn print_parse_stmts<'sess>(
    data: &str,
    session: &'sess session_global::SessionGlobal,
) {
    let tokens = parse::lexer::lex_token_tree(&data, session);
    if tokens.is_err(){
        println!("lexer error");
        return
    } else {
        println!("lexer success");
    }
    let ast = parse::parser::parse(tokens.unwrap(), session).unwrap();
    let mut printer = ast::pretty_print::Printer::new();
    printer.print_stmts(&ast.stmts);
    println!("{}", printer.output);
}

// fn print_parse_items<'sess>(
//     data: &str,
//     file: Arc<SourceFile>,
//     session: &'sess session_global::SessionGlobal,
// ) {
//     let emitter = ErrorHandler {
//         file: file.clone(),
//     };
//     let tokens = parse::lexer::lex_token_tree(&data, emitter, session).unwrap();
//     let emitter = ErrorHandler {
//         file: file.clone(),
//     };
//     let ast = parse::parser::parse(tokens, session).unwrap();
//     let mut printer = ast::pretty_print::Printer::new();
//     printer.print_stmts(&ast.stmts);
//     println!("{}", printer.output);
// }
