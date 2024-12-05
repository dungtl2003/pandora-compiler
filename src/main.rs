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

use std::{env, fs, path::Path, process, sync::Arc};

use session::SourceFile;

use crate::error_handler::*;

fn main() {
    // collect args
    let args: Vec<String> = env::args().collect();

    // verbose mode flag check
    let is_verbose =
        args.contains(&String::from("--verbose")) || args.contains(&String::from("-v"));

    // error display
    if args.len() < 2
        || args.contains(&String::from("help"))
        || args.contains(&String::from("--help"))
        || args.contains(&String::from("-h"))
    {
        help();
        process::exit(1);
    }

    let filename = &args[1];

    if !filename.ends_with(".box") {
        eprintln!("Error: Input file must have a .box extension");
        process::exit(1);
    }

    // file check disk
    if !Path::new(filename).exists() {
        eprintln!("Error: File '{}' not found", filename);
        process::exit(1);
    }

    // read file
    let contents = fs::read_to_string(filename)
        .map_err(|e| format!("Failed to read file: {e}"))
        .unwrap();

    let contents = Arc::new(contents);

    let source = SourceFile::new(filename, Arc::clone(&contents));
    let file = Arc::new(source);
    let session = session::Session::new(Arc::clone(&file));

    // parsing
    let ast = parse::parser::parse(contents.as_str(), &session);
    if ast.is_none() {
        process::exit(1);
    }
    let ast = ast.unwrap();

    // interpret
    interpreter::interpret(&ast, &session, is_verbose);
}

fn help() {
    println!("");
    println!("Pandora Programming Language");
    println!("Made by dungtl2003 and himarawi for school project.");
    println!("");
    println!("Usage: unbox <file.box> [--verbose | -v]");
    println!("Options:");
    println!("  --verbose, -v      Enable verbose output");
    println!("  help, --help, -h   Display this help message");
    println!("");
}
