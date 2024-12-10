mod ast;
mod error_handler;
mod interpreter;
#[path = "keyword.rs"]
mod kw;
mod lexer;
mod parse;
mod session;
mod span_encoding;
mod symbol;
mod visitor;

use std::{env, fs, path::Path, process, sync::Arc};

use session::SourceFile;

use crate::error_handler::*;
use once_cell::sync::Lazy;
use std::sync::atomic::{AtomicBool, Ordering};

// Global flag for Gen Z mode
static GENZ_MODE: Lazy<AtomicBool> = Lazy::new(|| AtomicBool::new(false));

pub fn enable_genz_mode() {
    GENZ_MODE.store(true, Ordering::SeqCst);
}

pub fn is_genz_mode() -> bool {
    GENZ_MODE.load(Ordering::SeqCst)
}

fn main() {
    // collect args
    let args: Vec<String> = env::args().collect();

    if args.len() == 1 {
        help();
        process::exit(1);
    }

    // validate options
    if let Err(e) = validate_options(args.clone()) {
        eprintln!("Error: {}", e);
        process::exit(1);
    }

    // help flag check
    if args.contains(&String::from("--help")) || args.contains(&String::from("-h")) {
        help();
        process::exit(0);
    }

    // version flag check
    if args.contains(&String::from("--version")) || args.contains(&String::from("-v")) {
        const VERSION: &str = env!("CARGO_PKG_VERSION");
        println!("Pandora version {}", VERSION);
        process::exit(0);
    }

    // verbose mode flag check
    let is_verbose = args.contains(&String::from("--verbose"));

    // chaos mode flag check
    let is_genz = args.contains(&String::from("--wreck"));

    let filename = &args[1];

    if is_genz {
        if !filename.ends_with(".unbx") {
            eprintln!("Error: Input file must have a .unbx extension");
            process::exit(1);
        }
    } else {
        if !filename.ends_with(".box") {
            eprintln!("Error: Input file must have a .box extension");
            process::exit(1);
        }
    }

    // file check disk
    if !Path::new(filename).exists() {
        eprintln!("Error: File '{}' not found", filename);
        process::exit(1);
    }

    if is_genz {
        enable_genz_mode();
        println!("UNLEASH THE CHAOS!!!!!!!!");
    }

    // read file
    let contents = fs::read_to_string(filename)
        .map_err(|e| format!("Failed to read file: {e}"))
        .unwrap();

    let contents = Arc::new(contents);

    let source = SourceFile::new(filename, Arc::clone(&contents));
    let file = Arc::new(source);
    let mut session = session::Session::new(Arc::clone(&file));

    // parsing
    let ast = parse::parser::parse(contents.as_str(), &mut session);
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
    println!("  --version, -v      Display version information");
    println!("  --verbose          Enable verbose output");
    println!("  --help, -h         Display this help message");
    println!("  --wreck            Enable Gen Z mode ☠️☠️☠️");
    println!("");
}

fn validate_options(args: Vec<String>) -> Result<(), String> {
    let valid_options = vec![
        "--verbose".to_string(),
        "-v".to_string(),
        "--help".to_string(),
        "-h".to_string(),
        "--version".to_string(),
        "--wreck".to_string(),
    ];

    for arg in args {
        if arg.starts_with("-") && !valid_options.contains(&arg) {
            return Err(format!("invalid option: {}", arg));
        }
    }

    Ok(())
}
