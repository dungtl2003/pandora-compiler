mod ast;
mod error_docs;
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

use std::{
    env, fs,
    path::Path,
    process::{self, Command, Stdio},
    sync::Arc,
};

use session::SourceFile;

use crate::error_handler::*;
use once_cell::sync::Lazy;
use std::io::Write;
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
        // no args
        help();
        process::exit(1);
    }

    // validate options
    validate_options(&args);

    // help flag check
    try_help(&args);

    // version flag check
    try_version(&args);

    // latest version check
    try_latest(&args);

    // explain flag check
    try_explain(&args);

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

fn try_latest(args: &Vec<String>) {
    if !args.contains(&String::from("--latest")) {
        return;
    }

    if args.len() != 2 {
        eprintln!("Error: latest flag must be used alone");
        process::exit(1);
    }

    let current_version = env!("CARGO_PKG_VERSION");
    let result = check_latest::check_max!();

    match result {
        Ok(Some(latest_version)) => {
            println!(
                "New version available: {} (current: {}), run `cargo install pandora` to update",
                latest_version, current_version
            );
            process::exit(0);
        }
        Ok(None) => {
            println!("You are using the latest version of Pandora.");
            process::exit(0);
        }
        Err(e) => {
            eprintln!("Failed to check for updates: {}", e);
            process::exit(1);
        }
    }
}

fn try_explain(args: &Vec<String>) {
    if !args.contains(&String::from("--explain")) {
        return;
    }

    if args.len() != 3 {
        eprintln!("Error: explain flag must be used with an error code");
        process::exit(1);
    }

    let code = &args[2];

    let docs = error_docs::get_error_docs();
    if !docs.contains_key(code.as_str()) {
        eprintln!("Error: Invalid error code '{}'", code);
        process::exit(1);
    }

    let content = docs.get(code.as_str()).unwrap();

    let result = if cfg!(windows) {
        // Use the Windows `more` command
        Command::new("cmd")
            .arg("/C") // Run a command
            .arg("more") // Use `more` to display paginated output
            .stdin(Stdio::piped()) // We'll write to `more` via its stdin
            .spawn()
    } else if cfg!(target_os = "linux") {
        // Spawn the `less` command
        Command::new("less")
            .stdin(Stdio::piped()) // We'll write to `less` via its stdin
            .spawn()
    } else {
        println!("{}", content);
        process::exit(0);
    };

    match result {
        Ok(mut child) => {
            // Write the content to the child process
            let result = child.stdin.as_mut().unwrap().write_all(content.as_bytes());

            match result {
                Ok(_) => {
                    // Wait for the child to finish
                    let status = child.wait();

                    match status {
                        Ok(_) => {}
                        Err(e) => {
                            eprintln!(
                                "Error: Failed to wait for pager: {}, print normally instead.",
                                e
                            );
                            println!("{}", content);
                        }
                    }
                }
                Err(e) => {
                    eprintln!(
                        "Error: Failed to write to pager: {}, print normally instead.",
                        e
                    );
                }
            }
        }
        Err(e) => {
            eprintln!(
                "Error: Failed to spawn pager: {}, print normally instead.",
                e
            );
            println!("{}", content);
        }
    }

    process::exit(0);
}

fn try_version(args: &Vec<String>) {
    if !(args.contains(&String::from("--version")) || args.contains(&String::from("-v"))) {
        return;
    }

    if args.len() != 2 {
        eprintln!("Error: version flag must be used alone");
        process::exit(1);
    }

    const VERSION: &str = env!("CARGO_PKG_VERSION");
    println!("Pandora version {}", VERSION);
    process::exit(0);
}

fn try_help(args: &Vec<String>) {
    if !(args.contains(&String::from("--help")) || args.contains(&String::from("-h"))) {
        return;
    }

    if args.len() != 2 {
        eprintln!("Error: help flag must be used alone");
        process::exit(1);
    }

    help();
    process::exit(0);
}

fn help() {
    println!("");
    println!("Pandora Programming Language");
    println!("Made by dungtl2003 and himarawi for school project.");
    println!("");
    println!("Usage: unbox <file.box> [--verbose | -v]");
    println!("Options:");
    println!("  --version, -v      Display version information");
    println!("  --latest           Check for the latest version");
    println!("  --verbose          Enable verbose output");
    println!("  --help, -h         Display this help message");
    println!("  --wreck            Enable Gen Z mode ☠️☠️☠️");
    println!("  --explain [code]   Explain the error code");
    println!("");
}

fn validate_options(args: &Vec<String>) {
    let valid_options = vec![
        "--verbose".to_string(),
        "--help".to_string(),
        "-h".to_string(),
        "--version".to_string(),
        "-v".to_string(),
        "--wreck".to_string(),
        "--explain".to_string(),
        "--latest".to_string(),
    ];

    for arg in args {
        if arg.starts_with("-") && !valid_options.contains(&arg) {
            eprintln!("Error: Invalid option '{}'", arg);
            process::exit(1);
        }
    }
}
