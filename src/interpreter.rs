pub mod environment;
mod errors;
pub mod eval;
mod expr;
mod ident;
mod stmt;
mod ty;

use std::process::exit;

use environment::Environment;
use errors::IError;
use eval::*;
use expr::*;
use stmt::*;
use ty::*;

use crate::{ast::Ast, session::Session};

pub fn interpret(ast: &Ast, session: &Session, is_verbose: bool) {
    let mut env = Environment::new();
    for stmt in &ast.stmts {
        if is_verbose {
            println!("\x1b[90m[DEBUG] Interpreting: {:?}\x1b[0m", stmt);
        }
        let result = interpret_stmt(&mut env, stmt, false, is_verbose);
        match result {
            Err(errors) => {
                for error in errors {
                    let report = error.to_report(&session.error_handler);
                    session.error_handler.report_err(report);
                }
                exit(1);
            }
            _ => {}
        }
    }

    exit(0);
}

pub type IResult = Result<EvalResult, Vec<IError>>;
