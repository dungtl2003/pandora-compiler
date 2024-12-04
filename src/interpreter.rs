pub mod environment;
pub mod eval;
mod expr;
mod stmt;
mod ty;

use std::process::exit;

use environment::Environment;
use eval::*;
use expr::*;
use stmt::*;
use ty::*;

use crate::{ast::Ast, session::Session};

pub fn interpret(ast: &Ast, session: &Session) {
    let mut env = Environment::new();
    for stmt in &ast.stmts {
        if interpret_stmt(&mut env, stmt, false).is_err() {
            println!("Error: Failed to interpret statement");
            exit(1);
        }
    }

    exit(0);
}

pub type IError = String;
pub type IResult = Result<EvalResult, IError>;
