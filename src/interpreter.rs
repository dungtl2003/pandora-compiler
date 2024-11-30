pub mod environment;
pub mod eval;
mod expr;
mod stmt;
mod ty;

use environment::Environment;
use eval::*;
use expr::*;
use stmt::*;
use ty::*;

use crate::{ast::Ast, session::Session};

pub fn interpret(ast: &Ast, session: &Session) -> IResult {
    let mut env = Environment::new();
    for stmt in &ast.stmts {
        interpret_stmt(&mut env, stmt, false)?;
    }
    Ok(EvalResult::StmtResult(None))
}

pub type IError = String;
pub type IResult = Result<EvalResult, IError>;
