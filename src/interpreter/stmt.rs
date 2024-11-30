use crate::{
    ast::{Expr, Fun, FunSig, Ident, Local, LocalKind, Stmt, StmtKind},
    kw::Keyword,
};

use super::{
    environment::Environment,
    eval::{ControlFlow, EvalResult},
    interpret_expr, interpret_ty,
    ty::Ty,
    Func, IResult, Value,
};

pub fn interpret_stmt(env: &mut Environment, stmt: &Box<Stmt>, in_loop: bool) -> IResult {
    match &stmt.kind {
        StmtKind::Import(ident) => interpret_stmt_import(env, ident),
        StmtKind::Expr(expr) => interpret_stmt_expr(env, expr, in_loop),
        StmtKind::Var(local) => interpret_stmt_var_decl(env, local, in_loop),
        StmtKind::If(cond, then_block, else_block) => {
            interpret_stmt_if(env, cond, then_block, else_block, in_loop)
        }
        StmtKind::While(cond, block) => interpret_stmt_while(env, cond, block, in_loop),
        StmtKind::Break => interpret_stmt_break(in_loop),
        StmtKind::Continue => interpret_stmt_continue(in_loop),
        StmtKind::Block(stmts) => interpret_stmt_block(env, stmts, in_loop),
        StmtKind::FuncDecl(fun) => interpret_stmt_func_decl(env, fun, in_loop),
        StmtKind::Return(expr) => interpret_stmt_return(env, expr),
        StmtKind::For(ident, iterator, block) => {
            interpret_stmt_for(env, ident, iterator, block, in_loop)
        }
        StmtKind::Empty => Ok(EvalResult::StmtResult(None)),
    }
}

pub fn interpret_stmt_for(
    env: &mut Environment,
    ident: &Ident,
    expr: &Box<Expr>,
    block: &Box<Stmt>,
    in_loop: bool,
) -> IResult {
    // We cannot call interpret_stmt here because we need to push a new scope with the variable
    // declared in the for loop
    let stmts = match &block.kind {
        StmtKind::Block(stmts) => stmts,
        _ => {
            return Err(format!(
                "expected block statement in {:?} statement",
                Keyword::During
            ))
        }
    };

    let value = interpret_expr(env, expr, in_loop, false)?;
    println!("value in for: {:?}", value);
    let value_ty = value.to_ty();
    let values = value.into_iter().map_err(|e| e.to_string())?;

    for value in values {
        env.push_scope();
        env.insert_variable(ident.name.as_str(), Some(value), false, value_ty.clone());

        for stmt in stmts {
            let result = interpret_stmt(env, &stmt, true)?;
            match result {
                EvalResult::StmtResult(Some(control_flow)) => match control_flow {
                    ControlFlow::Break => {
                        env.pop_scope();
                        break;
                    }
                    ControlFlow::Continue => {
                        env.pop_scope();
                        continue;
                    }
                    ControlFlow::Return(value) => {
                        env.pop_scope();

                        if !env.in_function {
                            unreachable!("return statement outside of function");
                        }

                        return Ok(EvalResult::StmtResult(Some(ControlFlow::Return(value))));
                    }
                },
                EvalResult::StmtResult(None) => {}
                EvalResult::Value(_) => unreachable!("statement should not return value"),
            }
        }

        env.pop_scope();
    }

    Ok(EvalResult::StmtResult(None))
}

pub fn interpret_stmt_return(env: &mut Environment, expr: &Option<Box<Expr>>) -> IResult {
    let value = match expr {
        Some(expr) => interpret_expr(env, expr, false, false)?,
        None => Value::Unit,
    };
    Ok(EvalResult::StmtResult(Some(ControlFlow::Return(value))))
}

pub fn interpret_stmt_func_decl(env: &mut Environment, fun: &Box<Fun>, in_loop: bool) -> IResult {
    let Fun { sig, body } = fun.as_ref();
    let FunSig {
        name,
        inputs,
        output,
        span: _,
    } = sig;
    let name = name.name.to_string();

    let mut params = vec![];
    for input in inputs {
        let ty = interpret_ty(env, &input.ty, in_loop, false)?;
        let name = input.ident.name.to_string();
        let is_mut = input.is_mut;
        params.push((name, ty, is_mut));
    }

    let stmts = match &body.kind {
        StmtKind::Block(stmts) => stmts,
        _ => {
            unreachable!("Function body should be a block statement");
        }
    }
    .to_vec();
    let ret_ty = match output {
        Some(ty) => interpret_ty(env, ty, in_loop, false)?,
        None => Ty::Unit,
    };

    let function = Func {
        name: name.clone(),
        args: params,
        stmts,
        ret_ty,
    };
    let func_val = Value::Function(function);

    env.insert_function(name.clone(), func_val)?;
    Ok(EvalResult::StmtResult(None))
}

pub fn interpret_stmt_break(in_loop: bool) -> IResult {
    if !in_loop {
        return Err("break statement outside of loop".to_string());
    }

    Ok(EvalResult::StmtResult(Some(ControlFlow::Break)))
}

pub fn interpret_stmt_continue(in_loop: bool) -> IResult {
    if !in_loop {
        return Err("continue statement outside of loop".to_string());
    }

    Ok(EvalResult::StmtResult(Some(ControlFlow::Continue)))
}

pub fn interpret_stmt_while(
    env: &mut Environment,
    cond: &Box<Expr>,
    block: &Box<Stmt>,
    in_loop: bool,
) -> IResult {
    match block.kind {
        StmtKind::Block(_) => {}
        _ => {
            return Err(format!(
                "expected block statement in {:?} statement",
                Keyword::During
            ))
        }
    }

    while interpret_expr(env, cond, in_loop, false)?.to_bool()? {
        match interpret_stmt(env, block, true)? {
            EvalResult::StmtResult(Some(ControlFlow::Continue)) => {}
            EvalResult::StmtResult(None) => {}
            EvalResult::StmtResult(Some(ControlFlow::Break)) => break,
            EvalResult::StmtResult(Some(ControlFlow::Return(value))) => {
                if !env.in_function {
                    unreachable!("return statement outside of function");
                }
                return Ok(EvalResult::StmtResult(Some(ControlFlow::Return(value))));
            }
            EvalResult::Value(_) => unreachable!("statement should not return value"),
        }
    }
    Ok(EvalResult::StmtResult(None))
}

pub fn interpret_stmt_expr(env: &mut Environment, expr: &Box<Expr>, in_loop: bool) -> IResult {
    interpret_expr(env, expr, in_loop, false)?;
    Ok(EvalResult::StmtResult(None))
}

pub fn interpret_stmt_block(
    env: &mut Environment,
    stmts: &Vec<Box<Stmt>>,
    in_loop: bool,
) -> IResult {
    env.push_scope();
    for stmt in stmts {
        let result = interpret_stmt(env, stmt, in_loop)?;
        match result {
            EvalResult::StmtResult(Some(control_flow)) => match control_flow {
                ControlFlow::Break => {
                    env.pop_scope();
                    if !in_loop {
                        return Err("break statement outside of loop".to_string());
                    }

                    return Ok(EvalResult::StmtResult(Some(ControlFlow::Break)));
                }
                ControlFlow::Continue => {
                    env.pop_scope();
                    if !in_loop {
                        return Err("continue statement outside of loop".to_string());
                    }

                    return Ok(EvalResult::StmtResult(Some(ControlFlow::Continue)));
                }
                ControlFlow::Return(value) => {
                    env.pop_scope();
                    if !env.in_function {
                        return Err("return statement outside of function".to_string());
                    }
                    return Ok(EvalResult::StmtResult(Some(ControlFlow::Return(value))));
                }
            },
            EvalResult::StmtResult(None) => {}
            EvalResult::Value(_) => unreachable!("statement should not return value"),
        }
    }
    env.pop_scope();
    Ok(EvalResult::StmtResult(None))
}

pub fn interpret_stmt_if(
    env: &mut Environment,
    cond: &Box<Expr>,
    then_block: &Box<Stmt>,
    else_block: &Option<Box<Stmt>>,
    in_loop: bool,
) -> IResult {
    match then_block.kind {
        StmtKind::Block(_) => {}
        _ => {
            return Err(format!(
                "expected block statement in {:?} statement",
                Keyword::During
            ))
        }
    }

    if let Some(else_block) = else_block {
        match else_block.kind {
            StmtKind::Block(_) => {}
            _ => {
                return Err(format!(
                    "expected block statement in {:?} statement",
                    Keyword::Alt
                ))
            }
        }
    }

    let cond = interpret_expr(env, cond, in_loop, false)?;
    let cond_ty = cond.to_ty();
    if cond_ty != Ty::Bool {
        return Err(format!("expected boolean expression, found {:?}", cond_ty));
    }

    let cond = cond.to_bool()?;
    if cond {
        interpret_stmt(env, then_block, in_loop)
    } else if let Some(else_block) = else_block {
        interpret_stmt(env, else_block, in_loop)
    } else {
        Ok(EvalResult::StmtResult(None))
    }
}

pub fn interpret_stmt_var_decl(env: &mut Environment, local: &Local, in_loop: bool) -> IResult {
    let Local {
        is_mut,
        ident,
        ty,
        kind,
        span: _,
    } = local;

    let var_ty = interpret_ty(env, ty, in_loop, false)?;
    if !var_ty.can_be_ident_type() {
        return Err(format!(
            "cannot declare variable `{}` of type {}",
            ident.name.as_str(),
            var_ty
        ));
    }

    // If the variable is array, it must have a length if it is not declared with an initializer
    let (value, var_ty) = match kind {
        LocalKind::Init(expr) => {
            let value = interpret_expr(env, expr, false, false)?;
            let value_ty = value.to_ty();
            if value_ty != var_ty {
                return Err(format!(
                    "cannot assign value of type {:?} to variable `{}` of type {:?}",
                    value_ty,
                    ident.name.as_str(),
                    var_ty
                ));
            }

            // Handle array length mismatch
            match (var_ty.clone(), value_ty) {
                (Ty::Array(_var_ty, var_len), Ty::Array(val_ty, val_len)) => {
                    if var_len != -1 && var_len != val_len {
                        return Err(format!(
                            "cannot assign array of length {} to array of length {}",
                            val_len, var_len
                        ));
                    }

                    (Some(value), *val_ty)
                }
                (_, _) => (Some(value), var_ty),
            }
        }
        LocalKind::Decl => match var_ty.clone() {
            Ty::Array(_ty, len) => {
                if len == -1 {
                    return Err("array type must have a length".to_string());
                }

                (None, var_ty)
            }
            _ => (None, var_ty),
        },
    };

    env.insert_variable(ident.name.as_str(), value, *is_mut, var_ty);
    Ok(EvalResult::StmtResult(None))
}

pub fn interpret_stmt_import(env: &mut Environment, ident: &Ident) -> IResult {
    env.import_library(ident.name.as_str())?;
    Ok(EvalResult::StmtResult(None))
}
