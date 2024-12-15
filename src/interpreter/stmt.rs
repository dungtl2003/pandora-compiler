use crate::{
    ast::{self, Expr, Fun, FunSig, Local, LocalKind, Stmt, StmtKind},
    kw::{self, Keyword},
    span_encoding::Span,
};

use super::{
    environment::Environment,
    errors::IError,
    eval::{ControlFlow, EvalResult},
    ident::Ident,
    interpret_expr, interpret_ty,
    ty::TyKind,
    Func, FuncParam, FuncSig, IResult, Ty, Value, ValueKind,
};

pub fn interpret_stmt(
    env: &mut Environment,
    stmt: &Box<Stmt>,
    in_loop: bool,
    is_verbose: bool,
) -> IResult {
    let span = stmt.span.clone();
    match &stmt.kind {
        StmtKind::Import(ident) => interpret_stmt_import(env, ident, is_verbose),
        StmtKind::Expr(expr) => interpret_stmt_expr(env, expr, in_loop, is_verbose),
        StmtKind::Var(local) => interpret_stmt_var_decl(env, local, in_loop, is_verbose),
        StmtKind::If(cond, then_block, else_block) => {
            interpret_stmt_if(env, cond, then_block, else_block, in_loop, is_verbose)
        }
        StmtKind::While(cond, block) => interpret_stmt_while(env, cond, block, in_loop, is_verbose),
        StmtKind::Break => interpret_stmt_break(span, in_loop, is_verbose),
        StmtKind::Continue => interpret_stmt_continue(span, in_loop, is_verbose),
        StmtKind::Block(stmts) => interpret_stmt_block(env, stmts, in_loop, is_verbose),
        StmtKind::FuncDecl(fun) => interpret_stmt_func_decl(env, fun, in_loop, is_verbose),
        StmtKind::Return(expr) => interpret_stmt_return(env, expr, span, is_verbose),
        StmtKind::For(ident, iterator, block) => {
            interpret_stmt_for(env, ident, iterator, block, in_loop, is_verbose)
        }
        StmtKind::Empty => Ok(EvalResult::StmtResult(None)),
    }
}

pub fn interpret_stmt_for(
    env: &mut Environment,
    ident: &ast::Ident,
    expr: &Box<Expr>,
    block: &Box<Stmt>,
    in_loop: bool,
    is_verbose: bool,
) -> IResult {
    if is_verbose {
        println!(
        "\x1b[90m[DEBUG] Interpreting for loop statement with identifier: {:?} --- expression: {:?} --- block: {:?}\x1b[0m",
        ident, expr, block
    );
    }
    // We cannot call interpret_stmt here because we need to push a new scope with the variable
    // declared in the for loop
    let stmts = match &block.kind {
        StmtKind::Block(stmts) => stmts,
        _ => {
            return Err(vec![IError::ExpectedBlock {
                stmt_span: block.span,
            }])
        }
    };

    let value = interpret_expr(env, expr, in_loop, is_verbose)?;
    let value_ty = value.to_ty_kind();
    let values = value.clone().into_iter().map_err(|_| {
        [IError::ExpectedIterator {
            ty: value_ty.to_string(),
            span: value.span,
        }]
    })?;

    let ast::Ident { name, span } = ident;
    let name = name.as_str();
    let ident = Ident {
        name: name.to_string(),
        span: span.clone(),
    };
    let ty = Ty {
        kind: value_ty,
        span: value.span.clone(),
    };

    for value in values {
        env.push_scope();
        env.insert_variable(
            ident.clone(),
            Some(value),
            false,
            ty.clone(),
            Some(span.clone()),
        );

        for stmt in stmts {
            let result = interpret_stmt(env, &stmt, true, is_verbose)?;
            match result {
                EvalResult::StmtResult(Some(control_flow)) => match control_flow {
                    ControlFlow::Break => {
                        env.pop_scope();
                        return Ok(EvalResult::StmtResult(None));
                    }
                    ControlFlow::Continue => {
                        break; // continue to next value
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
            }
        }

        env.pop_scope();
    }

    Ok(EvalResult::StmtResult(None))
}

pub fn interpret_stmt_return(
    env: &mut Environment,
    expr: &Option<Box<Expr>>,
    span: Span,
    is_verbose: bool,
) -> IResult {
    if is_verbose {
        println!(
            "\x1b[90m[DEBUG] Interpreting return statement with expression: {:?}\x1b[0m",
            expr
        );
    }

    if !env.in_function {
        return Err(vec![IError::ReturnOutsideFunction {
            symbol: kw::to_string(Keyword::Yeet),
            span,
        }]);
    }

    let value = match expr {
        Some(expr) => interpret_expr(env, expr, false, is_verbose)?,
        None => Value {
            kind: ValueKind::Unit,
            span: Span::after(span),
        },
    };
    Ok(EvalResult::StmtResult(Some(ControlFlow::Return(value))))
}

pub fn interpret_stmt_func_decl(
    env: &mut Environment,
    fun: &Box<Fun>,
    in_loop: bool,
    is_verbose: bool,
) -> IResult {
    if is_verbose {
        println!(
            "\x1b[90m[DEBUG] Interpreting function declaration with function: {:?}\x1b[0m",
            fun
        );
    }
    let Fun { sig, body } = fun.as_ref();
    let FunSig {
        name,
        inputs,
        output,
        span: sig_span,
    } = sig;

    let mut params: Vec<FuncParam> = vec![];
    for input in inputs {
        let ty = interpret_ty(env, &input.ty, in_loop, is_verbose)?;
        let ident = Ident {
            name: input.ident.name.to_string(),
            span: input.ident.span.clone(),
        };
        let is_mut = input.is_mut;
        let span = input.span;
        params.push(FuncParam {
            ident,
            ty,
            is_mut,
            span,
        });
    }

    let output = match output {
        Some(ty) => Some(interpret_ty(env, ty, in_loop, is_verbose)?),
        None => None,
    };

    let ident = Ident {
        name: name.name.as_str().to_string(),
        span: name.span.clone(),
    };
    let func_sig = FuncSig {
        ident,
        inputs: params,
        output,
        span: sig_span.clone(),
    };

    let function = Func {
        sig: func_sig,
        body: body.clone(),
    };
    let func_val = ValueKind::Function(function);

    env.insert_function(
        name.name.as_str().to_string(),
        func_val,
        name.span.clone(),
        is_verbose,
    )?;
    Ok(EvalResult::StmtResult(None))
}

pub fn interpret_stmt_break(span: Span, in_loop: bool, is_verbose: bool) -> IResult {
    if is_verbose {
        println!("\x1b[90m[DEBUG] Interpreting break statement\x1b[0m");
    }

    if !in_loop {
        return Err(vec![IError::BreakOutsideLoop {
            symbol: kw::to_string(Keyword::Br),
            span,
        }]);
    }

    Ok(EvalResult::StmtResult(Some(ControlFlow::Break)))
}

pub fn interpret_stmt_continue(span: Span, in_loop: bool, is_verbose: bool) -> IResult {
    if is_verbose {
        println!("\x1b[90m[DEBUG] Interpreting continue statement\x1b[0m");
    }

    if !in_loop {
        return Err(vec![IError::ContinueOutsideLoop {
            symbol: kw::to_string(Keyword::Skip),
            span,
        }]);
    }

    Ok(EvalResult::StmtResult(Some(ControlFlow::Continue)))
}

pub fn interpret_stmt_while(
    env: &mut Environment,
    cond: &Box<Expr>,
    block: &Box<Stmt>,
    in_loop: bool,
    is_verbose: bool,
) -> IResult {
    if is_verbose {
        println!(
        "\x1b[90m[DEBUG] Interpreting while statement with condition: {:?} --- block: {:?}\x1b[0m",
        cond, block
    );
    }
    match block.kind {
        StmtKind::Block(_) => {}
        _ => {
            return Err(vec![IError::ExpectedBlock {
                stmt_span: block.span,
            }])
        }
    }

    loop {
        let value = interpret_expr(env, cond, in_loop, is_verbose)?;
        match value.kind {
            ValueKind::Bool(is_true) => {
                if is_true {
                } else {
                    break;
                }
            }
            _ => {
                return Err(vec![IError::MismatchedType {
                    expected: TyKind::Bool.to_string(),
                    found: value.to_ty_kind().to_string(),
                    span: value.span,
                }])
            }
        }

        match interpret_stmt(env, block, true, is_verbose)? {
            EvalResult::StmtResult(Some(ControlFlow::Continue)) => {}
            EvalResult::StmtResult(None) => {}
            EvalResult::StmtResult(Some(ControlFlow::Break)) => break,
            EvalResult::StmtResult(Some(ControlFlow::Return(value))) => {
                if !env.in_function {
                    unreachable!("return statement outside of function");
                }
                return Ok(EvalResult::StmtResult(Some(ControlFlow::Return(value))));
            }
        }
    }
    Ok(EvalResult::StmtResult(None))
}

pub fn interpret_stmt_expr(
    env: &mut Environment,
    expr: &Box<Expr>,
    in_loop: bool,
    is_verbose: bool,
) -> IResult {
    if is_verbose {
        println!(
            "\x1b[90m[DEBUG] Interpreting expression statement with expression: {:?}\x1b[0m",
            expr
        );
    }
    interpret_expr(env, expr, in_loop, is_verbose)?;
    Ok(EvalResult::StmtResult(None))
}

pub fn interpret_stmt_block(
    env: &mut Environment,
    stmts: &Vec<Box<Stmt>>,
    in_loop: bool,
    is_verbose: bool,
) -> IResult {
    if is_verbose {
        println!(
            "\x1b[90m[DEBUG] Interpreting block statement with statements: {:?}\x1b[0m",
            stmts
        );
    }

    env.push_scope();
    for stmt in stmts {
        let result = interpret_stmt(env, stmt, in_loop, is_verbose)?;
        match result {
            EvalResult::StmtResult(Some(control_flow)) => match control_flow {
                ControlFlow::Break => {
                    env.pop_scope();
                    if !in_loop {
                        unreachable!("break statement outside of loop");
                    }

                    return Ok(EvalResult::StmtResult(Some(ControlFlow::Break)));
                }
                ControlFlow::Continue => {
                    env.pop_scope();
                    if !in_loop {
                        unreachable!("continue statement outside of loop");
                    }

                    return Ok(EvalResult::StmtResult(Some(ControlFlow::Continue)));
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
    is_verbose: bool,
) -> IResult {
    if is_verbose {
        println!("\x1b[90m[DEBUG] Interpreting if statement with condition: {:?} --- then block: {:?} --- else block: {:?}\x1b[0m", cond, then_block, else_block);
    }

    let cond_result = interpret_expr(env, cond, in_loop, is_verbose);

    match then_block.kind {
        StmtKind::Block(_) => {}
        _ => {
            if cond_result.is_err() {
                return Err(cond_result.unwrap_err());
            }

            return Err(vec![IError::ExpectedBlockAfterCondition {
                condition_span: cond_result.unwrap().span,
                if_symbol: kw::to_string(Keyword::When),
                stmt_span: then_block.span,
            }]);
        }
    }

    if let Some(else_block) = else_block {
        match else_block.kind {
            StmtKind::Block(_) => {}
            StmtKind::If(_, _, _) => {}
            _ => {
                return Err(vec![IError::ExpectedBlock {
                    stmt_span: else_block.span,
                }])
            }
        }
    }

    if cond_result.is_err() {
        return Err(cond_result.unwrap_err());
    }

    let cond = cond_result.clone().unwrap();
    let is_true = match cond.kind {
        ValueKind::Bool(bool) => bool,
        _ => {
            return Err(vec![IError::MismatchedType {
                expected: TyKind::Bool.to_string(),
                found: cond.to_ty_kind().to_string(),
                span: cond.span,
            }])
        }
    };

    if is_true {
        interpret_stmt(env, then_block, in_loop, is_verbose)
    } else {
        match else_block {
            Some(else_block) => interpret_stmt(env, else_block, in_loop, is_verbose),
            None => Ok(EvalResult::StmtResult(None)),
        }
    }
}

pub fn interpret_stmt_var_decl(
    env: &mut Environment,
    local: &Local,
    in_loop: bool,
    is_verbose: bool,
) -> IResult {
    if is_verbose {
        println!(
            "\x1b[90m[DEBUG] Interpreting variable declaration statement with local: {:?}\x1b[0m",
            local
        );
    }
    let Local {
        is_mut,
        ident,
        ty,
        kind,
        span: _,
    } = local;

    let decl_ty = interpret_ty(env, ty, in_loop, is_verbose)?;

    // If the variable is array, it must have a length if it is not declared with an initializer
    let (value, var_ty_kind, first_assigned_span) = match kind {
        LocalKind::Init(expr) => {
            let value = interpret_expr(env, expr, false, is_verbose)?;
            let value_ty = value.to_ty_kind();
            if value_ty != decl_ty.kind {
                return Err(vec![IError::MismatchedType {
                    expected: decl_ty.to_string(),
                    found: value_ty.to_string(),
                    span: value.span,
                }]);
            }

            // Handle array length mismatch
            let decl_ty_span = decl_ty.span.clone();
            match (decl_ty.clone().kind, value_ty) {
                (TyKind::Array(_var_ty, var_len), TyKind::Array(val_ty, val_len)) => {
                    if var_len != -1 && var_len != val_len {
                        return Err(vec![IError::MismatchArrayTypeLength {
                            expected_len: var_len,
                            found_len: val_len,
                            value_span: value.span,
                            ty_span: decl_ty_span,
                        }]);
                    }

                    (
                        Some(value),
                        TyKind::Array(val_ty, val_len),
                        Some(ident.span.clone()),
                    )
                }
                (_, _) => (Some(value), decl_ty.kind, Some(ident.span.clone())),
            }
        }
        LocalKind::Decl => match decl_ty.kind {
            TyKind::Array(ref _ty, len) => {
                if len == -1 {
                    return Err(vec![IError::UnknownSizeArray {
                        ty: decl_ty.to_string(),
                        span: ty.span,
                    }]);
                }

                (None, decl_ty.kind, None)
            }
            _ => (None, decl_ty.kind, None),
        },
    };

    let decl_ty = Ty {
        kind: var_ty_kind,
        span: decl_ty.span.clone(),
    };
    let ident = Ident {
        name: ident.name.to_string(),
        span: ident.span.clone(),
    };
    env.insert_variable(ident, value, *is_mut, decl_ty, first_assigned_span);
    Ok(EvalResult::StmtResult(None))
}

pub fn interpret_stmt_import(
    env: &mut Environment,
    ident: &ast::Ident,
    is_verbose: bool,
) -> IResult {
    if is_verbose {
        println!(
            "\x1b[90m[DEBUG] Interpreting import statement with identifier: {:?}\x1b[0m",
            ident
        );
    }
    env.import_library(ident.name.as_str(), ident.span, is_verbose)?;
    Ok(EvalResult::StmtResult(None))
}
