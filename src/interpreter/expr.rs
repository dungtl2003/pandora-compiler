use crate::{
    ast::{self, BinOp, BinOpKind, Expr, ExprKind, Lit, LitKind},
    span_encoding::Span,
};

use super::{environment::Environment, eval::ValueKind, interpret_ty, ty::TyKind, IError, Value};

pub fn interpret_expr(
    env: &mut Environment,
    expr: &Box<Expr>,
    in_loop: bool,
    prev_comparison_span: Option<Span>,
    is_verbose: bool,
) -> Result<Value, Vec<IError>> {
    let expr_span = expr.span;
    let ty = match &expr.kind {
        ExprKind::Binary(op, lhs, rhs) => {
            interpret_expr_binary(env, op, lhs, rhs, in_loop, prev_comparison_span, is_verbose)?
        }
        ExprKind::Identifier(ident) => interpret_expr_ident(env, ident)?,
        ExprKind::Literal(value) => interpret_expr_literal(value)?,
        ExprKind::FunCall(prefix, args) => {
            interpret_expr_funcall(env, prefix, args, in_loop, is_verbose)?
        }
        ExprKind::LibFunCall(lib_fun, args) => {
            interpret_expr_lib_funcall(env, lib_fun, args, in_loop, is_verbose)?
        }
        ExprKind::Cast(expr, ty) => interpret_expr_cast(env, expr, ty, in_loop, is_verbose)?,
        ExprKind::Assign(lhs, rhs, assign_span) => {
            interpret_expr_assign(env, lhs, rhs, *assign_span, in_loop, expr_span, is_verbose)?
        }
        ExprKind::Unary(unop, expr) => interpret_expr_unary(env, unop, expr, in_loop, is_verbose)?,
        ExprKind::AssignOp(binop, lhs, rhs) => {
            interpret_expr_assign_op(env, lhs, binop, rhs, in_loop, expr_span, is_verbose)?
        }
        ExprKind::LibAccess(lib, ident) => {
            interpret_expr_lib_access(env, lib, ident, in_loop, is_verbose)?
        }
        ExprKind::Array(elements) => interpret_expr_array(env, elements, in_loop, is_verbose)?,
        ExprKind::Index(array, index, _) => {
            interpret_expr_index(env, array, index, in_loop, is_verbose)?
        }
        ExprKind::Repeat(element, count) => {
            interpret_expr_repeat(env, element, count, in_loop, is_verbose)?
        }
    };

    Ok(Value {
        kind: ty,
        span: expr.span,
    })
}

fn interpret_expr_repeat(
    env: &mut Environment,
    element: &Box<Expr>,
    count: &Box<Expr>,
    in_loop: bool,
    is_verbose: bool,
) -> Result<ValueKind, Vec<IError>> {
    let e = interpret_expr(env, element, in_loop, None, is_verbose)?;
    let c = interpret_expr(env, count, in_loop, None, is_verbose)?;

    match c.kind {
        ValueKind::Int(c) => {
            if c < 0 {
                return Err(vec![IError::NegRepeatCount {
                    count: c,
                    span: count.span,
                }]);
            }
            Ok(ValueKind::Array(vec![e; c as usize]))
        }
        _ => Err(vec![IError::MismatchedType {
            expected: TyKind::Int.to_string(),
            found: c.to_ty_kind().to_string(),
            span: count.span,
        }]),
    }
}

fn interpret_expr_index(
    env: &mut Environment,
    array: &Box<Expr>,
    index: &Box<Expr>,
    in_loop: bool,
    is_verbose: bool,
) -> Result<ValueKind, Vec<IError>> {
    let a = interpret_expr(env, array, in_loop, None, is_verbose)?;
    let i = interpret_expr(env, index, in_loop, None, is_verbose)?;

    match a.kind {
        ValueKind::Array(elements) => match i.kind {
            ValueKind::Int(i) => {
                if i < 0 || i as usize >= elements.len() {
                    return Err(vec![IError::IndexOutOfBounds {
                        len: elements.len() as i64,
                        index: i,
                        span: index.span,
                    }]);
                }
                Ok(elements[i as usize].clone().kind)
            }
            _ => Err(vec![IError::MismatchedType {
                expected: TyKind::Int.to_string(),
                found: i.to_ty_kind().to_string(),
                span: index.span,
            }]),
        },
        _ => Err(vec![IError::IndexingWrongType {
            ty: a.to_ty_kind().to_string(),
            span: array.span,
        }]),
    }
}

fn interpret_expr_array(
    env: &mut Environment,
    elements: &Vec<Box<Expr>>,
    in_loop: bool,
    is_verbose: bool,
) -> Result<ValueKind, Vec<IError>> {
    let mut result = Vec::new();
    let mut ty: Option<TyKind> = None;
    for element in elements {
        let el = interpret_expr(env, element, in_loop, None, is_verbose)?;
        match ty {
            Some(ref ty) => {
                if *ty != el.to_ty_kind() {
                    return Err(vec![IError::MismatchedType {
                        expected: ty.to_string(),
                        found: el.to_ty_kind().to_string(),
                        span: element.span,
                    }]);
                }
            }
            None => {
                ty = Some(el.to_ty_kind());
            }
        }
        result.push(el);
    }

    Ok(ValueKind::Array(result))
}

pub fn interpret_expr_lib_access(
    env: &mut Environment,
    lib: &Box<Expr>,
    ident: &ast::Ident,
    in_loop: bool,
    is_verbose: bool,
) -> Result<ValueKind, Vec<IError>> {
    let lib = interpret_expr(env, lib, in_loop, None, is_verbose)?;
    let lib_name = match lib.kind {
        ValueKind::Str(val) => val,
        _ => return Err(vec![IError::InvalidLibraryName { span: lib.span }]),
    };

    let result = env.lookup_library(&lib_name);
    if result.is_none() {
        return Err(vec![IError::LibraryNotFound {
            library: lib_name,
            span: lib.span,
        }]);
    }

    let lib = &result.unwrap().1;
    let ast::Ident {
        name: func_name,
        span: func_span,
    } = ident;
    let func_name = func_name.as_str();
    let func = lib.get_function(func_name);
    if func.is_none() {
        return Err(vec![IError::FunctionInLibraryNotFound {
            func_name: func_name.to_string(),
            lib_name,
            span: func_span.clone(),
        }]);
    }

    Ok(ValueKind::Unit)
}

pub fn interpret_expr_unary(
    env: &mut Environment,
    op: &ast::UnOp,
    expr: &Box<Expr>,
    in_loop: bool,
    is_verbose: bool,
) -> Result<ValueKind, Vec<IError>> {
    let e = interpret_expr(env, expr, in_loop, None, is_verbose)?;
    match op {
        ast::UnOp::Ne => match e.kind {
            ValueKind::Int(val) => Ok(ValueKind::Int(-val)),
            ValueKind::Float(val) => Ok(ValueKind::Float(-val)),
            _ => Err(vec![IError::CannotApplyUnaryOp {
                op: op.to_string(),
                ty: e.to_ty_kind().to_string(),
                span: expr.span,
            }]),
        },
        ast::UnOp::Not => match e.kind {
            ValueKind::Bool(val) => Ok(ValueKind::Bool(!val)),
            _ => Err(vec![IError::CannotApplyUnaryOp {
                op: op.to_string(),
                ty: e.to_ty_kind().to_string(),
                span: expr.span,
            }]),
        },
    }
}

fn interpret_expr_cast(
    env: &mut Environment,
    expr: &Box<Expr>,
    ty: &ast::Ty,
    in_loop: bool,
    is_verbose: bool,
) -> Result<ValueKind, Vec<IError>> {
    let e = interpret_expr(env, expr, in_loop, None, is_verbose)?;
    let ty = interpret_ty(env, ty, in_loop, None, is_verbose)?;
    match e.try_cast_to(&ty.kind) {
        Ok(val) => Ok(val.kind),
        Err((from, to)) => Err(vec![IError::CannotCast {
            from,
            to,
            span: expr.span,
        }]),
    }
}

fn interpret_expr_assign_op(
    env: &mut Environment,
    lhs: &Box<Expr>,
    op: &BinOp,
    rhs: &Box<Expr>,
    in_loop: bool,
    expr_span: Span,
    is_verbose: bool,
) -> Result<ValueKind, Vec<IError>> {
    let binary_expr = ast::Expr {
        kind: ast::ExprKind::Binary(op.clone(), lhs.clone(), rhs.clone()),
        span: expr_span,
    };
    let result = interpret_expr(env, &Box::new(binary_expr), in_loop, None, is_verbose)?;
    interpret_expr_assign_with_known_value(env, lhs, result, in_loop, expr_span, is_verbose)
}

fn interpret_expr_assign(
    env: &mut Environment,
    lhs: &Box<Expr>,
    rhs: &Box<Expr>,
    assign_span: Span,
    in_loop: bool,
    expr_span: Span,
    is_verbose: bool,
) -> Result<ValueKind, Vec<IError>> {
    let rhs = interpret_expr(env, rhs, in_loop, None, is_verbose)?;
    let Value { kind, span } = rhs;
    match &lhs.kind {
        ExprKind::Identifier(ident) => interpret_expr_assign_ident_with_known_value(
            env, ident, kind, span, expr_span, is_verbose,
        ),
        ExprKind::Index(arr, idx, _) => interpret_expr_assign_array_index_with_known_value(
            env, arr, idx, kind, in_loop, is_verbose,
        ),
        _ => Err(vec![IError::InvalidLhsAssign {
            assign_span,
            lhs_span: lhs.span,
        }]),
    }
}

fn interpret_expr_lib_funcall(
    env: &mut Environment,
    lib_fun: &Box<Expr>,
    args: &Vec<Box<Expr>>,
    in_loop: bool,
    is_verbose: bool,
) -> Result<ValueKind, Vec<IError>> {
    let (lib, func) = match &lib_fun.kind {
        ExprKind::LibAccess(prefix, func) => match &prefix.kind {
            ExprKind::Identifier(lib) => (lib, func),
            _ => return Err(vec![IError::InvalidLibraryPath { span: lib_fun.span }]),
        },
        _ => unreachable!("Library function call prefix must be a library access"),
    };

    // Fix: not handle mutable arguments
    let evaluated_args = {
        let mut args_vec = Vec::new();
        for arg in args {
            args_vec.push((interpret_expr(env, arg, in_loop, None, is_verbose)?, true));
        }
        args_vec
    };

    let ast::Ident {
        name: lib_name,
        span: lib_span,
    } = lib;
    let ast::Ident {
        name: func_name,
        span: func_span,
    } = func;
    let lib_name = lib_name.as_str();
    let func_name = func_name.as_str();

    if let Some((_span, lib)) = env.lookup_library(lib_name) {
        if let Some(func) = lib.get_function(func_name) {
            func(evaluated_args).map_err(|_err| vec![]) // FIX
        } else {
            return Err(vec![IError::FunctionInLibraryNotFound {
                func_name: func_name.to_string(),
                lib_name: lib_name.to_string(),
                span: func_span.clone(),
            }]);
        }
    } else {
        return Err(vec![IError::LibraryNotFound {
            library: lib_name.to_string(),
            span: lib_span.clone(),
        }]);
    }
}

fn interpret_expr_binary(
    env: &mut Environment,
    binop: &BinOp,
    lhs: &Box<Expr>,
    rhs: &Box<Expr>,
    in_loop: bool,
    prev_comparation_span: Option<Span>,
    is_verbose: bool,
) -> Result<ValueKind, Vec<IError>> {
    let lhs = interpret_expr(env, lhs, in_loop, prev_comparation_span, is_verbose)?;
    let rhs = interpret_expr(env, rhs, in_loop, prev_comparation_span, is_verbose)?;

    let lhs_ty = lhs.to_ty_kind();
    let rhs_ty = rhs.to_ty_kind();

    match &binop.node {
        BinOpKind::Add => match (lhs.kind, rhs.kind) {
            (ValueKind::Int(lhs), ValueKind::Int(rhs)) => Ok(ValueKind::Int(lhs + rhs)),
            (ValueKind::Float(lhs), ValueKind::Float(rhs)) => Ok(ValueKind::Float(lhs + rhs)),
            (ValueKind::Str(lhs), ValueKind::Str(rhs)) => {
                Ok(ValueKind::Str(format!("{}{}", lhs, rhs)))
            }
            _ => {
                return Err(vec![IError::CannotAdd {
                    lhs_ty: lhs_ty.to_string(),
                    rhs_ty: rhs_ty.to_string(),
                    op_span: binop.span,
                }])
            }
        },
        BinOpKind::Sub => match (lhs.kind, rhs.kind) {
            (ValueKind::Int(lhs), ValueKind::Int(rhs)) => Ok(ValueKind::Int(lhs - rhs)),
            (ValueKind::Float(lhs), ValueKind::Float(rhs)) => Ok(ValueKind::Float(lhs - rhs)),
            _ => {
                return Err(vec![IError::CannotSubtract {
                    lhs_ty: lhs_ty.to_string(),
                    rhs_ty: rhs_ty.to_string(),
                    op_span: binop.span,
                }])
            }
        },
        BinOpKind::Mul => match (lhs.kind, rhs.kind) {
            (ValueKind::Int(lhs), ValueKind::Int(rhs)) => Ok(ValueKind::Int(lhs * rhs)),
            (ValueKind::Float(lhs), ValueKind::Float(rhs)) => Ok(ValueKind::Float(lhs * rhs)),
            _ => {
                return Err(vec![IError::CannotMultiply {
                    lhs_ty: lhs_ty.to_string(),
                    rhs_ty: rhs_ty.to_string(),
                    op_span: binop.span,
                }])
            }
        },
        BinOpKind::Div => match (lhs.kind, rhs.kind) {
            (ValueKind::Int(lhs), ValueKind::Int(rhs)) => Ok(ValueKind::Int(lhs / rhs)),
            (ValueKind::Float(lhs), ValueKind::Float(rhs)) => Ok(ValueKind::Float(lhs / rhs)),
            _ => {
                return Err(vec![IError::CannotDivide {
                    lhs_ty: lhs_ty.to_string(),
                    rhs_ty: rhs_ty.to_string(),
                    op_span: binop.span,
                }])
            }
        },
        BinOpKind::Mod => match (lhs.kind, rhs.kind) {
            (ValueKind::Int(lhs), ValueKind::Int(rhs)) => Ok(ValueKind::Int(lhs % rhs)),
            (ValueKind::Float(lhs), ValueKind::Float(rhs)) => Ok(ValueKind::Float(lhs % rhs)),
            _ => {
                return Err(vec![IError::CannotModulo {
                    lhs_ty: lhs_ty.to_string(),
                    rhs_ty: rhs_ty.to_string(),
                    op_span: binop.span,
                }])
            }
        },
        BinOpKind::Eq => {
            if prev_comparation_span.is_some() {
                return Err(vec![IError::ComparisonOperatorsCannotBeChained {
                    chain_op_span: vec![prev_comparation_span.unwrap(), binop.span],
                }]);
            }
            match (lhs.kind, rhs.kind) {
                (ValueKind::Int(lhs), ValueKind::Int(rhs)) => Ok(ValueKind::Bool(lhs == rhs)),
                (ValueKind::Float(lhs), ValueKind::Float(rhs)) => Ok(ValueKind::Bool(lhs == rhs)),
                (ValueKind::Str(lhs), ValueKind::Str(rhs)) => Ok(ValueKind::Bool(lhs == rhs)),
                (ValueKind::Bool(lhs), ValueKind::Bool(rhs)) => Ok(ValueKind::Bool(lhs == rhs)),
                (ValueKind::Char(lhs), ValueKind::Char(rhs)) => Ok(ValueKind::Bool(lhs == rhs)),
                _ => Err(vec![IError::CannotCompare {
                    lhs_ty: lhs_ty.to_string(),
                    rhs_ty: rhs_ty.to_string(),
                    op: binop.to_string(),
                    op_span: binop.span,
                }]),
            }
        }
        BinOpKind::Ne => {
            if prev_comparation_span.is_some() {
                return Err(vec![IError::ComparisonOperatorsCannotBeChained {
                    chain_op_span: vec![prev_comparation_span.unwrap(), binop.span],
                }]);
            }
            match (lhs.kind, rhs.kind) {
                (ValueKind::Int(lhs), ValueKind::Int(rhs)) => Ok(ValueKind::Bool(lhs != rhs)),
                (ValueKind::Float(lhs), ValueKind::Float(rhs)) => Ok(ValueKind::Bool(lhs != rhs)),
                (ValueKind::Str(lhs), ValueKind::Str(rhs)) => Ok(ValueKind::Bool(lhs != rhs)),
                (ValueKind::Bool(lhs), ValueKind::Bool(rhs)) => Ok(ValueKind::Bool(lhs != rhs)),
                (ValueKind::Char(lhs), ValueKind::Char(rhs)) => Ok(ValueKind::Bool(lhs != rhs)),
                _ => Err(vec![IError::CannotCompare {
                    lhs_ty: lhs_ty.to_string(),
                    rhs_ty: rhs_ty.to_string(),
                    op: binop.to_string(),
                    op_span: binop.span,
                }]),
            }
        }
        BinOpKind::Lt => {
            if prev_comparation_span.is_some() {
                return Err(vec![IError::ComparisonOperatorsCannotBeChained {
                    chain_op_span: vec![prev_comparation_span.unwrap(), binop.span],
                }]);
            }
            match (lhs.kind, rhs.kind) {
                (ValueKind::Int(lhs), ValueKind::Int(rhs)) => Ok(ValueKind::Bool(lhs < rhs)),
                (ValueKind::Float(lhs), ValueKind::Float(rhs)) => Ok(ValueKind::Bool(lhs < rhs)),
                (ValueKind::Str(lhs), ValueKind::Str(rhs)) => Ok(ValueKind::Bool(lhs < rhs)),
                (ValueKind::Char(lhs), ValueKind::Char(rhs)) => Ok(ValueKind::Bool(lhs < rhs)),
                _ => Err(vec![IError::CannotCompare {
                    lhs_ty: lhs_ty.to_string(),
                    rhs_ty: rhs_ty.to_string(),
                    op: binop.to_string(),
                    op_span: binop.span,
                }]),
            }
        }
        BinOpKind::Le => {
            if prev_comparation_span.is_some() {
                return Err(vec![IError::ComparisonOperatorsCannotBeChained {
                    chain_op_span: vec![prev_comparation_span.unwrap(), binop.span],
                }]);
            }
            match (lhs.kind, rhs.kind) {
                (ValueKind::Int(lhs), ValueKind::Int(rhs)) => Ok(ValueKind::Bool(lhs <= rhs)),
                (ValueKind::Float(lhs), ValueKind::Float(rhs)) => Ok(ValueKind::Bool(lhs <= rhs)),
                (ValueKind::Str(lhs), ValueKind::Str(rhs)) => Ok(ValueKind::Bool(lhs <= rhs)),
                (ValueKind::Char(lhs), ValueKind::Char(rhs)) => Ok(ValueKind::Bool(lhs <= rhs)),
                _ => Err(vec![IError::CannotCompare {
                    lhs_ty: lhs_ty.to_string(),
                    rhs_ty: rhs_ty.to_string(),
                    op: binop.to_string(),
                    op_span: binop.span,
                }]),
            }
        }
        BinOpKind::Gt => {
            if prev_comparation_span.is_some() {
                return Err(vec![IError::ComparisonOperatorsCannotBeChained {
                    chain_op_span: vec![prev_comparation_span.unwrap(), binop.span],
                }]);
            }
            match (lhs.kind, rhs.kind) {
                (ValueKind::Int(lhs), ValueKind::Int(rhs)) => Ok(ValueKind::Bool(lhs > rhs)),
                (ValueKind::Float(lhs), ValueKind::Float(rhs)) => Ok(ValueKind::Bool(lhs > rhs)),
                (ValueKind::Str(lhs), ValueKind::Str(rhs)) => Ok(ValueKind::Bool(lhs > rhs)),
                (ValueKind::Char(lhs), ValueKind::Char(rhs)) => Ok(ValueKind::Bool(lhs > rhs)),
                _ => Err(vec![IError::CannotCompare {
                    lhs_ty: lhs_ty.to_string(),
                    rhs_ty: rhs_ty.to_string(),
                    op: binop.to_string(),
                    op_span: binop.span,
                }]),
            }
        }
        BinOpKind::Ge => {
            if prev_comparation_span.is_some() {
                return Err(vec![IError::ComparisonOperatorsCannotBeChained {
                    chain_op_span: vec![prev_comparation_span.unwrap(), binop.span],
                }]);
            }
            match (lhs.kind, rhs.kind) {
                (ValueKind::Int(lhs), ValueKind::Int(rhs)) => Ok(ValueKind::Bool(lhs >= rhs)),
                (ValueKind::Float(lhs), ValueKind::Float(rhs)) => Ok(ValueKind::Bool(lhs >= rhs)),
                (ValueKind::Str(lhs), ValueKind::Str(rhs)) => Ok(ValueKind::Bool(lhs >= rhs)),
                (ValueKind::Char(lhs), ValueKind::Char(rhs)) => Ok(ValueKind::Bool(lhs >= rhs)),
                _ => Err(vec![IError::CannotCompare {
                    lhs_ty: lhs_ty.to_string(),
                    rhs_ty: rhs_ty.to_string(),
                    op: binop.to_string(),
                    op_span: binop.span,
                }]),
            }
        }
        BinOpKind::And => match (lhs.kind, rhs.kind) {
            (ValueKind::Bool(lhs), ValueKind::Bool(rhs)) => Ok(ValueKind::Bool(lhs && rhs)),
            _ => Err(vec![IError::NoImplForOp {
                lhs_ty: lhs_ty.to_string(),
                rhs_ty: rhs_ty.to_string(),
                op: binop.to_string(),
                op_span: binop.span,
            }]),
        },
        BinOpKind::Or => match (lhs.kind, rhs.kind) {
            (ValueKind::Bool(lhs), ValueKind::Bool(rhs)) => Ok(ValueKind::Bool(lhs || rhs)),
            _ => Err(vec![IError::NoImplForOp {
                lhs_ty: lhs_ty.to_string(),
                rhs_ty: rhs_ty.to_string(),
                op: binop.to_string(),
                op_span: binop.span,
            }]),
        },
        BinOpKind::BitAnd => match (lhs.kind, rhs.kind) {
            (ValueKind::Int(lhs), ValueKind::Int(rhs)) => Ok(ValueKind::Int(lhs & rhs)),
            _ => Err(vec![IError::NoImplForOp {
                lhs_ty: lhs_ty.to_string(),
                rhs_ty: rhs_ty.to_string(),
                op: binop.to_string(),
                op_span: binop.span,
            }]),
        },
        BinOpKind::BitOr => match (lhs.kind, rhs.kind) {
            (ValueKind::Int(lhs), ValueKind::Int(rhs)) => Ok(ValueKind::Int(lhs | rhs)),
            _ => Err(vec![IError::NoImplForOp {
                lhs_ty: lhs_ty.to_string(),
                rhs_ty: rhs_ty.to_string(),
                op: binop.to_string(),
                op_span: binop.span,
            }]),
        },
        BinOpKind::BitXor => match (lhs.kind, rhs.kind) {
            (ValueKind::Int(lhs), ValueKind::Int(rhs)) => Ok(ValueKind::Int(lhs ^ rhs)),
            _ => Err(vec![IError::NoImplForOp {
                lhs_ty: lhs_ty.to_string(),
                rhs_ty: rhs_ty.to_string(),
                op: binop.to_string(),
                op_span: binop.span,
            }]),
        },
        BinOpKind::Shl => match (lhs.kind, rhs.kind) {
            (ValueKind::Int(lhs), ValueKind::Int(rhs)) => Ok(ValueKind::Int(lhs << rhs)),
            _ => Err(vec![IError::NoImplForOp {
                lhs_ty: lhs_ty.to_string(),
                rhs_ty: rhs_ty.to_string(),
                op: binop.to_string(),
                op_span: binop.span,
            }]),
        },
        BinOpKind::Shr => match (lhs.kind, rhs.kind) {
            (ValueKind::Int(lhs), ValueKind::Int(rhs)) => Ok(ValueKind::Int(lhs >> rhs)),
            _ => Err(vec![IError::NoImplForOp {
                lhs_ty: lhs_ty.to_string(),
                rhs_ty: rhs_ty.to_string(),
                op: binop.to_string(),
                op_span: binop.span,
            }]),
        },
    }
}

fn interpret_expr_ident(
    env: &mut Environment,
    ident: &ast::Ident,
) -> Result<ValueKind, Vec<IError>> {
    let result = env.lookup_variable(ident.name.as_str());
    if result.is_none() {
        return Err(vec![IError::CannotFindVariableInScope {
            var_name: ident.name.to_string(),
            span: ident.span,
        }]);
    }

    let variable = result.unwrap();
    let value = variable.borrow().val.clone();
    if value.is_none() {
        return Err(vec![IError::VariableIsNotInitialized {
            var_name: variable.borrow().ident.name.clone(),
            declared_span: variable.borrow().ident.span,
            used_span: ident.span,
        }]);
    }

    Ok(value.unwrap().kind)
}

fn interpret_expr_funcall(
    env: &mut Environment,
    prefix: &Box<Expr>,
    args: &Vec<Box<Expr>>,
    in_loop: bool,
    is_verbose: bool,
) -> Result<ValueKind, Vec<IError>> {
    let ident = match &prefix.kind {
        ExprKind::Identifier(ident) => ident,
        _ => return Err(vec![IError::InvalidFunctionCall { span: prefix.span }]),
    };

    let result = env.lookup_function(ident.name.as_str());
    if result.is_none() {
        // We will try to find the function in the standard library
        let std_func_name = ident.name.as_str();
        let std_func_span = ident.span;

        // Fix: not handle mutable arguments
        let evaluated_args = {
            let mut args_vec = Vec::new();
            for arg in args {
                args_vec.push((interpret_expr(env, arg, in_loop, None, is_verbose)?, true));
            }
            args_vec
        };

        if let Some(lib) = env.lookup_default_library("std") {
            if let Some(func) = lib.get_function(std_func_name) {
                return func(evaluated_args).map_err(|_err| vec![]); // FIX
            } else {
                return Err(vec![IError::FunctionNotInScope {
                    function: std_func_name.to_string(),
                    span: std_func_span,
                }]);
            }
        } else {
            unreachable!("Standard library must be loaded");
        }
    }

    let mut evaluated_args = Vec::new();
    for arg in args {
        evaluated_args.push(interpret_expr(env, arg, in_loop, None, is_verbose)?);
    }

    Value::evaluate_function(env, result.unwrap().1, evaluated_args, is_verbose)
}

fn interpret_expr_literal(value: &Lit) -> Result<ValueKind, Vec<IError>> {
    let Lit { kind, symbol } = value;
    let val = symbol.as_str();
    match kind {
        LitKind::Int => Ok(ValueKind::Int(val.parse::<i64>().unwrap())),
        LitKind::Float => Ok(ValueKind::Float(val.parse::<f64>().unwrap())),
        LitKind::Str => Ok(ValueKind::Str(val.to_string())),
        LitKind::Bool => Ok(ValueKind::Bool(val.parse::<bool>().unwrap())),
        LitKind::Char => Ok(ValueKind::Char(val.chars().next().unwrap())),
        LitKind::RawStr(_) => Ok(ValueKind::Str(val.to_string())),
        LitKind::Err => unreachable!(),
    }
}

fn update_array_index(
    env: &mut Environment,
    arr: &Box<Expr>,
    value: Vec<Value>,
    is_verbose: bool,
) -> Result<(), Vec<IError>> {
    // This will store all the index access of the array (in reverse).
    let mut indices: Vec<i64> = Vec::new();
    let mut e = arr;
    let var = loop {
        match &e.kind {
            ExprKind::Index(array, index, _) => {
                let index = interpret_expr(env, index, false, None, is_verbose).unwrap();
                match index.kind {
                    ValueKind::Int(index) => indices.push(index),
                    _ => unreachable!(),
                }
                e = array;
            }
            ExprKind::Identifier(array_name) => {
                let var = env.lookup_variable(array_name.name.as_str());
                if var.is_none() {
                    return Err(vec![IError::CannotFindVariableInScope {
                        var_name: array_name.name.to_string(),
                        span: array_name.span,
                    }]);
                }
                break var.unwrap();
            }
            _ => unreachable!(),
        }
    };

    // Now we have the variable, we can update the value by traversing the indices in reverse.
    let mut var_bind = var.borrow_mut();
    let mut elements: &mut Vec<Value> = match var_bind.val.as_mut().unwrap().kind {
        ValueKind::Array(ref mut elements) => elements,
        _ => unreachable!(),
    };

    for index in indices.iter().rev() {
        let index = *index as usize;

        if index >= elements.len() {
            return Err(vec![IError::IndexOutOfBounds {
                len: elements.len() as i64,
                index: index as i64,
                span: arr.span,
            }]);
        }

        elements = match &mut elements[index].kind {
            ValueKind::Array(elements) => elements,
            _ => unreachable!("This is not the last index so it must be an array"),
        };
    }

    // Now we have the last array's dimension, we can update the value.
    *elements = value;
    Ok(())
}

fn interpret_expr_assign_ident_with_known_value(
    env: &mut Environment,
    ident: &ast::Ident,
    rhs: ValueKind,
    rhs_span: Span,
    expr_span: Span,
    _is_verbose: bool,
) -> Result<ValueKind, Vec<IError>> {
    let var = env.lookup_variable(ident.name.as_str());
    if var.is_none() {
        return Err(vec![IError::CannotFindVariableInScope {
            var_name: ident.name.to_string(),
            span: ident.span,
        }]);
    }

    let var = var.unwrap();
    let first_assign_span = var.borrow().first_assigned_span;
    let decl_span = var.borrow().ident.span;
    if !var.borrow().can_be_assigned() {
        return Err(vec![IError::MutateImmutableVariable {
            var_name: ident.name.to_string(),
            first_assign_span: first_assign_span.expect("Variable must be assigned before"),
            second_assign_span: expr_span,
            help_span: decl_span,
        }]);
    }

    if var.borrow().ty.kind != rhs.to_ty_kind() {
        return Err(vec![IError::MismatchedType {
            expected: var.borrow().ty.to_string(),
            found: rhs.to_ty_kind().to_string(),
            span: expr_span,
        }]);
    }

    var.borrow_mut().val = Some(Value {
        kind: rhs,
        span: rhs_span,
    });

    var.borrow_mut().first_assigned_span = Some(expr_span);
    Ok(ValueKind::Unit)
}

fn interpret_expr_assign_array_index_with_known_value(
    env: &mut Environment,
    array: &Box<Expr>,
    index: &Box<Expr>,
    rhs: ValueKind,
    in_loop: bool,
    is_verbose: bool,
) -> Result<ValueKind, Vec<IError>> {
    let arr = interpret_expr(env, array, in_loop, None, is_verbose)?;

    match arr.kind {
        ValueKind::Array(mut elements) => {
            let index = interpret_expr(env, index, in_loop, None, is_verbose)?;

            match index.kind {
                ValueKind::Int(i) => {
                    if i < 0 || i as usize >= elements.len() {
                        return Err(vec![IError::IndexOutOfBounds {
                            len: elements.len() as i64,
                            index: i,
                            span: index.span,
                        }]);
                    }
                    elements[i as usize].kind = rhs;

                    update_array_index(env, array, elements, is_verbose)?;
                    Ok(ValueKind::Unit)
                }
                _ => Err(vec![IError::MismatchedType {
                    expected: TyKind::Int.to_string(),
                    found: index.to_ty_kind().to_string(),
                    span: index.span,
                }]),
            }
        }
        _ => Err(vec![IError::IndexingWrongType {
            ty: arr.to_ty_kind().to_string(),
            span: array.span,
        }]),
    }
}

fn interpret_expr_assign_with_known_value(
    env: &mut Environment,
    lhs: &Box<Expr>,
    rhs: Value,
    in_loop: bool,
    expr_span: Span,
    is_verbose: bool,
) -> Result<ValueKind, Vec<IError>> {
    let Value { kind, span } = rhs;
    match &lhs.kind {
        ExprKind::Identifier(ident) => interpret_expr_assign_ident_with_known_value(
            env, ident, kind, span, expr_span, is_verbose,
        ),
        ExprKind::Index(arr, idx, _) => interpret_expr_assign_array_index_with_known_value(
            env, arr, idx, kind, in_loop, is_verbose,
        ),
        _ => {
            return Err(vec![IError::InvalidLhsAssign {
                assign_span: expr_span,
                lhs_span: lhs.span,
            }]);
        }
    }
}
