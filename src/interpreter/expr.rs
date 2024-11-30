use crate::{
    ast::{self, BinOp, BinOpKind, Expr, ExprKind, Ident, Lit, LitKind},
    interpreter::eval::ControlFlow,
};

use super::{
    environment::{variable::Variable, Environment, Wrapper},
    eval::{EvalResult, Func, Value},
    interpret_ty, stmt,
    ty::Ty,
    IError,
};

pub fn interpret_expr(
    env: &mut Environment,
    expr: &Box<Expr>,
    in_loop: bool,
    comparation_not_allowed: bool,
) -> Result<Value, IError> {
    match &expr.kind {
        ExprKind::Binary(op, lhs, rhs) => {
            interpret_expr_binary(env, op, lhs, rhs, in_loop, comparation_not_allowed)
        }
        ExprKind::Identifier(ident) => interpret_expr_ident(env, ident),
        ExprKind::Literal(value) => interpret_expr_literal(value),
        ExprKind::FunCall(prefix, args) => interpret_expr_funcall(env, prefix, args, in_loop),
        ExprKind::LibFunCall(lib_fun, args) => {
            interpret_expr_lib_funcall(env, lib_fun, args, in_loop)
        }
        ExprKind::Cast(expr, ty) => {
            interpret_expr_cast(env, expr, ty, in_loop, comparation_not_allowed)
        }
        ExprKind::Assign(lhs, rhs, _) => interpret_expr_assign(env, lhs, rhs, in_loop),
        ExprKind::Unary(unop, expr) => {
            interpret_expr_unary(env, unop, expr, in_loop, comparation_not_allowed)
        }
        ExprKind::AssignOp(binop, lhs, rhs) => {
            interpret_expr_assign_op(env, lhs, binop, rhs, in_loop)
        }
        ExprKind::LibAccess(lib, ident) => {
            interpret_expr_lib_access(env, lib, ident, in_loop, comparation_not_allowed)
        }
        ExprKind::Array(elements) => {
            interpret_expr_array(env, elements, in_loop, comparation_not_allowed)
        }
        ExprKind::Index(array, index, _) => {
            interpret_expr_index(env, array, index, in_loop, comparation_not_allowed)
        }
        ExprKind::Repeat(element, count) => {
            interpret_expr_repeat(env, element, count, in_loop, comparation_not_allowed)
        }
    }
}

pub fn interpret_expr_repeat(
    env: &mut Environment,
    element: &Box<Expr>,
    count: &Box<Expr>,
    in_loop: bool,
    comparation_not_allowed: bool,
) -> Result<Value, IError> {
    let element = interpret_expr(env, element, in_loop, comparation_not_allowed)?;
    let count = interpret_expr(env, count, in_loop, comparation_not_allowed)?;

    match count {
        Value::Int(count) => {
            if count < 0 {
                return Err(format!("Repeat count must be non-negative, found '{}'", count).into());
            }
            Ok(Value::Array(vec![element; count as usize]))
        }
        _ => Err(format!("Repeat count must be an integer, found '{}'", count.to_ty()).into()),
    }
}

pub fn interpret_expr_index(
    env: &mut Environment,
    array: &Box<Expr>,
    index: &Box<Expr>,
    in_loop: bool,
    comparation_not_allowed: bool,
) -> Result<Value, IError> {
    let array = interpret_expr(env, array, in_loop, comparation_not_allowed)?;
    let index = interpret_expr(env, index, in_loop, comparation_not_allowed)?;

    match array {
        Value::Array(elements) => match index {
            Value::Int(index) => {
                if index < 0 || index as usize >= elements.len() {
                    return Err(format!(
                        "Index out of bounds, expected index between 0 and {} but got {}",
                        elements.len() - 1,
                        index
                    )
                    .into());
                }
                Ok(elements[index as usize].clone())
            }
            _ => Err(format!("Index must be an integer, found '{}'", index.to_ty()).into()),
        },
        _ => Err(format!(
            "Indexing must be performed on an array, found '{}'",
            array.to_ty()
        )
        .into()),
    }
}

pub fn interpret_expr_array(
    env: &mut Environment,
    elements: &Vec<Box<Expr>>,
    in_loop: bool,
    comparation_not_allowed: bool,
) -> Result<Value, IError> {
    let mut result = Vec::new();
    let mut ty: Option<Ty> = None;
    for element in elements {
        let element = interpret_expr(env, element, in_loop, comparation_not_allowed)?;
        match ty {
            Some(ref ty) => {
                if *ty != element.to_ty() {
                    return Err(format!(
                        "Array elements must have the same type, expected '{}' but got '{}'",
                        ty,
                        element.to_ty()
                    )
                    .into());
                }
            }
            None => {
                ty = Some(element.to_ty());
            }
        }
        result.push(element);
    }

    Ok(Value::Array(result))
}

pub fn interpret_expr_lib_access(
    env: &mut Environment,
    lib: &Box<Expr>,
    ident: &Ident,
    in_loop: bool,
    comparation_not_allowed: bool,
) -> Result<Value, IError> {
    let lib = interpret_expr(env, lib, in_loop, comparation_not_allowed)?;
    let lib_name = match lib {
        Value::Str(val) => val,
        _ => return Err("Library name must be a string".into()),
    };

    let result = env.lookup_library(&lib_name);
    if result.is_none() {
        return Err(format!("Library '{}' not found", lib_name).into());
    }

    let lib = result.unwrap();
    let func = lib.get_function(ident.name.as_str());
    if func.is_none() {
        return Err(format!(
            "Function '{}' not found in library '{}'",
            ident.name, lib_name
        )
        .into());
    }

    Ok(Value::Unit)
}

pub fn interpret_expr_unary(
    env: &mut Environment,
    op: &ast::UnOp,
    expr: &Box<Expr>,
    in_loop: bool,
    comparation_not_allowed: bool,
) -> Result<Value, IError> {
    let expr = interpret_expr(env, expr, in_loop, comparation_not_allowed)?;
    match op {
        ast::UnOp::Ne => match expr {
            Value::Int(val) => Ok(Value::Int(-val)),
            Value::Float(val) => Ok(Value::Float(-val)),
            _ => Err(format!("Cannot negate value of type '{}'", expr.to_ty()).into()),
        },
        ast::UnOp::Not => match expr {
            Value::Bool(val) => Ok(Value::Bool(!val)),
            _ => Err(format!("Cannot negate value of type '{}'", expr.to_ty()).into()),
        },
    }
}

pub fn interpret_expr_cast(
    env: &mut Environment,
    expr: &Box<Expr>,
    ty: &ast::Ty,
    in_loop: bool,
    comparation_not_allowed: bool,
) -> Result<Value, IError> {
    let expr = interpret_expr(env, expr, in_loop, comparation_not_allowed)?;
    let ty = interpret_ty(env, ty, in_loop, comparation_not_allowed)?;
    match expr.try_cast_to(&ty) {
        Ok(val) => Ok(val),
        Err(err) => Err(err),
    }
}

pub fn interpret_expr_assign_op(
    env: &mut Environment,
    lhs: &Box<Expr>,
    op: &BinOp,
    rhs: &Box<Expr>,
    in_loop: bool,
) -> Result<Value, IError> {
    let result = interpret_expr_binary(env, op, lhs, rhs, in_loop, false)?;
    interpret_expr_assign_with_known_value(env, lhs, result, in_loop)
}

pub fn interpret_expr_assign(
    env: &mut Environment,
    lhs: &Box<Expr>,
    rhs: &Box<Expr>,
    in_loop: bool,
) -> Result<Value, IError> {
    let rhs = interpret_expr(env, rhs, in_loop, false)?;
    match &lhs.kind {
        ExprKind::Identifier(ident) => {
            interpret_expr_assign_ident_with_known_value(env, ident, rhs, in_loop)
        }
        ExprKind::Index(arr, idx, _) => {
            interpret_expr_assign_array_index_with_known_value(env, arr, idx, rhs, in_loop)
        }
        _ => {
            println!("{:?}", lhs);
            return Err("left-hand side of assignment must be an identifier".into());
        }
    }
}

pub fn interpret_expr_lib_funcall(
    env: &mut Environment,
    lib_fun: &Box<Expr>,
    args: &Vec<Box<Expr>>,
    in_loop: bool,
) -> Result<Value, IError> {
    let (lib_name, func_name) = match &lib_fun.kind {
        ExprKind::LibAccess(prefix, func_name) => match &prefix.kind {
            ExprKind::Identifier(lib_name) => {
                (lib_name.name.to_string(), func_name.name.to_string())
            }
            _ => return Err("Library does not support part currently".into()),
        },
        _ => return Err("Library function call prefix must be a library access".into()),
    };

    eval_expr_lib_funcall(env, &lib_name, &func_name, args, in_loop)
}

pub fn interpret_expr_binary(
    env: &mut Environment,
    binop: &BinOp,
    lhs: &Box<Expr>,
    rhs: &Box<Expr>,
    in_loop: bool,
    comparation_not_allowed: bool,
) -> Result<Value, IError> {
    let lhs = interpret_expr(env, lhs, in_loop, comparation_not_allowed)?;
    let rhs = interpret_expr(env, rhs, in_loop, comparation_not_allowed)?;

    let lhs_ty = lhs.to_ty();
    let rhs_ty = rhs.to_ty();

    match &binop.node {
        BinOpKind::Add => match (lhs, rhs) {
            (Value::Int(lhs), Value::Int(rhs)) => Ok(Value::Int(lhs + rhs)),
            (Value::Float(lhs), Value::Float(rhs)) => Ok(Value::Float(lhs + rhs)),
            (Value::Str(lhs), Value::Str(rhs)) => Ok(Value::Str(format!("{}{}", lhs, rhs))),
            _ => return Err(format!("Cannot add '{}' and '{}'", lhs_ty, rhs_ty).into()),
        },
        BinOpKind::Sub => match (lhs, rhs) {
            (Value::Int(lhs), Value::Int(rhs)) => Ok(Value::Int(lhs - rhs)),
            (Value::Float(lhs), Value::Float(rhs)) => Ok(Value::Float(lhs - rhs)),
            _ => return Err(format!("Cannot subtract '{}' and '{}'", lhs_ty, rhs_ty).into()),
        },
        BinOpKind::Mul => match (lhs, rhs) {
            (Value::Int(lhs), Value::Int(rhs)) => Ok(Value::Int(lhs * rhs)),
            (Value::Float(lhs), Value::Float(rhs)) => Ok(Value::Float(lhs * rhs)),
            _ => return Err(format!("Cannot multiply '{}' and '{}'", lhs_ty, rhs_ty).into()),
        },
        BinOpKind::Div => match (lhs, rhs) {
            (Value::Int(lhs), Value::Int(rhs)) => Ok(Value::Int(lhs / rhs)),
            (Value::Float(lhs), Value::Float(rhs)) => Ok(Value::Float(lhs / rhs)),
            _ => return Err(format!("Cannot divide '{}' and '{}'", lhs_ty, rhs_ty).into()),
        },
        BinOpKind::Mod => match (lhs, rhs) {
            (Value::Int(lhs), Value::Int(rhs)) => Ok(Value::Int(lhs % rhs)),
            (Value::Float(lhs), Value::Float(rhs)) => Ok(Value::Float(lhs % rhs)),
            _ => return Err(format!("Cannot modulo '{}' and '{}'", lhs_ty, rhs_ty).into()),
        },
        BinOpKind::Eq => {
            if comparation_not_allowed {
                // must be chained comparation
                return Err("Chained comparation is not allowed".into());
            }
            match (lhs, rhs) {
                (Value::Int(lhs), Value::Int(rhs)) => Ok(Value::Bool(lhs == rhs)),
                (Value::Float(lhs), Value::Float(rhs)) => Ok(Value::Bool(lhs == rhs)),
                (Value::Str(lhs), Value::Str(rhs)) => Ok(Value::Bool(lhs == rhs)),
                (Value::Bool(lhs), Value::Bool(rhs)) => Ok(Value::Bool(lhs == rhs)),
                (Value::Char(lhs), Value::Char(rhs)) => Ok(Value::Bool(lhs == rhs)),
                _ => unreachable!(),
            }
        }
        BinOpKind::Ne => {
            if comparation_not_allowed {
                // must be chained comparation
                return Err("Chained comparation is not allowed".into());
            }
            match (lhs, rhs) {
                (Value::Int(lhs), Value::Int(rhs)) => Ok(Value::Bool(lhs != rhs)),
                (Value::Float(lhs), Value::Float(rhs)) => Ok(Value::Bool(lhs != rhs)),
                (Value::Str(lhs), Value::Str(rhs)) => Ok(Value::Bool(lhs != rhs)),
                (Value::Bool(lhs), Value::Bool(rhs)) => Ok(Value::Bool(lhs != rhs)),
                (Value::Char(lhs), Value::Char(rhs)) => Ok(Value::Bool(lhs != rhs)),
                _ => unreachable!(),
            }
        }
        BinOpKind::Lt => {
            if comparation_not_allowed {
                // must be chained comparation
                return Err("Chained comparation is not allowed".into());
            }
            match (lhs, rhs) {
                (Value::Int(lhs), Value::Int(rhs)) => Ok(Value::Bool(lhs < rhs)),
                (Value::Float(lhs), Value::Float(rhs)) => Ok(Value::Bool(lhs < rhs)),
                (Value::Str(lhs), Value::Str(rhs)) => Ok(Value::Bool(lhs < rhs)),
                (Value::Char(lhs), Value::Char(rhs)) => Ok(Value::Bool(lhs < rhs)),
                _ => unreachable!(),
            }
        }
        BinOpKind::Le => {
            if comparation_not_allowed {
                // must be chained comparation
                return Err("Chained comparation is not allowed".into());
            }
            match (lhs, rhs) {
                (Value::Int(lhs), Value::Int(rhs)) => Ok(Value::Bool(lhs <= rhs)),
                (Value::Float(lhs), Value::Float(rhs)) => Ok(Value::Bool(lhs <= rhs)),
                (Value::Str(lhs), Value::Str(rhs)) => Ok(Value::Bool(lhs <= rhs)),
                (Value::Char(lhs), Value::Char(rhs)) => Ok(Value::Bool(lhs <= rhs)),
                _ => unreachable!(),
            }
        }
        BinOpKind::Gt => {
            if comparation_not_allowed {
                // must be chained comparation
                return Err("Chained comparation is not allowed".into());
            }
            match (lhs, rhs) {
                (Value::Int(lhs), Value::Int(rhs)) => Ok(Value::Bool(lhs > rhs)),
                (Value::Float(lhs), Value::Float(rhs)) => Ok(Value::Bool(lhs > rhs)),
                (Value::Str(lhs), Value::Str(rhs)) => Ok(Value::Bool(lhs > rhs)),
                (Value::Char(lhs), Value::Char(rhs)) => Ok(Value::Bool(lhs > rhs)),
                _ => unreachable!(),
            }
        }
        BinOpKind::Ge => {
            if comparation_not_allowed {
                // must be chained comparation
                return Err("Chained comparation is not allowed".into());
            }
            match (lhs, rhs) {
                (Value::Int(lhs), Value::Int(rhs)) => Ok(Value::Bool(lhs >= rhs)),
                (Value::Float(lhs), Value::Float(rhs)) => Ok(Value::Bool(lhs >= rhs)),
                (Value::Str(lhs), Value::Str(rhs)) => Ok(Value::Bool(lhs >= rhs)),
                (Value::Char(lhs), Value::Char(rhs)) => Ok(Value::Bool(lhs >= rhs)),
                _ => unreachable!(),
            }
        }
        BinOpKind::And => match (lhs, rhs) {
            (Value::Bool(lhs), Value::Bool(rhs)) => Ok(Value::Bool(lhs && rhs)),
            _ => {
                return Err(format!("Cannot perform 'and' on '{}' and '{}'", lhs_ty, rhs_ty).into())
            }
        },
        BinOpKind::Or => match (lhs, rhs) {
            (Value::Bool(lhs), Value::Bool(rhs)) => Ok(Value::Bool(lhs || rhs)),
            _ => {
                return Err(format!("Cannot perform 'or' on '{}' and '{}'", lhs_ty, rhs_ty).into())
            }
        },
        BinOpKind::BitAnd => match (lhs, rhs) {
            (Value::Int(lhs), Value::Int(rhs)) => Ok(Value::Int(lhs & rhs)),
            _ => {
                return Err(
                    format!("Cannot perform 'bitand' on '{}' and '{}'", lhs_ty, rhs_ty).into(),
                )
            }
        },
        BinOpKind::BitOr => match (lhs, rhs) {
            (Value::Int(lhs), Value::Int(rhs)) => Ok(Value::Int(lhs | rhs)),
            _ => {
                return Err(
                    format!("Cannot perform 'bitor' on '{}' and '{}'", lhs_ty, rhs_ty).into(),
                )
            }
        },
        BinOpKind::BitXor => match (lhs, rhs) {
            (Value::Int(lhs), Value::Int(rhs)) => Ok(Value::Int(lhs ^ rhs)),
            _ => {
                return Err(
                    format!("Cannot perform 'bitxor' on '{}' and '{}'", lhs_ty, rhs_ty).into(),
                )
            }
        },
        BinOpKind::Shl => match (lhs, rhs) {
            (Value::Int(lhs), Value::Int(rhs)) => Ok(Value::Int(lhs << rhs)),
            _ => {
                return Err(format!("Cannot perform 'shl' on '{}' and '{}'", lhs_ty, rhs_ty).into())
            }
        },
        BinOpKind::Shr => match (lhs, rhs) {
            (Value::Int(lhs), Value::Int(rhs)) => Ok(Value::Int(lhs >> rhs)),
            _ => {
                return Err(format!("Cannot perform 'shr' on '{}' and '{}'", lhs_ty, rhs_ty).into())
            }
        },
    }
}

pub fn interpret_expr_ident(env: &mut Environment, ident: &Ident) -> Result<Value, IError> {
    let result = env.lookup_variable(ident.name.as_str());
    if result.is_none() {
        return Err(format!("Variable '{}' not found", ident.name).into());
    }

    let variable = result.unwrap();
    let value = variable.borrow().val.clone();
    if value.is_none() {
        return Err(format!("Variable '{}' is not initialized", ident.name).into());
    }

    Ok(value.unwrap())
}

pub fn interpret_expr_funcall(
    env: &mut Environment,
    prefix: &Box<Expr>,
    args: &Vec<Box<Expr>>,
    in_loop: bool,
) -> Result<Value, IError> {
    let ident = match &prefix.kind {
        ExprKind::Identifier(ident) => ident,
        _ => return Err("Function call prefix must be an identifier".into()),
    };

    let result = env.lookup_function(ident.name.as_str());
    if result.is_none() {
        // We will try to find the function in the standard library
        let std_lib_name = "std";
        let std_func_name = ident.name.as_str();

        return eval_expr_lib_funcall(env, std_lib_name, std_func_name, args, in_loop).map_err(
            |err| {
                format!(
                    "Function '{}' not found in the environment or standard library: {}",
                    ident.name, err
                )
                .into()
            },
        );
    }

    let mut evaluated_args = Vec::new();
    for arg in args {
        evaluated_args.push(interpret_expr(env, arg, in_loop, false)?);
    }

    match result.unwrap() {
        Value::Function(func) => {
            let Func {
                name,
                args: params,
                stmts: body,
                ret_ty,
            } = func;
            let mut func_env = Environment::new_with_parent(env);
            func_env.in_function = true;

            if params.len() != evaluated_args.len() {
                return Err(format!(
                    "Function '{}' expects {} arguments but got {}",
                    name,
                    params.len(),
                    evaluated_args.len()
                ));
            }

            // FIX: not handle case where function argument is mutable
            for ((param_name, param_ty, need_mut), arg) in params.iter().zip(evaluated_args) {
                let arg_ty = arg.to_ty();
                if *param_ty != arg_ty {
                    return Err(format!(
                        "Function '{}' expects argument type '{}' but got '{}'",
                        name, param_ty, arg_ty
                    ));
                }

                let name = param_name;
                func_env.insert_variable(name.as_str(), Some(arg), *need_mut, arg_ty);
            }

            let result = stmt::interpret_stmt_block(&mut func_env, &body, false)?;
            match result {
                EvalResult::StmtResult(control_flow) => match control_flow {
                    Some(ControlFlow::Return(val)) => {
                        if val.to_ty() != ret_ty {
                            return Err(format!(
                                "Function '{}' expects return type '{}' but got '{}'",
                                name,
                                ret_ty,
                                val.to_ty()
                            ));
                        }
                        Ok(val)
                    }
                    None => {
                        if ret_ty != Ty::Unit {
                            return Err(format!(
                                "Function '{}' expects return type '{}' but got '()'",
                                name, ret_ty
                            ));
                        }
                        Ok(Value::Unit)
                    }
                    _ => unreachable!("function body should not return continue or break"),
                },
                EvalResult::Value(_) => unreachable!("statement should not return value"),
            }
        }
        _ => unreachable!("This should be a function"),
    }
}

pub fn interpret_expr_literal(value: &Lit) -> Result<Value, IError> {
    let Lit { kind, symbol } = value;
    let val = symbol.as_str();
    match kind {
        LitKind::Int => Ok(Value::Int(val.parse::<i64>().unwrap())),
        LitKind::Float => Ok(Value::Float(val.parse::<f64>().unwrap())),
        LitKind::Str => Ok(Value::Str(val.to_string())),
        LitKind::Bool => Ok(Value::Bool(val.parse::<bool>().unwrap())),
        LitKind::Char => Ok(Value::Char(val.chars().next().unwrap())),
        LitKind::RawStr(_) => Ok(Value::Str(val.to_string())),
        LitKind::Err => unreachable!(),
    }
}

fn find_array_variable(
    env: &mut Environment,
    expr: &Box<Expr>,
) -> Result<Wrapper<Variable>, IError> {
    match &expr.kind {
        ExprKind::Identifier(ident) => {
            let var = env.lookup_variable(ident.name.as_str());
            if var.is_none() {
                return Err(format!("Variable '{}' not found", ident.name).into());
            }

            Ok(var.unwrap())
        }
        ExprKind::Index(array, _, _) => find_array_variable(env, array),
        _ => unreachable!(),
    }
}

fn update_array_index(env: &mut Environment, arr: &Box<Expr>, value: Vec<Value>) {
    // This will store all the index access of the array (in reverse).
    let mut indices: Vec<i64> = Vec::new();
    let mut e = arr;
    let var = loop {
        match &e.kind {
            ExprKind::Index(array, index, _) => {
                let index = interpret_expr(env, index, false, false).unwrap();
                match index {
                    Value::Int(index) => indices.push(index),
                    _ => unreachable!(),
                }
                e = array;
            }
            ExprKind::Identifier(array_name) => {
                let var = env.lookup_variable(array_name.name.as_str());
                if var.is_none() {
                    panic!("Variable '{}' not found", array_name.name);
                }
                break var.unwrap();
            }
            _ => unreachable!(),
        }
    };

    // Now we have the variable, we can update the value by traversing the indices in reverse.
    let mut var_bind = var.borrow_mut();
    let mut elements: &mut Vec<Value> = match var_bind.val.as_mut().unwrap() {
        Value::Array(elements) => elements,
        _ => unreachable!(),
    };

    for index in indices.iter().rev() {
        let index = *index as usize;

        if index >= elements.len() {
            panic!(
                "Index out of bounds, expected index between 0 and {} but got {}",
                elements.len() - 1,
                index
            );
        }

        elements = match &mut elements[index] {
            Value::Array(elements) => elements,
            _ => unreachable!("This is not the last index so it must be an array"),
        };
    }

    // Now we have the last array's dimension, we can update the value.
    *elements = value;
}

fn interpret_expr_assign_ident_with_known_value(
    env: &mut Environment,
    ident: &Ident,
    rhs: Value,
    _is_loop: bool,
) -> Result<Value, IError> {
    let var = env.lookup_variable(ident.name.as_str());
    if var.is_none() {
        return Err(format!("Variable '{}' not found", ident.name).into());
    }

    let var = var.unwrap();
    if !var.borrow().can_be_assigned() {
        return Err(format!("Variable '{}' cannot be assigned", ident.name).into());
    }

    if var.borrow().ty != rhs.to_ty() {
        return Err(format!(
            "Cannot assign value of type '{}' to variable of type '{}'",
            rhs.to_ty(),
            var.borrow().ty
        )
        .into());
    }

    var.borrow_mut().val = Some(rhs);
    Ok(Value::Unit)
}

fn interpret_expr_assign_array_index_with_known_value(
    env: &mut Environment,
    array: &Box<Expr>,
    index: &Box<Expr>,
    rhs: Value,
    in_loop: bool,
) -> Result<Value, IError> {
    let arr = interpret_expr(env, array, in_loop, false)?;

    match arr {
        Value::Array(mut elements) => {
            let index = interpret_expr(env, index, in_loop, false)?;

            match index {
                Value::Int(index) => {
                    if index < 0 || index as usize >= elements.len() {
                        return Err(format!(
                            "Index out of bounds, expected index between 0 and {} but got {}",
                            elements.len() - 1,
                            index
                        )
                        .into());
                    }
                    elements[index as usize] = rhs;

                    update_array_index(env, array, elements);
                    Ok(Value::Unit)
                }
                _ => Err(format!("Index must be an integer, found '{}'", index.to_ty()).into()),
            }
        }
        _ => Err(format!(
            "Indexing must be performed on an array, found '{}'",
            arr.to_ty()
        )
        .into()),
    }
}

fn interpret_expr_assign_with_known_value(
    env: &mut Environment,
    lhs: &Box<Expr>,
    rhs: Value,
    in_loop: bool,
) -> Result<Value, IError> {
    match &lhs.kind {
        ExprKind::Identifier(ident) => {
            interpret_expr_assign_ident_with_known_value(env, ident, rhs, in_loop)
        }
        ExprKind::Index(arr, idx, _) => {
            interpret_expr_assign_array_index_with_known_value(env, arr, idx, rhs, in_loop)
        }
        _ => {
            println!("{:?}", lhs);
            return Err("left-hand side of assignment must be an identifier".into());
        }
    }
}

fn eval_expr_lib_funcall(
    env: &mut Environment,
    lib_name: &str,
    func_name: &str,
    args: &Vec<Box<Expr>>,
    in_loop: bool,
) -> Result<Value, IError> {
    // Fix: not handle mutable arguments
    let evaluated_args = {
        let mut args_vec = Vec::new();
        for arg in args {
            args_vec.push((interpret_expr(env, arg, in_loop, false)?, true));
        }
        args_vec
    };

    if let Some(lib) = env.lookup_library(&lib_name) {
        if let Some(func) = lib.get_function(&func_name) {
            func(evaluated_args)
        } else {
            Err(format!(
                "Function '{}' not found in library '{}'",
                func_name, lib_name
            ))
        }
    } else {
        Err(format!("Library '{}' not found", lib_name))
    }
}
