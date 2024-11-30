use core::fmt;
use std::fmt::{Display, Formatter};

use crate::ast;

use super::{environment::Environment, interpret_expr, IError, Value};

pub fn interpret_ty(
    env: &mut Environment,
    ty: &ast::Ty,
    in_loop: bool,
    comparation_not_allowed: bool,
) -> Result<Ty, IError> {
    match &ty.kind {
        ast::TyKind::Named(ident) => interpret_ty_ident(ident),
        ast::TyKind::Array(ty, len) => {
            interpret_ty_array(env, ty, len, in_loop, comparation_not_allowed)
        }
    }
}

fn interpret_ty_array(
    env: &mut Environment,
    ty: &ast::Ty,
    len: &Option<Box<ast::Expr>>,
    in_loop: bool,
    comparation_not_allowed: bool,
) -> Result<Ty, IError> {
    let ty = interpret_ty(env, ty, in_loop, comparation_not_allowed)?;

    match len {
        Some(len) => {
            let len = interpret_expr(env, len, in_loop, comparation_not_allowed)?;
            match len {
                Value::Int(len) => Ok(Ty::Array(Box::new(ty), len)),
                _ => Err(format!("expected integer literal, found {:?}", len)),
            }
        }
        None => Ok(Ty::Array(Box::new(ty), -1)),
    }
}

fn interpret_ty_ident(ident: &ast::Ident) -> Result<Ty, IError> {
    match ident.name.as_str() {
        "int" => Ok(Ty::Int),
        "float" => Ok(Ty::Float),
        "str" => Ok(Ty::Str),
        "bool" => Ok(Ty::Bool),
        "char" => Ok(Ty::Char),
        "()" => Ok(Ty::Unit),
        _ => Err(format!("unknown type `{}`", ident.name.as_str())),
    }
}

#[derive(Debug, Clone)]
pub enum Ty {
    Int,
    Float,
    Str,
    Bool,
    Function,
    Unit,
    Char,
    Array(Box<Ty>, i64),
}

impl PartialEq for Ty {
    fn eq(&self, other: &Self) -> bool {
        match self {
            Ty::Int => match other {
                Ty::Int => true,
                _ => false,
            },
            Ty::Float => match other {
                Ty::Float => true,
                _ => false,
            },
            Ty::Str => match other {
                Ty::Str => true,
                _ => false,
            },
            Ty::Bool => match other {
                Ty::Bool => true,
                _ => false,
            },
            Ty::Function => match other {
                Ty::Function => true,
                _ => false,
            },
            Ty::Unit => match other {
                Ty::Unit => true,
                _ => false,
            },
            Ty::Char => match other {
                Ty::Char => true,
                _ => false,
            },
            Ty::Array(ty, len) => match other {
                Ty::Array(other_ty, other_len) => {
                    // There are cases when the length is not known (-1)
                    ty == other_ty && (*len == -1 || *other_len == -1 || len == other_len)
                }
                _ => false,
            },
        }
    }
}

impl Display for Ty {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", self.to_string())
    }
}

impl Ty {
    pub fn is_iterable(&self) -> bool {
        match self {
            Ty::Int => false,
            Ty::Float => false,
            Ty::Str => true,
            Ty::Bool => false,
            Ty::Function => false,
            Ty::Unit => false,
            Ty::Char => false,
            Ty::Array(_, _) => true,
        }
    }
    pub fn is_clonable(&self) -> bool {
        match self {
            Ty::Int => true,
            Ty::Float => true,
            Ty::Str => true,
            Ty::Bool => true,
            Ty::Function => false,
            Ty::Unit => false,
            Ty::Char => true,
            Ty::Array(ty, _) => ty.is_clonable(),
        }
    }

    pub fn castable_to(&self, other: &Ty) -> bool {
        match self {
            Ty::Int => match other {
                Ty::Int => true,
                Ty::Float => true,
                _ => false,
            },
            Ty::Float => match other {
                Ty::Int => true,
                Ty::Float => true,
                _ => false,
            },
            Ty::Str => match other {
                Ty::Str => true,
                _ => false,
            },
            Ty::Bool => match other {
                Ty::Bool => true,
                _ => false,
            },
            Ty::Function => false,
            Ty::Unit => false,
            Ty::Char => match other {
                Ty::Int => true,
                Ty::Char => true,
                _ => false,
            },
            Ty::Array(..) => false,
        }
    }

    pub fn can_be_ident_type(&self) -> bool {
        match self {
            Ty::Int => true,
            Ty::Float => true,
            Ty::Str => true,
            Ty::Bool => true,
            Ty::Function => false,
            Ty::Unit => false,
            Ty::Char => true,
            Ty::Array(ty, _) => ty.can_be_ident_type(),
        }
    }

    pub fn to_string(&self) -> String {
        match self {
            Ty::Int => "int".to_string(),
            Ty::Float => "float".to_string(),
            Ty::Str => "str".to_string(),
            Ty::Bool => "bool".to_string(),
            Ty::Function => "function".to_string(),
            Ty::Unit => "()".to_string(),
            Ty::Char => "char".to_string(),
            Ty::Array(ty, len) => {
                if *len == -1 {
                    format!("[{}]", ty.to_string())
                } else {
                    format!("[{}; {}]", ty.to_string(), len)
                }
            }
        }
    }
}
