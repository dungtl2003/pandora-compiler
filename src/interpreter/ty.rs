use core::fmt;
use std::fmt::{Display, Formatter};

use crate::{ast, span_encoding::Span};

use super::{environment::Environment, interpret_expr, IError, ValueKind};

pub fn interpret_ty(
    env: &mut Environment,
    ty: &ast::Ty,
    in_loop: bool,
    prev_comparison_span: Option<Span>,
    is_verbose: bool,
) -> Result<Ty, Vec<IError>> {
    let kind = match &ty.kind {
        ast::TyKind::Named(ident) => interpret_ty_ident(ident)?,
        ast::TyKind::Array(ty, len) => {
            interpret_ty_array(env, ty, len, in_loop, prev_comparison_span, is_verbose)?
        }
    };

    Ok(Ty {
        kind,
        span: ty.span,
    })
}

fn interpret_ty_array(
    env: &mut Environment,
    ty: &ast::Ty,
    len: &Option<Box<ast::Expr>>,
    in_loop: bool,
    prev_comparison_span: Option<Span>,
    is_verbose: bool,
) -> Result<TyKind, Vec<IError>> {
    let ty = interpret_ty(env, ty, in_loop, prev_comparison_span, is_verbose)?;

    match len {
        Some(len) => {
            let len = interpret_expr(env, len, in_loop, prev_comparison_span, is_verbose)?;
            match len.kind {
                ValueKind::Int(len) => Ok(TyKind::Array(Box::new(ty.kind), len)),
                _ => Err(vec![IError::MismatchedType {
                    expected: TyKind::Int.to_string(),
                    found: len.to_ty_kind().to_string(),
                    span: len.span,
                }]),
            }
        }
        None => Ok(TyKind::Array(Box::new(ty.kind), -1)),
    }
}

fn interpret_ty_ident(ident: &ast::Ident) -> Result<TyKind, Vec<IError>> {
    match ident.name.as_str() {
        "int" => Ok(TyKind::Int),
        "float" => Ok(TyKind::Float),
        "str" => Ok(TyKind::Str),
        "bool" => Ok(TyKind::Bool),
        "char" => Ok(TyKind::Char),
        _ => Err(vec![IError::CannotFindTypeInScope {
            type_name: ident.name.as_str().to_string(),
            span: ident.span,
        }]),
    }
}

#[derive(Debug, Clone)]
pub struct Ty {
    pub kind: TyKind,
    pub span: Span,
}

impl Ty {
    pub fn to_string(&self) -> String {
        self.kind.to_string()
    }

    pub fn can_be_ident_type(&self) -> bool {
        self.kind.can_be_ident_type()
    }
}

#[derive(Debug, Clone)]
pub enum TyKind {
    Int,
    Float,
    Str,
    Bool,
    Function,
    Unit,
    Char,
    Array(Box<TyKind>, i64),
}

impl PartialEq for TyKind {
    fn eq(&self, other: &Self) -> bool {
        match self {
            TyKind::Int => match other {
                TyKind::Int => true,
                _ => false,
            },
            TyKind::Float => match other {
                TyKind::Float => true,
                _ => false,
            },
            TyKind::Str => match other {
                TyKind::Str => true,
                _ => false,
            },
            TyKind::Bool => match other {
                TyKind::Bool => true,
                _ => false,
            },
            TyKind::Function => match other {
                TyKind::Function => true,
                _ => false,
            },
            TyKind::Unit => match other {
                TyKind::Unit => true,
                _ => false,
            },
            TyKind::Char => match other {
                TyKind::Char => true,
                _ => false,
            },
            TyKind::Array(ty, len) => match other {
                TyKind::Array(other_ty, other_len) => {
                    // There are cases when the length is not known (-1)
                    ty == other_ty && (*len == -1 || *other_len == -1 || len == other_len)
                }
                _ => false,
            },
        }
    }
}

impl Display for TyKind {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", self.to_string())
    }
}

impl TyKind {
    pub fn is_iterable(&self) -> bool {
        match self {
            TyKind::Int => false,
            TyKind::Float => false,
            TyKind::Str => true,
            TyKind::Bool => false,
            TyKind::Function => false,
            TyKind::Unit => false,
            TyKind::Char => false,
            TyKind::Array(_, _) => true,
        }
    }
    pub fn is_clonable(&self) -> bool {
        match self {
            TyKind::Int => true,
            TyKind::Float => true,
            TyKind::Str => true,
            TyKind::Bool => true,
            TyKind::Function => false,
            TyKind::Unit => false,
            TyKind::Char => true,
            TyKind::Array(ty, _) => ty.is_clonable(),
        }
    }

    pub fn castable_to(&self, other: &TyKind) -> bool {
        match self {
            TyKind::Int => match other {
                TyKind::Int => true,
                TyKind::Float => true,
                _ => false,
            },
            TyKind::Float => match other {
                TyKind::Int => true,
                TyKind::Float => true,
                _ => false,
            },
            TyKind::Str => match other {
                TyKind::Str => true,
                _ => false,
            },
            TyKind::Bool => match other {
                TyKind::Bool => true,
                _ => false,
            },
            TyKind::Function => false,
            TyKind::Unit => false,
            TyKind::Char => match other {
                TyKind::Int => true,
                TyKind::Char => true,
                _ => false,
            },
            TyKind::Array(..) => false,
        }
    }

    pub fn can_be_ident_type(&self) -> bool {
        match self {
            TyKind::Int => true,
            TyKind::Float => true,
            TyKind::Str => true,
            TyKind::Bool => true,
            TyKind::Function => false,
            TyKind::Unit => false,
            TyKind::Char => true,
            TyKind::Array(ty, _) => ty.can_be_ident_type(),
        }
    }

    pub fn to_string(&self) -> String {
        match self {
            TyKind::Int => "int".to_string(),
            TyKind::Float => "float".to_string(),
            TyKind::Str => "str".to_string(),
            TyKind::Bool => "bool".to_string(),
            TyKind::Function => "function".to_string(),
            TyKind::Unit => "()".to_string(),
            TyKind::Char => "char".to_string(),
            TyKind::Array(ty, len) => {
                if *len == -1 {
                    format!("[{}]", ty.to_string())
                } else {
                    format!("[{}; {}]", ty.to_string(), len)
                }
            }
        }
    }
}
