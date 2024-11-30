use crate::ast::Stmt;

use super::ty::Ty;

pub enum EvalResult {
    Value(Value),
    StmtResult(Option<ControlFlow>),
}

pub enum ControlFlow {
    Continue,
    Break,
    Return(Value),
}

#[derive(Debug, Clone)]
pub enum Value {
    Int(i64),
    Float(f64),
    Str(String),
    Bool(bool),
    Function(Func),
    Char(char),
    Array(Vec<Value>),
    Unit,
}

#[derive(Debug, Clone)]
pub struct Func {
    pub name: String,
    pub args: Vec<(String, Ty, bool)>, // (name, type, mutable)
    pub stmts: Vec<Box<Stmt>>,
    pub ret_ty: Ty,
}

impl Value {
    pub fn into_iter(self) -> Result<Vec<Value>, String> {
        match self {
            Value::Array(values) => Ok(values),
            Value::Str(s) => Ok(s.chars().map(Value::Char).collect()),
            _ => Err(format!("expected array or string, found {:?}", self)),
        }
    }

    pub fn to_bool(&self) -> Result<bool, String> {
        match self {
            Value::Bool(val) => Ok(*val),
            _ => Err(format!("expected bool, found {:?}", self)),
        }
    }

    pub fn try_cast_to(&self, ty: &Ty) -> Result<Value, String> {
        match self {
            Value::Int(val) => match ty {
                Ty::Int => Ok(Value::Int(*val)),
                Ty::Float => Ok(Value::Float(*val as f64)),
                Ty::Str => Ok(Value::Str(val.to_string())),
                _ => Err(format!("cannot cast {} to {}", self.to_ty(), ty)),
            },
            Value::Float(val) => match ty {
                Ty::Int => Ok(Value::Int(*val as i64)),
                Ty::Float => Ok(Value::Float(*val)),
                Ty::Str => Ok(Value::Str(val.to_string())),
                _ => Err(format!("cannot cast {} to {}", self.to_ty(), ty)),
            },
            Value::Str(val) => match ty {
                Ty::Str => Ok(Value::Str(val.to_string())),
                Ty::Int => val
                    .parse::<i64>()
                    .map(Value::Int)
                    .map_err(|_| format!(r#"cannot cast string "{}" to type {}"#, val, ty)),
                Ty::Float => val
                    .parse::<f64>()
                    .map(Value::Float)
                    .map_err(|_| format!(r#"cannot cast string "{}" to type {}"#, val, ty)),
                Ty::Bool => match val.as_str() {
                    "true" => Ok(Value::Bool(true)),
                    "false" => Ok(Value::Bool(false)),
                    _ => Err(format!(r#"cannot cast string "{}" to type {}"#, val, ty)),
                },
                _ => Err(format!("cannot cast {} to {}", self.to_ty(), ty)),
            },
            Value::Bool(val) => match ty {
                Ty::Bool => Ok(Value::Bool(*val)),
                Ty::Int => Ok(Value::Int(if *val { 1 } else { 0 })),
                Ty::Str => Ok(Value::Str(val.to_string())),
                _ => Err(format!("cannot cast {} to {}", self.to_ty(), ty)),
            },
            Value::Char(val) => match ty {
                Ty::Char => Ok(Value::Char(*val)),
                Ty::Int => Ok(Value::Int(*val as i64)),
                Ty::Str => Ok(Value::Str(val.to_string())),
                _ => Err(format!("cannot cast {} to {}", self.to_ty(), ty)),
            },
            Value::Function(_) => Err(format!("cannot cast function")),
            Value::Unit => Err(format!("cannot cast unit")),
            Value::Array(_) => Err(format!("cannot cast array")),
        }
    }

    pub fn to_ty(&self) -> Ty {
        match self {
            Value::Int(_) => Ty::Int,
            Value::Float(_) => Ty::Float,
            Value::Str(_) => Ty::Str,
            Value::Char(_) => Ty::Char,
            Value::Bool(_) => Ty::Bool,
            Value::Function(_) => Ty::Function,
            Value::Array(values) => {
                let ty = values.first().unwrap().to_ty();
                let len = values.len() as i64;
                Ty::Array(Box::new(ty), len)
            }
            Value::Unit => Ty::Unit,
        }
    }

    pub fn match_ty(&self, ty: &Ty) -> bool {
        self.to_ty() == *ty
    }
}
