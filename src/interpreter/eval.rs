use crate::{ast::Stmt, kw::Keyword, span_encoding::Span};

use super::{environment::Environment, errors::IError, ident::Ident, stmt, ty::TyKind, Ty};

#[derive(Debug, Clone)]
pub enum EvalResult {
    StmtResult(Option<ControlFlow>),
}

#[derive(Debug, Clone)]
pub enum ControlFlow {
    Continue,
    Break,
    Return(Value),
}

#[derive(Debug, Clone)]
pub struct Value {
    pub kind: ValueKind,
    pub span: Span,
}

impl Value {
    pub fn evaluate_function(
        env: &Environment,
        prefix_span: Span,
        function: ValueKind,
        evaluated_args: Vec<(Value, bool)>,
        is_verbose: bool,
    ) -> Result<ValueKind, Vec<IError>> {
        // TODO: handle mutable arguments
        let evaluated_args: Vec<Value> = evaluated_args.into_iter().map(|(val, _)| val).collect();
        match function {
            ValueKind::Function(func) => {
                let Func { sig, body } = func;
                let FuncSig {
                    ident,
                    inputs,
                    output,
                    span,
                } = sig;

                let Ident {
                    name: _func_name,
                    span: func_name_span,
                } = ident;

                let ret_ty_kind = if output.is_some() {
                    output.clone().unwrap().kind
                } else {
                    TyKind::Unit
                };
                // For return type hint if needed
                let after_sig_span = Span::after(span);

                let mut func_env = Environment::new_with_parent(env, is_verbose);
                func_env.in_function = true;
                let mut errors: Vec<IError> = Vec::new();

                let params = inputs
                    .iter()
                    .map(|param| {
                        let FuncParam {
                            ty,
                            ident,
                            is_mut,
                            span: _,
                        } = param;
                        (ident.clone(), ty.clone(), *is_mut)
                    })
                    .collect::<Vec<_>>();

                // FIX: not handle case where function argument is mutable
                let mut mismatch_params: Vec<(Span, String, String)> = vec![];
                for ((param_ident, param_ty, need_mut), arg) in
                    params.iter().zip(evaluated_args.clone())
                {
                    let Value { ref kind, span } = arg;
                    let arg_ty = kind.to_ty_kind();
                    if param_ty.kind != arg_ty {
                        mismatch_params.push((span, param_ty.to_string(), arg_ty.to_string()));
                    } else {
                        func_env.insert_variable(
                            param_ident.clone(),
                            Some(arg),
                            *need_mut,
                            param_ty.clone(),
                            Some(param_ident.span),
                        );
                    }
                }

                let mut missing_param_tys: Vec<String> = vec![];
                if params.len() > evaluated_args.len() {
                    for (_, ty, _) in params.iter().skip(evaluated_args.len()) {
                        missing_param_tys.push(ty.to_string());
                    }
                }

                let mut unexpected_param_tys: Vec<(Span, String)> = vec![];
                if params.len() < evaluated_args.len() {
                    for arg in evaluated_args.iter().skip(params.len()) {
                        unexpected_param_tys.push((arg.span, arg.kind.to_ty_kind().to_string()));
                    }
                }

                if !mismatch_params.is_empty() || !unexpected_param_tys.is_empty() {
                    errors.push(IError::FunctionParamMismatch {
                        func_decl_span: func_name_span.clone(),
                        args: evaluated_args.len(),
                        mismatch_params,
                        missing_param_tys,
                        unexpected_param_tys,
                        prefix_span,
                    });
                }

                if !errors.is_empty() {
                    return Err(errors);
                }

                let result = stmt::interpret_stmt(&mut func_env, &body, false, is_verbose)?;
                match result {
                    EvalResult::StmtResult(control_flow) => match control_flow {
                        Some(ControlFlow::Return(val)) => {
                            let val_ty_kind = val.to_ty_kind();
                            if val_ty_kind != ret_ty_kind {
                                if output.is_none() {
                                    errors.push(IError::MissingFunctionReturnType {
                                        found: val_ty_kind.to_string(),
                                        ret_help_span: after_sig_span,
                                        found_span: val.span,
                                    })
                                } else {
                                    let output = output.unwrap();
                                    errors.push(IError::MismatchedFunctionReturnType {
                                        expected: ret_ty_kind.to_string(),
                                        found: val_ty_kind.to_string(),
                                        expected_span: output.span,
                                        found_span: val.span,
                                    });
                                }
                            }
                            if errors.is_empty() {
                                Ok(val.kind)
                            } else {
                                Err(errors)
                            }
                        }
                        None => {
                            if ret_ty_kind != TyKind::Unit {
                                errors.push(IError::MissingReturnStatement {
                                    ret_kw: Keyword::Yeet.as_ref().to_string(),
                                    expected: ret_ty_kind.to_string(),
                                    expected_span: output.unwrap().span,
                                    func_decl_span: ident.span,
                                })
                            }
                            if errors.is_empty() {
                                Ok(ValueKind::Unit)
                            } else {
                                Err(errors)
                            }
                        }
                        _ => unreachable!("function body should not return continue or break"),
                    },
                }
            }
            _ => unreachable!("This should be a function"),
        }
    }

    pub fn to_ty_kind(&self) -> TyKind {
        self.kind.to_ty_kind()
    }

    pub fn try_cast_to(&self, ty: &TyKind) -> Result<Value, (String, String)> {
        self.kind.try_cast_to(ty).map(|ty| Value {
            kind: ty,
            span: self.span,
        })
    }

    pub fn into_iter(self) -> Result<Vec<Value>, String> {
        let span = self.span;
        match self.kind {
            ValueKind::Array(values) => Ok(values),
            ValueKind::Str(s) => Ok(s
                .chars()
                .map(|c| Value {
                    kind: ValueKind::Char(c),
                    span,
                })
                .collect()),
            _ => Err(format!("expected array or string, found {:?}", self)),
        }
    }
}

#[derive(Debug, Clone)]
pub enum ValueKind {
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
    pub sig: FuncSig,
    pub body: Box<Stmt>,
}

#[derive(Debug, Clone)]
pub struct FuncSig {
    pub ident: Ident,
    pub inputs: Vec<FuncParam>,
    pub output: Option<Ty>,

    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct FuncParam {
    pub ty: Ty,
    pub ident: Ident,
    pub is_mut: bool,
    pub span: Span,
}

impl ValueKind {
    pub fn try_cast_to(&self, ty: &TyKind) -> Result<ValueKind, (String, String)> {
        match self {
            ValueKind::Int(val) => match ty {
                TyKind::Int => Ok(ValueKind::Int(*val)),
                TyKind::Float => Ok(ValueKind::Float(*val as f64)),
                TyKind::Str => Ok(ValueKind::Str(val.to_string())),
                _ => Err((TyKind::Int.to_string(), ty.to_string())),
            },
            ValueKind::Float(val) => match ty {
                TyKind::Int => Ok(ValueKind::Int(*val as i64)),
                TyKind::Float => Ok(ValueKind::Float(*val)),
                TyKind::Str => Ok(ValueKind::Str(val.to_string())),
                _ => Err((TyKind::Float.to_string(), ty.to_string())),
            },
            ValueKind::Str(val) => match ty {
                TyKind::Str => Ok(ValueKind::Str(val.to_string())),
                TyKind::Int => val
                    .parse::<i64>()
                    .map(ValueKind::Int)
                    .map_err(|_| (val.to_string(), ty.to_string())),
                TyKind::Float => val
                    .parse::<f64>()
                    .map(ValueKind::Float)
                    .map_err(|_| (val.to_string(), ty.to_string())),
                TyKind::Bool => match val.as_str() {
                    "true" => Ok(ValueKind::Bool(true)),
                    "false" => Ok(ValueKind::Bool(false)),
                    _ => Err((val.to_string(), ty.to_string())),
                },
                _ => Err((TyKind::Str.to_string(), ty.to_string())),
            },
            ValueKind::Bool(val) => match ty {
                TyKind::Bool => Ok(ValueKind::Bool(*val)),
                TyKind::Int => Ok(ValueKind::Int(if *val { 1 } else { 0 })),
                TyKind::Str => Ok(ValueKind::Str(val.to_string())),
                _ => Err((TyKind::Bool.to_string(), ty.to_string())),
            },
            ValueKind::Char(val) => match ty {
                TyKind::Char => Ok(ValueKind::Char(*val)),
                TyKind::Int => Ok(ValueKind::Int(*val as i64)),
                TyKind::Str => Ok(ValueKind::Str(val.to_string())),
                _ => Err((TyKind::Char.to_string(), ty.to_string())),
            },
            ValueKind::Function(_) => Err((TyKind::Function.to_string(), ty.to_string())),
            ValueKind::Unit => Err((TyKind::Unit.to_string(), ty.to_string())),
            ValueKind::Array(_) => Err(("array".to_string(), ty.to_string())),
        }
    }

    pub fn to_ty_kind(&self) -> TyKind {
        match self {
            ValueKind::Int(_) => TyKind::Int,
            ValueKind::Float(_) => TyKind::Float,
            ValueKind::Str(_) => TyKind::Str,
            ValueKind::Char(_) => TyKind::Char,
            ValueKind::Bool(_) => TyKind::Bool,
            ValueKind::Function(_) => TyKind::Function,
            ValueKind::Array(values) => {
                let ty_kind = values.first().unwrap().to_ty_kind();
                let len = values.len() as i64;
                TyKind::Array(Box::new(ty_kind), len)
            }
            ValueKind::Unit => TyKind::Unit,
        }
    }
}
