pub mod variable;

use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    rc::Rc,
    sync::Arc,
};

use miette::NamedSource;
use variable::Variable;

use crate::{ast, parse::parser, session::Session, span_encoding::Span};

use super::{
    errors::IError,
    eval::ValueKind,
    ident::Ident,
    interpret_ty,
    libs::{math::MathLib, std::StdLib, CallerAttrs, Library},
    Func, FuncParam, FuncSig, Ty, Value,
};

pub type Wrapper<T> = Rc<RefCell<T>>;

pub struct Environment {
    pub scopes: Vec<Scope>,
    pub in_function: bool,
    pub default_libs: HashMap<String, Box<dyn Library>>,
}

impl Environment {
    pub fn new() -> Self {
        let mut default_libs: HashMap<String, Box<dyn Library>> = HashMap::new();
        default_libs.insert("std".to_string(), Box::new(StdLib::new()));

        Environment {
            scopes: vec![Scope::new()],
            in_function: false,
            default_libs,
        }
    }

    pub fn new_with_parent(parent: &Environment, is_verbose: bool) -> Self {
        let mut env = Environment::new();

        let mut functions = vec![];
        let mut has_lib = HashSet::new();
        let mut libs = vec![];
        for scope in parent.scopes.iter() {
            functions.extend(scope.functions.clone());
            for (name, (span, _)) in scope.libraries.iter() {
                if !has_lib.contains(name) {
                    has_lib.insert(name.to_string());
                    libs.push((name.to_string(), span.clone()));
                }
            }
        }

        for (name, span) in libs {
            env.import_library(&name, span, is_verbose).unwrap();
        }
        for (name, (span, value)) in functions {
            env.insert_function(name, value, span, is_verbose).unwrap();
        }

        env
    }

    pub fn push_scope(&mut self) {
        self.scopes.push(Scope::new());
    }

    pub fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    /// Lookup the nearest variable with the given name.
    pub fn lookup_variable(&self, name: &str) -> Option<Wrapper<Variable>> {
        self.scopes
            .iter()
            .rev()
            .find_map(|scope| scope.lookup_variable(name))
    }

    /// Lookup the nearest function with the given name.
    pub fn lookup_function(&self, name: &str) -> Option<(Span, ValueKind)> {
        self.scopes
            .iter()
            .rev()
            .find_map(|scope| scope.lookup_function(name))
    }

    pub fn lookup_library(&self, name: &str) -> Option<&(Span, Box<dyn Library>)> {
        self.scopes
            .iter()
            .rev()
            .find_map(|scope| scope.lookup_library(name))
    }

    pub fn lookup_default_library(&self, name: &str) -> Option<&Box<dyn Library>> {
        self.default_libs.get(name)
    }

    pub fn insert_variable(
        &mut self,
        ident: Ident,
        value: Option<Value>,
        is_mut: bool,
        ty: Ty,
        first_assigned_span: Option<Span>,
    ) {
        let var = Variable {
            ident,
            is_mut,
            val: value,
            ty,
            first_assigned_span,
        };

        let var = Rc::new(RefCell::new(var));
        self.scopes.last_mut().unwrap().variables.push(var);
    }

    pub fn insert_function(
        &mut self,
        name: String,
        value: ValueKind,
        span: Span, // declaration span
        _is_verbose: bool,
    ) -> Result<(), Vec<IError>> {
        // There must be no function with the same name in the current scope.
        if self.scopes.last().unwrap().functions.contains_key(&name) {
            let first_decl_span = self.scopes.last().unwrap().functions[&name].0;
            return Err(vec![IError::FunctionAlreadyDeclaredInScope {
                func_name: name,
                first_decl_span,
                second_decl_span: span,
            }]);
        }

        self.scopes
            .last_mut()
            .unwrap()
            .functions
            .insert(name, (span, value));
        Ok(())
    }

    pub fn import_library(
        &mut self,
        name: &str,
        span: Span,
        is_verbose: bool,
    ) -> Result<(), Vec<IError>> {
        if self.scopes.last().unwrap().libraries.contains_key(name) {
            let first_import_span = self.scopes.last().unwrap().libraries[name].0;
            return Err(vec![IError::MultipleLibrariesInScope {
                lib_name: name.to_string(),
                first_lib_span: first_import_span,
                second_lib_span: span,
            }]);
        }

        let lib: Box<dyn Library> = if self.can_resolve_external_file(name) {
            self.load_external_library(name, span, is_verbose)?
        } else {
            self.load_embedded_library(name, span, is_verbose)?
        };

        self.scopes
            .last_mut()
            .unwrap()
            .libraries
            .insert(name.to_string(), (span, lib));
        Ok(())
    }

    fn load_embedded_library(
        &mut self,
        name: &str,
        span: Span,
        _is_verbose: bool,
    ) -> Result<Box<dyn Library>, Vec<IError>> {
        Ok(match name {
            "std" => Box::new(StdLib::new()),
            "math" => Box::new(MathLib::new()),
            _ => {
                return Err(vec![IError::LibraryNotFound {
                    library: name.to_string(),
                    span,
                }])
            }
        })
    }

    fn can_resolve_external_file(&self, name: &str) -> bool {
        let args: Vec<String> = std::env::args().collect();
        if args.len() < 2 {
            return false;
        }

        let source_path = std::path::Path::new(&args[1]);
        let source_dir = source_path.parent();
        if source_dir.is_none() {
            return false;
        }

        let source_dir = source_dir.unwrap();
        let lib_filename = format!("{}.boxx", name);
        let lib_path = source_dir.join(&lib_filename);

        if !lib_path.exists() {
            return false;
        }

        true
    }

    fn load_external_library(
        &mut self,
        name: &str,
        span: Span,
        is_verbose: bool,
    ) -> Result<Box<dyn Library>, Vec<IError>> {
        let args: Vec<String> = std::env::args().collect();
        if args.len() < 2 {
            return Err(vec![IError::NoSourceFileSpecified]);
        }

        let source_path = std::path::Path::new(&args[1]);
        let source_dir = source_path
            .parent()
            .ok_or_else(|| vec![IError::DirectoryNotFound])?;

        let is_genz = crate::is_genz_mode();
        let lib_filename = if is_genz {
            format!("{}.unbxx", name)
        } else {
            format!("{}.boxx", name)
        };

        let lib_path = source_dir.join(&lib_filename);
        if !lib_path.exists() {
            return Err(vec![IError::ExternalLibraryNotFound {
                lib_name: name.to_string(),
                span,
            }]);
        }

        let contents = Arc::new(std::fs::read_to_string(&lib_path).map_err(|_| {
            vec![IError::ReadLibraryFileFailed {
                path: lib_path.display().to_string(),
            }]
        })?);

        let file = Arc::new(NamedSource::new(name, Arc::clone(&contents)));
        let mut session = Session::new(file);

        let ast = parser::parse(&contents, &mut session);
        if ast.is_none() {
            return Err(vec![IError::ParseLibraryFileFailed {
                span,
                path: lib_path.display().to_string(),
            }]);
        }

        let mut lib = Box::new(ExternalLibrary::new());

        let ast = ast.unwrap();
        for stmt in ast.stmts {
            match stmt.kind {
                ast::StmtKind::FuncDecl(fun) => {
                    let ast::Fun { sig, body } = *fun;
                    let ast::FunSig {
                        name,
                        inputs,
                        output,
                        span,
                    } = sig;

                    let ident = Ident {
                        name: name.name.to_string(),
                        span: name.span,
                    };

                    let output = if output.is_some() {
                        Some(interpret_ty(
                            &mut Environment::new(),
                            &output.unwrap(),
                            false,
                            is_verbose,
                        )?)
                    } else {
                        None
                    };

                    let inputs = inputs
                        .iter()
                        .map(|param| {
                            let ast::FunParam {
                                ty,
                                ident,
                                is_mut,
                                span,
                            } = param;

                            let ty = interpret_ty(&mut Environment::new(), &ty, false, is_verbose)?;
                            let ident = Ident {
                                name: ident.name.as_str().to_string(),
                                span: ident.span,
                            };

                            Ok(FuncParam {
                                ty,
                                ident,
                                is_mut: *is_mut,
                                span: *span,
                            })
                        })
                        .collect::<Result<Vec<FuncParam>, Vec<IError>>>()?;

                    let sig = FuncSig {
                        ident,
                        inputs,
                        output,
                        span,
                    };

                    let eval_function = ValueKind::Function(Func { sig, body });

                    let name = name.name.to_string();
                    lib.add_function(name, eval_function, is_verbose)?;
                }
                _ => return Err(vec![IError::NonFunctionDeclaredInExternalLibrary { span }]),
            }
        }

        Ok(lib)
    }
}

pub struct ExternalLibrary {
    functions: HashMap<
        String,
        Box<dyn Fn(CallerAttrs, Vec<(Value, bool)>) -> Result<ValueKind, Vec<IError>>>,
    >,
}

impl Library for ExternalLibrary {
    fn get_function(
        &self,
        name: &str,
    ) -> Option<&Box<dyn Fn(CallerAttrs, Vec<(Value, bool)>) -> Result<ValueKind, Vec<IError>>>>
    {
        self.functions.get(name)
    }
}

impl ExternalLibrary {
    pub fn new() -> Self {
        ExternalLibrary {
            functions: HashMap::new(),
        }
    }

    pub fn add_function(
        &mut self,
        name: String,
        eval: ValueKind,
        is_verbose: bool,
    ) -> Result<(), Vec<IError>> {
        let func_name = name.to_string();
        let function = Box::new(
            move |cattrs: CallerAttrs,
                  args: Vec<(Value, bool)>|
                  -> Result<ValueKind, Vec<IError>> {
                let mut env = Environment::new();

                Value::evaluate_function(
                    &mut env,
                    cattrs.prefix_span,
                    eval.clone(),
                    args,
                    is_verbose,
                )
                // FIX: we actually want to use errors, but the error handler is not emitting
                // errors correctly when dealing with multiple files
                .map_err(|_errs| {
                    vec![IError::PredefinedError {
                        span: cattrs.span,
                        message: "Error occurred while evaluating function".to_string(),
                    }]
                })
            },
        );

        self.functions.insert(func_name, function);
        Ok(())
    }
}

pub struct Scope {
    pub variables: Vec<Wrapper<Variable>>,
    pub libraries: HashMap<String, (Span, Box<dyn Library>)>, // (import span, library)
    pub functions: HashMap<String, (Span, ValueKind)>,        // (declaration span, function)
}

impl Scope {
    pub fn new() -> Self {
        Scope {
            variables: Vec::new(),
            libraries: HashMap::new(),
            functions: HashMap::new(),
        }
    }

    /// Lookup the nearest variable with the given name.
    pub fn lookup_variable(&self, name: &str) -> Option<Wrapper<Variable>> {
        self.variables
            .iter()
            .rev()
            .find(|var| var.borrow().ident.name == name)
            .cloned()
    }

    pub fn lookup_function(&self, name: &str) -> Option<(Span, ValueKind)> {
        self.functions.get(name).cloned()
    }

    pub fn lookup_library(&self, name: &str) -> Option<&(Span, Box<dyn Library>)> {
        self.libraries.get(name)
    }
}
