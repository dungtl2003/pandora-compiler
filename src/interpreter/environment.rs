pub mod variable;

use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    rc::Rc,
    sync::Arc,
};

use miette::NamedSource;
use variable::Variable;

use crate::{
    ast::{self, Stmt},
    libs::{math::MathLib, std::StdLib, Library},
    parse::{lexer, parser},
    session::Session,
};

use super::{
    eval::{ControlFlow, EvalResult, Value},
    interpret_stmt_block, interpret_ty,
    ty::Ty,
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

    pub fn new_with_parent(parent: &Environment) -> Self {
        let mut env = Environment::new();

        let mut functions = vec![];
        let mut lib_names = HashSet::new();
        for scope in parent.scopes.iter() {
            functions.extend(scope.functions.clone());
            for name in scope.libraries.keys() {
                lib_names.insert(name.clone());
            }
        }

        for name in lib_names {
            env.import_library(&name).unwrap();
        }
        for (name, value) in functions {
            env.insert_function(name, value).unwrap();
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
    pub fn lookup_function(&self, name: &str) -> Option<Value> {
        self.scopes
            .iter()
            .rev()
            .find_map(|scope| scope.lookup_function(name))
    }

    pub fn lookup_library(&self, name: &str) -> Option<&Box<dyn Library>> {
        let lib_func = self
            .scopes
            .iter()
            .rev()
            .find_map(|scope| scope.lookup_library(name));

        if lib_func.is_none() {
            self.default_libs.get(name)
        } else {
            lib_func
        }
    }

    pub fn insert_variable(&mut self, name: &str, value: Option<Value>, is_mut: bool, ty: Ty) {
        let var = Variable {
            name: name.to_string(),
            is_mut,
            val: value,
            ty,
        };

        let var = Rc::new(RefCell::new(var));
        self.scopes.last_mut().unwrap().variables.push(var);
    }

    pub fn insert_function(&mut self, name: String, value: Value) -> Result<(), String> {
        // There must be no function with the same name in the current scope.
        if self.scopes.last().unwrap().functions.contains_key(&name) {
            return Err(format!(
                "Function with name '{}' already exists in this scope",
                name
            ));
        }

        self.scopes
            .last_mut()
            .unwrap()
            .functions
            .insert(name, value);
        Ok(())
    }

    pub fn import_library(&mut self, name: &str) -> Result<(), String> {
        if self.scopes.last().unwrap().libraries.contains_key(name) {
            return Err(format!(
                "There are multiple libraries with the same name '{}' in the same scope",
                name
            ));
        }

        let lib: Box<dyn Library> = if self.can_resolve_external_file(name) {
            self.load_external_library(name)?
        } else {
            self.load_embedded_library(name)?
        };

        self.scopes
            .last_mut()
            .unwrap()
            .libraries
            .insert(name.to_string(), lib);
        Ok(())
    }

    fn load_embedded_library(&mut self, name: &str) -> Result<Box<dyn Library>, String> {
        Ok(match name {
            "std" => Box::new(StdLib::new()),
            "math" => Box::new(MathLib::new()),
            _ => return Err(format!("Library '{}' not found", name)),
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

    fn load_external_library(&mut self, name: &str) -> Result<Box<dyn Library>, String> {
        let args: Vec<String> = std::env::args().collect();
        if args.len() < 2 {
            return Err("No source file specified".to_string());
        }

        let source_path = std::path::Path::new(&args[1]);
        let source_dir = source_path
            .parent()
            .ok_or_else(|| "Could not determine source file directory".to_string())?;

        let lib_filename = format!("{}.box", name);
        let lib_path = source_dir.join(&lib_filename);

        if !lib_path.exists() {
            return Err(format!("External library '{}' not found", name));
        }

        let contents = Arc::new(
            std::fs::read_to_string(&lib_path)
                .map_err(|_| format!("Failed to read library file '{}'", lib_path.display()))?,
        );

        let file = Arc::new(NamedSource::new(name, Arc::clone(&contents)));
        let session = Session::new(file);

        let tokens = lexer::lex_token_tree(&contents, &session)
            .map_err(|e| format!("Failed to lex library file '{}': {}", lib_path.display(), e))?;
        let ast = parser::parse(tokens, &session).map_err(|e| {
            format!(
                "Failed to parse library file '{}': {}",
                lib_path.display(),
                e
            )
        })?;

        let mut lib = Box::new(ExternalLibrary::new());

        for stmt in ast.stmts {
            match stmt.kind {
                ast::StmtKind::FuncDecl(fun) => {
                    let ast::Fun { sig, body } = *fun;
                    let ast::FunSig {
                        name,
                        inputs,
                        output: _,
                        span: _,
                    } = sig;

                    let mut params = vec![];
                    for input in inputs {
                        let ast::FunParam {
                            ty,
                            ident,
                            is_mut,
                            span: _,
                        } = input;

                        let ty = interpret_ty(&mut Environment::new(), &ty, false, false)?;
                        let name = ident.name.to_string();
                        params.push((name, ty, is_mut));
                    }

                    let stmts = match body.kind {
                        ast::StmtKind::Block(stmts) => stmts,
                        _ => {
                            unreachable!("This should be resolved within the parser");
                        }
                    };

                    let name = name.name.to_string();
                    lib.add_function(name, params, stmts)?;
                }
                _ => {
                    return Err(
                        "Only function declarations are allowed in external libraries".to_string(),
                    )
                }
            }
        }

        Ok(lib)
    }
}

pub struct ExternalLibrary {
    functions: HashMap<String, Box<dyn Fn(Vec<(Value, bool)>) -> Result<Value, String>>>,
}

impl Library for ExternalLibrary {
    fn get_function(
        &self,
        name: &str,
    ) -> Option<&Box<dyn Fn(Vec<(Value, bool)>) -> Result<Value, String>>> {
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
        params: Vec<(String, Ty, bool)>,
        body: Vec<Box<Stmt>>,
    ) -> Result<(), String> {
        let func_name = name.to_string();
        let function = Box::new(move |args: Vec<(Value, bool)>| -> Result<Value, String> {
            let mut env = Environment::new();
            env.in_function = true;

            if args.len() != params.len() {
                return Err(format!(
                    "Function '{}' expects {} arguments but got {}",
                    name,
                    params.len(),
                    args.len()
                ));
            }

            // FIX: handle mutable arguments
            for ((p_name, p_ty, _p_is_mut), (arg_val, arg_is_mut)) in params.iter().zip(args) {
                if *p_ty != arg_val.to_ty() {
                    return Err(format!(
                        "Function '{}' expects argument '{}' to be of type {:?} but got {:?}",
                        name,
                        p_name,
                        p_ty,
                        arg_val.to_ty()
                    ));
                }
                let var_name = p_name.to_string();
                let value = Some(arg_val);
                let is_arg_mut = arg_is_mut;
                let ty = p_ty.clone();
                env.insert_variable(var_name.as_str(), value, is_arg_mut, ty);
            }

            let result = interpret_stmt_block(&mut env, &body, false)?;
            match result {
                EvalResult::StmtResult(None) => Ok(Value::Unit),
                EvalResult::StmtResult(Some(control_flow)) => match control_flow {
                    ControlFlow::Break => {
                        return Err("break statement outside of loop".to_string());
                    }
                    ControlFlow::Continue => {
                        return Err("continue statement outside of loop".to_string());
                    }
                    ControlFlow::Return(value) => Ok(value),
                },
                EvalResult::Value(_) => {
                    unreachable!("statement should not return value");
                }
            }
        });

        self.functions.insert(func_name, function);
        Ok(())
    }
}

pub struct Scope {
    pub variables: Vec<Wrapper<Variable>>,
    pub libraries: HashMap<String, Box<dyn Library>>,
    pub functions: HashMap<String, Value>,
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
            .find(|var| var.borrow().name == name)
            .cloned()
    }

    pub fn lookup_function(&self, name: &str) -> Option<Value> {
        self.functions.get(name).cloned()
    }

    pub fn lookup_library(&self, name: &str) -> Option<&Box<dyn Library>> {
        self.libraries.get(name)
    }
}
