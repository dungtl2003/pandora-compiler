use std::rc::Rc;

use crate::{
    ast::{Ast, Ident, Path, Stmt, StmtKind},
    environment::{variable::Variable, Environment, Wrapper},
    span_encoding::Span,
    symbol::Symbol,
};

mod item;
mod stmt;
mod type_check;

pub struct SematicResolver {
    env: Environment,
}

impl SematicResolver {
    pub fn new() -> Self {
        SematicResolver {
            env: Environment::new(),
        }
    }

    pub fn parse<'ast>(mut self, ast: &'ast mut Ast) -> SResult<Environment> {
        let res = self.resolve(&mut ast.stmts);
        res.map(|_| self.env)
    }

    fn resolve(&mut self, stmts: &mut Vec<Box<Stmt>>) -> SResult<()> {
        // Type resolution
        //self.resolve_items(stmts);

        // Name resolution
        self.resolve_remaining(stmts)
    }

    fn resolve_items(&mut self, stmts: &Vec<Box<Stmt>>) {
        for stmt in stmts {
            if let Stmt {
                kind: StmtKind::Item(item),
                ..
            } = stmt.as_ref()
            {}
        }
    }

    fn resolve_remaining(&mut self, stmts: &mut Vec<Box<Stmt>>) -> SResult<()> {
        for stmt in stmts {
            if let Stmt {
                kind: StmtKind::Item(_),
                ..
            } = stmt.as_ref()
            {
                continue;
            }

            self.resolve_stmt(stmt)?;
        }
        Ok(())
    }

    fn resolve_in_new_scope(
        &mut self,
        resolver: impl FnOnce(&mut Self) -> SResult<()>,
    ) -> SResult<()> {
        self.enter_scope();
        let res = resolver(self);
        self.exit_scope();
        res
    }

    // FIX: Currently, we only take the last segment of the path.
    pub fn resolve_variable_from_path(&self, path: &Box<Path>) -> Option<Wrapper<Variable>> {
        let Path { segments, .. } = path.as_ref();
        let last_segment = segments.last().unwrap();
        let Ident { name, span, .. } = last_segment.ident.clone();

        self.resolve_variable(name, span)
    }

    pub fn resolve_item_from_path(&self, path: &Box<Path>) -> Option<Ty> {
        let Path { segments, .. } = path.as_ref();
        let last_segment = segments.last().unwrap();
        let Ident { name, span: _, .. } = last_segment.ident.clone();

        self.resolve_type(name)
    }

    pub fn resolve_type(&self, name: Symbol) -> Option<Ty> {
        let binding: Wrapper<SematicScope> = Rc::clone(&self.env.current_scope);
        let binding = binding.borrow();
        let scope_id = binding.id.as_str();

        let level = self.env.level(scope_id) as isize;
        if level < 0 {
            return None;
        }

        let scope = self
            .env
            .scopes
            .get(level as usize)
            .unwrap()
            .iter()
            .find(|scope| scope.borrow().id == scope_id)
            .unwrap();

        let ty = scope.borrow().lookup_type(name);

        if ty.is_none() {
            let parent_id = binding.prefix_id();
            self.lookup_type(name, &parent_id)
        } else {
            ty
        }
    }

    pub fn resolve_variable(&self, name: Symbol, span: Span) -> Option<Wrapper<Variable>> {
        let binding: Wrapper<SematicScope> = Rc::clone(&self.env.current_scope);
        let binding = binding.borrow();
        let scope_id = binding.id.as_str();

        let level = self.env.level(scope_id) as isize;
        if level < 0 {
            return None;
        }

        let scope = self
            .env
            .scopes
            .get(level as usize)
            .unwrap()
            .iter()
            .find(|scope| scope.borrow().id == scope_id)
            .unwrap();

        let variable = scope.borrow().lookup_variable(name, span);

        if variable.is_none() {
            let parent_id = binding.prefix_id();
            self.lookup_variable(name, span, &parent_id)
        } else {
            variable
        }
    }

    fn enter_scope(&mut self) {
        self.env.enter_new_scope();
    }

    fn exit_scope(&mut self) {
        self.env.previous_scope();
    }

    fn insert_variable(&self, variable: Variable) -> String {
        self.env.insert_variable(variable)
    }

    fn insert_type(&self, ty: Ty) -> String {
        self.env.insert_type(ty)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Ty {
    pub kind: TyKind,
}

impl Ty {
    pub fn is_primitive(&self) -> bool {
        self.kind.is_primitive()
    }

    pub fn to_rust_ty_str(&self) -> String {
        match &self.kind {
            TyKind::Prim(prim_ty) => prim_ty.to_rust_str_ty().to_string(),
            TyKind::Void => "()".to_string(),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum TyKind {
    Prim(PrimTy),
    //Class(PointerType<'static>),
    //Interface(PointerType<'static>),
    Void,
}

impl TyKind {
    pub fn is_primitive(&self) -> bool {
        match self {
            TyKind::Prim(_) => true,
            _ => false,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum PrimTy {
    Int,
    Float,
    Bool,
}

impl PrimTy {
    pub fn can_cast_to(&self, other: &PrimTy) -> bool {
        match self {
            PrimTy::Int => match other {
                PrimTy::Int => true,
                PrimTy::Float => true,
                _ => false,
            },
            PrimTy::Float => match other {
                PrimTy::Int => true,
                PrimTy::Float => true,
                _ => false,
            },
            PrimTy::Bool => match other {
                PrimTy::Bool => true,
                _ => false,
            },
        }
    }

    pub fn to_str_ty(&self) -> &str {
        match self {
            PrimTy::Int => "int",
            PrimTy::Float => "float",
            PrimTy::Bool => "bool",
        }
    }

    pub fn to_rust_str_ty(&self) -> &str {
        match self {
            PrimTy::Int => "i64",
            PrimTy::Float => "f64",
            PrimTy::Bool => "bool",
        }
    }

    pub fn from_str_ty(s: &str) -> Option<Self> {
        match s {
            "int" => Some(PrimTy::Int),
            "float" => Some(PrimTy::Float),
            "bool" => Some(PrimTy::Bool),
            _ => None,
        }
    }
}

type SErr = String;
type SResult<T> = Result<T, SErr>;
