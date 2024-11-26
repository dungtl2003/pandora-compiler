use std::rc::Rc;

use scope::{ContextManager, SematicScope, Wrapper};
use variable::Variable;

use crate::{
    ast::{Ast, Stmt, StmtKind},
    span_encoding::Span,
    symbol::Symbol,
};

mod expr;
mod item;
pub mod scope;
mod stmt;
mod table;
mod type_check;
pub mod variable;

pub struct SematicResolver {
    context: ContextManager,
}

impl SematicResolver {
    pub fn new() -> Self {
        SematicResolver {
            context: ContextManager::new(),
        }
    }

    pub fn parse<'ast>(mut self, ast: &'ast mut Ast) -> SResult<ContextManager> {
        let res = self.resolve(&mut ast.stmts);
        res.map(|_| self.context)
    }

    pub fn lookup_variable(
        &self,
        name: Symbol,
        span: Span,
        scope_id: &str,
    ) -> Option<Wrapper<Variable>> {
        self.context.lookup_variable(name, span, scope_id)
    }

    // FIX: Currently, this function does not care about the path.
    pub fn lookup_type(&self, name: Symbol, scope_id: Option<String>) -> Option<Ty> {
        self.context.lookup_type(name, scope_id)
    }

    fn resolve(&mut self, stmts: &mut Vec<Box<Stmt>>) -> SResult<()> {
        // Type resolution
        //self.resolve_items(stmts);

        // Name resolution
        self.resolve_remaining(stmts)
    }

    fn resolve_items(&mut self, stmts: &Vec<Box<Stmt>>) {
        todo!();
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

    pub fn resolve_variable(&self, name: Symbol, span: Span) -> Option<Wrapper<Variable>> {
        let binding: Wrapper<SematicScope> = Rc::clone(&self.context.current_scope);
        let binding = binding.borrow();
        let scope_id = binding.id.as_str();

        let level = self.context.level(scope_id) as isize;
        if level < 0 {
            return None;
        }

        let scope = self
            .context
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
        self.context.enter_new_scope();
    }

    fn exit_scope(&mut self) {
        self.context.previous_scope();
    }

    fn insert_variable(&self, variable: Variable) -> String {
        self.context.insert_variable(variable)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Ty {
    pub kind: TyKind,
}

impl Ty {
    pub fn is_primitive(ty: &str) -> bool {
        PrimTy::from_str_ty(ty).is_some()
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum TyKind {
    Prim(PrimTy),
    //Class(PointerType<'static>),
    //Interface(PointerType<'static>),
    Void,
}

#[derive(Debug, PartialEq, Clone)]
pub enum PrimTy {
    Int,
    Float,
    Bool,
}

impl PrimTy {
    pub fn to_str_ty(&self) -> &str {
        match self {
            PrimTy::Int => "int",
            PrimTy::Float => "float",
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
