use std::cell::{RefCell, RefMut};

use scope::SematicScope;
use ty::{Ty, TyKind};
use type_check::path::PathStyle;
use variable::{BindingMode, Mutability, Variable};

use crate::{
    ast::{Ast, Local, LocalKind, Stmt, StmtKind},
    span_encoding::Span,
    symbol::Symbol,
};

mod expr;
mod item;
mod scope;
mod table;
mod ty;
mod type_check;
mod variable;

pub struct SematicResolver<'ast> {
    scopes: Vec<RefCell<SematicScope>>,
    current_depth: usize,
    ast: &'ast Ast,
}

impl<'ast> SematicResolver<'ast> {
    pub fn new(ast: &'ast Ast) -> Self {
        SematicResolver {
            scopes: vec![RefCell::new(SematicScope::new(0))],
            current_depth: 0,
            ast,
        }
    }

    pub fn parse(mut self) {
        self.resolve(&self.ast.stmts);
    }

    pub fn lookup_variable(&mut self, name: Symbol, span: Span) -> Option<Variable> {
        let mut depth: isize = self.current_depth as isize;
        loop {
            if depth < 0 {
                return None;
            }

            let scope = self.scope(depth.try_into().unwrap());
            let variable = scope.lookup_variable(name, span);

            if variable.is_some() {
                return variable;
            }

            depth -= 1;
        }
    }

    // FIX: Currently, this function does not care about the path.
    pub fn lookup_type(&mut self, name: Symbol) -> Option<Ty> {
        self.scope(self.current_depth).lookup_type(name)
    }

    fn resolve(&mut self, stmts: &Vec<Box<Stmt>>) {
        // Type resolution
        //self.resolve_items(stmts);

        // Name resolution
        self.resolve_remaining(stmts);
    }

    fn resolve_items(&mut self, stmts: &Vec<Box<Stmt>>) {
        todo!();
    }

    fn resolve_remaining(&mut self, stmts: &Vec<Box<Stmt>>) {
        for stmt in stmts {
            if let Stmt {
                kind: StmtKind::Item(_),
                ..
            } = stmt.as_ref()
            {
                continue;
            }

            self.resolve_stmt(stmt);
        }
    }

    fn resolve_stmt(&mut self, stmt: &Box<Stmt>) {
        let Stmt { kind, .. } = stmt.as_ref();
        match kind {
            StmtKind::Block(block) => self.resolve_block(block),
            StmtKind::Var(local) => self.resolve_local(local),
            StmtKind::Expr(_expr) => {}
            StmtKind::Empty => {}
            _ => todo!(),
        }
    }

    fn resolve_local(&mut self, local: &Box<Local>) {
        let Local {
            binding_mode,
            ident,
            ty,
            kind: _,
            span,
        } = local.as_ref();

        let binding_mode = BindingMode(match binding_mode.0 {
            crate::ast::Mutability::Mutable => Mutability::Mutable,
            crate::ast::Mutability::Immutable => Mutability::Immutable,
        });

        // FIX: This is not the correct way to get the type of the variable.
        let ty_kind = match ty.kind {
            crate::ast::TyKind::Path(ref path) => self.get_path_ty(path, PathStyle::Type).unwrap(),
            crate::ast::TyKind::Never => TyKind::Void,
        };

        let ty = Ty {
            kind: ty_kind.clone(),
        };

        if let LocalKind::Init(expr) = &local.kind {
            let expr_ty = self.get_expr_ty(expr).unwrap();
            if ty_kind != expr_ty {
                eprintln!("Type mismatch: expected {:?}, found {:?}", ty, expr_ty);
            }
        }

        let variable = Variable {
            name: ident.name,
            ty,
            span: span.clone(),
            binding_mode,
        };

        self.insert_variable(variable);
    }

    fn resolve_block(&mut self, stmts: &Vec<Box<Stmt>>) {
        self.resolve_in_new_scope(|resolver| {
            resolver.resolve(stmts);
        });
    }

    fn resolve_in_new_scope(&mut self, resolver: impl FnOnce(&mut Self)) {
        self.enter_scope();
        resolver(self);
        self.exit_scope();
    }

    fn enter_scope(&mut self) {
        self.current_depth += 1;
        if self.current_depth >= self.scopes.len() {
            self.scopes
                .push(RefCell::new(SematicScope::new(self.current_depth)));
        }
    }

    fn exit_scope(&mut self) {
        self.current_depth -= 1;
    }

    fn scope(&self, depth: usize) -> RefMut<SematicScope> {
        self.scopes.get(depth).unwrap().borrow_mut()
    }

    fn insert_variable(&mut self, variable: Variable) {
        self.scope(self.current_depth).insert_variable(variable);
    }
}

type SErr = String;
type SResult<T> = Result<T, SErr>;
