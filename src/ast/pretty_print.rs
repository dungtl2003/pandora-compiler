use crate::span_encoding::Span;
use super::{AngleBracketedArgs, ClassBody, Expr, ExprKind, ExtClause, FunParam, FunRetTy, FunSig, GenericArgs, GenericParam, Ident, ImplClause, InterfaceBody, Item, Local, LocalKind, Mutability, Path, PathSegment, SelfKind, SelfParam, Stmt, StmtKind, Ty, TyKind, Visibility, VisibilityKind};
use crate::visitor::Visitor;

pub struct Printer {
    pub output: String,
    pub indent: usize,
    pub indent_spaces: usize,
}

impl Printer {
    pub fn new() -> Self {
        Printer {
            output: String::new(),
            indent: 0,
            indent_spaces: 2,
        }
    }

    pub fn print_stmts(&mut self, stmts: &Vec<Box<Stmt>>) {
        for stmt in stmts {
            self.print_stmt(&stmt);
        }
    }

    pub fn print_path(&mut self, path: &Path) {
        self.visit_path(path);
    }

    pub fn print_stmt(&mut self, stmt: &Stmt) {
        self.visit_stmt(stmt);
    }

    pub fn print_items(&mut self, items: &Vec<Box<Item>>) {
        for item in items {
            self.print_item(&item);
        }
    }

    pub fn print_item(&mut self, item: &Item) {
        self.visit_item(item);
    }
}

impl<'ast> Visitor<'ast> for Printer {

    fn visit_item_fun(&mut self, generics: &'ast Vec<GenericParam>, sig: &'ast FunSig, body: Option<&'ast Stmt>, span: &'ast Span, vis: Option<&'ast Visibility>, ident: &'ast Ident) {
        self.output
            .push_str(&format!("{}Function: {}\n", space(self.indent), span));

        self.indent += self.indent_spaces;
        self.output.push_str(&format!(
            "{}Visibility: {}\n",
            space(self.indent),
            if vis.is_some() { "public" } else { "private" }
        ));
        self.output.push_str(&format!(
            "{}Identifier: {}\n",
            space(self.indent),
            ident.name
        ));

        self.output.push_str(&format!(
            "{}Generics: \n",
            space(self.indent)
        ));

        self.indent += self.indent_spaces;
        for generic in generics {
            self.output.push_str(&format!(
                "{}Ident: {} - {}\n",
                space(self.indent),
                generic.ident.span,
                generic.ident.name
            ));

            self.indent += self.indent_spaces;
            self.output.push_str(&format!(
                "{}Bounds: \n",
                space(self.indent),
            ));

            self.indent += self.indent_spaces;
            for Ty {kind,span} in generic.bounds.iter() {
                match kind {
                    TyKind::Never => {
                        self.output.push_str(&format!(
                            "{}{} - void\n",
                            space(self.indent),
                            span,
                        ));
                    },
                    TyKind::Path(path) => {
                        self.visit_path(&path);
                    }
                }
            }
            self.indent -= self.indent_spaces;
            self.indent -= self.indent_spaces;
        }
        self.indent -= self.indent_spaces;

        self.output.push_str(&format!(
            "{}Inputs: \n",
            space(self.indent),
        ));
        self.indent += self.indent_spaces;
        let FunSig{inputs,output,span} = sig;
        let (self_param, params) = inputs;
        if let  Some(SelfParam{kind: self_kind, span: self_span}) = self_param{
            self.output.push_str(&format!(
                "{}Self: {} - ",
                space(self.indent),
                self_span
            ));
            match self_kind {
                SelfKind::Value(mutability) => {
                    self.output.push_str(&format!("{}\n", mutability));
                },
                SelfKind::Explicit(ty,mutability) => {
                    self.output.push_str(&format!("{} {} - {}\n", mutability, ty.kind, ty.span));
                }
            };
        }

        for FunParam{span,ty,ident} in params {
            self.output.push_str(&format!("{}param: {}\n", space(self.indent), span));
            self.indent += self.indent_spaces; //3
            self.output.push_str(&format!("{}ident: {} - {}\n", space(self.indent), ident.span, ident.name));
            // self.output.push_str(&format!("{}type: {} - {}\n", space(self.indent), param.ty.span, param.ty.kind));
            let kind= &ty.kind;
            match kind {
                TyKind::Never => {
                    self.output.push_str(&format!(
                        "{}type: {} - void\n",
                        space(self.indent),
                        span,
                    ));
                },
                TyKind::Path(path) => {
                    self.output.push_str(&format!(
                        "{}type:\n",
                        space(self.indent),
                    ));
                    self.indent += self.indent_spaces;
                    self.visit_path(&path);
                    self.indent -= self.indent_spaces;
                }
            }
            self.indent -= self.indent_spaces;
        };
        self.indent -= self.indent_spaces;

        self.output.push_str(&format!(
            "{}Output: ",
            space(self.indent),
        ));
        match output {
            FunRetTy::Default(span) => self.output.push_str(&format!("{} - void\n", span)),
            FunRetTy::Ty(ty) => {
                // self.output.push_str(&format!("{} - {}\n", ty.span, ty.kind));
                let kind = &ty.kind;
                match kind {
                    TyKind::Never => {
                        self.output.push_str(&format!(
                            "{}{} - void\n",
                            space(self.indent),
                            span,
                        ));
                    },
                    TyKind::Path(path) => {
                        self.output.push_str("\n");
                        self.indent += self.indent_spaces;
                        self.visit_path(&path);
                        self.indent -= self.indent_spaces;
                    }
                }
            },

        }

        let stmts = if let Some(Stmt {kind:body_kind,span:_}) = body {
            match body_kind {
                StmtKind::Block(stmts) => stmts,
                _ => &vec![]
            }
        } else {
            &vec![]
        };

        self.output.push_str(&format!("{}Body: \n", space(self.indent)));
        self.indent += self.indent_spaces;
        self.visit_stmt_block(&stmts);
        self.indent -= self.indent_spaces;

        self.indent -= self.indent_spaces;

    }

    fn visit_item_class(&mut self, generics: &'ast Vec<GenericParam>, ext_clause: Option<&'ast ExtClause>, impl_clause: Option<&'ast ImplClause>, body: &'ast ClassBody, span: &'ast Span, vis: Option<&'ast Visibility>, ident: &'ast Ident) {
        todo!()
    }

    fn visit_item_interface(&mut self, generics: &'ast Vec<GenericParam>, ext_clause: Option<&'ast ExtClause>, body: &'ast InterfaceBody, span: &'ast Span, vis: Option<&'ast Visibility>, ident: &'ast Ident) {
        todo!()
    }
    fn visit_stmt_for(&mut self, ident: &'ast Ident, expr: &'ast Expr, block: &'ast Stmt) {
        self.output
            .push_str(&format!("{}For statement:\n", space(self.indent)));
        self.indent += self.indent_spaces;
        self.output.push_str(&format!(
            "{}Identifier: {}\n",
            space(self.indent),
            ident.name
        ));

        self.output
            .push_str(&format!("{}Expression:\n", space(self.indent)));
        self.visit_expr(expr);

        self.output
            .push_str(&format!("{}Block:\n", space(self.indent)));
        self.indent += self.indent_spaces;
        self.visit_stmt(block);
        self.indent -= self.indent_spaces;
        self.indent -= self.indent_spaces;
    }

    fn visit_stmt_while(&mut self, condition: &'ast Expr, block: &'ast Stmt) {
        self.output
            .push_str(&format!("{}While statement:\n", space(self.indent),));
        self.indent += self.indent_spaces;
        self.output
            .push_str(&format!("{}Condition:\n", space(self.indent)));
        self.visit_expr(condition);

        self.output
            .push_str(&format!("{}Block:\n", space(self.indent)));
        self.indent += self.indent_spaces;
        self.visit_stmt(block);
        self.indent -= self.indent_spaces;
        self.indent -= self.indent_spaces;
    }

    fn visit_stmt_if(&mut self, condition: &'ast Expr, block: &'ast Stmt, optional_else: Option<&'ast Stmt>) {
        self.output
            .push_str(&format!("{}If statement:\n", space(self.indent),));
        self.indent += self.indent_spaces;
        self.output
            .push_str(&format!("{}Condition:\n", space(self.indent)));
        self.visit_expr(condition);

        self.output
            .push_str(&format!("{}Block:\n", space(self.indent)));
        self.indent += self.indent_spaces;
        self.visit_stmt(block);
        self.indent -= self.indent_spaces;

        if let Some(else_stmt) = optional_else {
            self.output
                .push_str(&format!("{}Else:\n", space(self.indent)));
            self.indent += self.indent_spaces;
            self.visit_stmt(else_stmt);
            self.indent -= self.indent_spaces;
        }
        self.indent -= self.indent_spaces;
    }

    fn visit_stmt_expr(&mut self, expr: &'ast Expr) {
        self.output.push_str(&format!(
            "{}Expr statement: {}\n",
            space(self.indent),
            expr.span
        ));
        self.visit_expr(expr);
    }

    fn visit_stmt_var(&mut self, local: &'ast Local) {
        self.output.push_str(&format!(
            "{}Var statement: {}\n",
            space(self.indent),
            local.span
        ));

        self.indent += self.indent_spaces;

        let Local {
            kind,
            ident,
            binding_mode,
            ty,
            ..
        } = local;

        self.output.push_str(&format!(
            "{}Binding mode: {}\n",
            space(self.indent),
            binding_mode,
        ));
        self.output.push_str(&format!(
            "{}Identifier: {} {}\n",
            space(self.indent),
            ident.name,
            ident.span
        ));
        self.visit_ty(ty);

        match kind {
            LocalKind::Init(expr) => {
                self.output
                    .push_str(&format!("{}Init expression:\n", space(self.indent)));
                self.visit_expr(expr);
            }
            LocalKind::Decl => {}
        }

        self.indent -= self.indent_spaces;
    }

    fn visit_stmt_return(&mut self, expr: Option<&'ast Expr>, span:&'ast Span) {
        self.output.push_str(&format!(
            "{}Return statement: {}\n",
            space(self.indent),
            span
        ));
        if expr.is_some() {
            self.visit_expr(expr.unwrap());
        }
    }

    fn visit_expr(&mut self, expr: &'ast Expr) {
        let Expr { kind, span } = expr;

        self.indent += self.indent_spaces;
        match kind {
            ExprKind::Binary(op, left_expression, right_expression) => {
                self.output.push_str(&format!(
                    "{}Binary operation: {} {}\n",
                    space(self.indent),
                    op,
                    span
                ));
                self.indent += self.indent_spaces;
                self.output
                    .push_str(&format!("{}Left:\n", space(self.indent)));
                self.visit_expr(left_expression);
                self.output
                    .push_str(&format!("{}Right:\n", space(self.indent)));
                self.visit_expr(right_expression);
                self.indent -= self.indent_spaces;
            }

            ExprKind::Unary(_op, subexpression) => {
                self.visit_expr(subexpression);
            }

            ExprKind::Assign(_lhs, _rhs, _span) => {
                self.visit_expr(_lhs);
                self.visit_expr(_rhs);
            }
            ExprKind::AssignOp(_op, _lhs, _rhs) => {
                self.visit_expr(_lhs);
                self.visit_expr(_rhs);
            }
            ExprKind::Literal(token) => {
                self.output.push_str(&format!(
                    "{}{} {}\n",
                    space(self.indent),
                    token.symbol,
                    span
                ));
            }
            ExprKind::Path(path) => {
                self.visit_path(path);
            }
            ExprKind::Cast(expr, ty) => {
                self.visit_expr(expr);
                self.visit_ty(ty);
            }
        }
        self.indent -= self.indent_spaces;
    }

    fn visit_path(&mut self, path: &'ast Path) {
        self.output
            .push_str(&format!("{}Path {}\n", space(self.indent), path.span));

        self.indent += self.indent_spaces;
        for segment in &path.segments {
            self.visit_path_segment(segment);
        }
        self.indent -= self.indent_spaces;
    }

    fn visit_path_segment(&mut self, segment: &'ast PathSegment) {
        let PathSegment { ident, args } = segment;
        self.output.push_str(&format!(
            "{}Segment: {} {}\n",
            space(self.indent),
            ident.name,
            ident.span
        ));

        if let Some(args) = args {
            self.indent += self.indent_spaces;
            self.visit_generic_args(args);
            self.indent -= self.indent_spaces;
        }
    }

    fn visit_generic_args(&mut self, args: &'ast GenericArgs) {
        match args {
            GenericArgs::AngleBracketed(args) => {
                self.output.push_str(&format!(
                    "{}GenericArgs: {}\n",
                    space(self.indent),
                    args.span
                ));
                self.visit_angle_bracketed_args(args);
            }
        }
    }

    fn visit_angle_bracketed_args(&mut self, args: &'ast AngleBracketedArgs) {
        self.indent += self.indent_spaces;
        for arg in &args.args {
            self.visit_angle_bracketed_arg(arg);
        }
        self.indent -= self.indent_spaces;
    }

    fn visit_ty(&mut self, ty: &'ast Ty) {
        self.output
            .push_str(&format!("{}Type:\n", space(self.indent)));
        self.indent += self.indent_spaces;
        match &ty.kind {
            TyKind::Path(path) => {
                self.visit_path(path);
            }
            TyKind::Never => {
                self.output
                    .push_str(&format!("{}Void\n", space(self.indent)));
            }
        }
        self.indent -= self.indent_spaces;
    }
}

fn space(n: usize) -> String {
    " ".repeat(n)
}
