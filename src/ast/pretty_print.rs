use crate::visitor::Visitor;

use super::{
    AngleBracketedArgs, Expr, ExprKind, GenericArgs, Local, LocalKind, Path, PathSegment, Stmt,
    StmtKind, Ty, TyKind,
};

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

    pub fn print_stmt(&mut self, stmt: &super::Stmt) {
        self.visit_stmt(stmt);
    }
}

impl<'ast> Visitor<'ast> for Printer {
    fn visit_stmt_expr(&mut self, expr: &'ast Expr) {
        self.output.push_str(&format!(
            "{}Expr statement: {}\n",
            space(self.indent),
            expr.span
        ));
        self.visit_expr(expr);
    }
    fn visit_stmt_var(&mut self, local: &'ast super::Local) {
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

    fn visit_expr(&mut self, expr: &'ast super::Expr) {
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
        }
        self.indent -= self.indent_spaces;
    }
}

fn space(n: usize) -> String {
    " ".repeat(n)
}
