use crate::{session_global::SessionGlobal, visitor::Visitor};

use super::{Expr, ExprKind, GenericArgs, Path, PathSegment, Ty};

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
            indent_spaces: 4,
        }
    }

    pub fn print_expr(&mut self, expr: &super::Expr) {
        self.visit_expr(expr);
    }

    pub fn print_path(&mut self, path: &Path) {
        self.visit_path(path);
    }
}

impl<'ast> Visitor<'ast> for Printer {
    fn visit_expr(&mut self, expr: &'ast super::Expr) {
        let Expr { kind, span } = expr;

        self.indent += self.indent_spaces;
        match kind {
            ExprKind::Binary(op, left_expression, right_expression) => {
                self.output
                    .push_str(&format!("Binary operation: {} {}\n", op, span));
                self.output
                    .push_str(&format!("{}Left: ", " ".repeat(self.indent)));
                self.visit_expr(left_expression);
                self.output
                    .push_str(&format!("{}Right: ", " ".repeat(self.indent)));
                self.visit_expr(right_expression);
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
                self.output.push_str(&format!("{}\n", token.symbol));
            }
        }
        self.indent -= self.indent_spaces;
    }

    fn visit_path(&mut self, path: &'ast Path) {
        self.output.push_str(&format!("Path: {}\n", path.span));
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
            " ".repeat(self.indent),
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
                    " ".repeat(self.indent),
                    args.span
                ));
                self.visit_angle_bracketed_args(args);
            }
        }
    }

    fn visit_angle_bracketed_args(&mut self, args: &'ast super::AngleBracketedArgs) {
        self.indent += self.indent_spaces;
        for arg in &args.args {
            self.visit_angle_bracketed_arg(arg);
        }
        self.indent -= self.indent_spaces;
    }

    fn visit_ty(&mut self, ty: &'ast Ty) {
        self.output
            .push_str(&format!("{}Type: {}\n", " ".repeat(self.indent), ty.span));
        self.indent += self.indent_spaces;
        match &ty.kind {
            super::TyKind::Path(path) => {
                self.visit_path(path);
            }
        }
        self.indent -= self.indent_spaces;
    }
}
