use crate::visitor::Visitor;

use super::{Expr, ExprKind};

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
                self.output
                    .push_str(&format!("Literal({}) {}", token, span));
                self.output.push_str("\n");
            }
        }
        self.indent -= self.indent_spaces;
    }
}
