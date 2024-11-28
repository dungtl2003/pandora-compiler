use super::{Expr, ExprKind, Fun, FunSig, Ident, Local, LocalKind, Path, PathSegment, Stmt, Ty};
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

    pub fn print_stmt(&mut self, stmt: &Stmt) {
        self.visit_stmt(stmt);
    }
}

impl<'ast> Visitor<'ast> for Printer {
    fn visit_stmt_func_decl(&mut self, fun: &'ast Fun) {
        let Fun { sig, body } = fun;
        let FunSig {
            name,
            inputs,
            output,
            span,
        } = sig;

        self.output.push_str(&format!(
            "{}Function declaration: {}\n",
            space(self.indent),
            span
        ));
        self.indent += self.indent_spaces;

        self.output.push_str(&format!(
            "{}Name: {} {}\n",
            space(self.indent),
            name.name,
            name.span
        ));

        self.output
            .push_str(&format!("{}Inputs:\n", space(self.indent)));
        self.indent += self.indent_spaces;
        for input in inputs {
            self.visit_ty(&input.ty);
        }
        self.indent -= self.indent_spaces;

        if let Some(output) = output {
            self.output
                .push_str(&format!("{}Output:\n", space(self.indent)));
            self.indent += self.indent_spaces;
            self.visit_ty(output);
            self.indent -= self.indent_spaces;
        }

        self.output
            .push_str(&format!("{}Body:\n", space(self.indent)));
        self.indent += self.indent_spaces;
        self.visit_stmt(body);
        self.indent -= self.indent_spaces;
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

    fn visit_stmt_if(
        &mut self,
        condition: &'ast Expr,
        block: &'ast Stmt,
        optional_else: Option<&'ast Stmt>,
    ) {
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

    fn visit_stmt_return(&mut self, expr: Option<&'ast Expr>) {
        self.output
            .push_str(&format!("{}Return statement:\n", space(self.indent),));
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

            ExprKind::Unary(op, subexpression) => {
                self.output.push_str(&format!(
                    "{}Unary operation: {} {}\n",
                    space(self.indent),
                    op,
                    span
                ));
                self.visit_expr(subexpression);
            }

            ExprKind::Assign(lhs, rhs, span) => {
                self.output
                    .push_str(&format!("{}Assignment: {}\n", space(self.indent), span));
                self.visit_expr(lhs);
                self.visit_expr(rhs);
            }
            ExprKind::AssignOp(op, lhs, rhs) => {
                self.output.push_str(&format!(
                    "{}Assignment operation: {} {}\n",
                    space(self.indent),
                    op,
                    span
                ));
                self.visit_expr(lhs);
                self.visit_expr(rhs);
            }
            ExprKind::Literal(token) => {
                self.output.push_str(&format!(
                    "{}{} {}\n",
                    space(self.indent),
                    token.symbol,
                    span
                ));
            }
            ExprKind::Identifier(ident) => {
                self.output.push_str(&format!(
                    "{}Identifier: {} {}\n",
                    space(self.indent),
                    ident.name,
                    ident.span
                ));
            }
            ExprKind::Cast(expr, ty) => {
                self.output
                    .push_str(&format!("{}Cast:\n", space(self.indent)));
                self.indent += self.indent_spaces;
                self.output
                    .push_str(&format!("{}From:\n", space(self.indent),));
                self.visit_expr(expr);
                self.output
                    .push_str(&format!("{}To:\n", space(self.indent),));
                self.indent += self.indent_spaces;
                self.visit_ty(ty);
                self.indent -= self.indent_spaces;
            }
            ExprKind::FunCall(fun, args) => {
                self.output.push_str(&format!(
                    "{}Function call: {} {}\n",
                    space(self.indent),
                    fun.span,
                    span
                ));
                self.indent += self.indent_spaces;
                self.output
                    .push_str(&format!("{}Function name:\n", space(self.indent)));
                self.visit_expr(fun);
                self.output
                    .push_str(&format!("{}Arguments:\n", space(self.indent)));
                for arg in args {
                    self.visit_expr(arg);
                }
                self.indent -= self.indent_spaces;
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
        let PathSegment { ident } = segment;
        self.output.push_str(&format!(
            "{}Segment: {} {}\n",
            space(self.indent),
            ident.name,
            ident.span
        ));
    }

    fn visit_ty(&mut self, ty: &'ast Ty) {
        self.output.push_str(&format!(
            "{}Type: {} {}\n",
            space(self.indent),
            ty.name,
            ty.span
        ));
    }
}

fn space(n: usize) -> String {
    " ".repeat(n)
}
