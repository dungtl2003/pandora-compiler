use super::{
    AngleBracketedArgs, Class, Expr, ExprKind, Fun, FunParam, FunRetTy, FunSig, GenericArgs, Ident,
    Interface, Item, ItemKind, Local, LocalKind, Path, PathSegment, SelfKind, SelfParam, Stmt,
    StmtKind, Ty, TyKind,
};
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

    pub fn print_item(&mut self, item: &Item) {
        self.visit_item(item);
    }
}

impl<'ast> Visitor<'ast> for Printer {
    fn visit_item(&mut self, item: &'ast Item) {
        let Item {
            kind,
            span,
            vis,
            ident,
        } = item;

        self.output.push_str(&format!(
            "{}Item: {} {}\n",
            space(self.indent),
            ident.name,
            span
        ));

        self.indent += self.indent_spaces;
        match vis {
            Some(vis) => {
                self.output.push_str(&format!(
                    "{}Visibility: {} {}\n",
                    space(self.indent),
                    vis.kind,
                    vis.span
                ));
            }
            None => {}
        }

        self.output
            .push_str(&format!("{}Kind:\n", space(self.indent)));

        self.indent += self.indent_spaces;
        match kind {
            ItemKind::Fun(fun) => self.visit_item_fun(fun),
            ItemKind::Class(class) => self.visit_item_class(class),
            ItemKind::Interface(interface) => self.visit_item_interface(interface),
            _ => {}
        }
        self.indent -= self.indent_spaces;

        self.indent -= self.indent_spaces;
    }

    fn visit_item_fun(&mut self, fun: &'ast Fun) {
        let Fun {
            generics,
            sig,
            body,
        } = fun;

        self.output
            .push_str(&format!("{}Function:\n", space(self.indent)));

        self.indent += self.indent_spaces;
        self.output
            .push_str(&format!("{}Generics: \n", space(self.indent)));

        for generic in generics {
            self.output.push_str(&format!(
                "{}Ident: {} {}\n",
                space(self.indent),
                generic.ident.name,
                generic.ident.span,
            ));

            self.indent += self.indent_spaces;
            self.output
                .push_str(&format!("{}Bounds: \n", space(self.indent),));

            self.indent += self.indent_spaces;
            for ty in generic.bounds.iter() {
                self.visit_ty(ty);
            }
            self.indent -= self.indent_spaces;
            self.indent -= self.indent_spaces;
        }

        self.output
            .push_str(&format!("{}Inputs: \n", space(self.indent),));
        self.indent += self.indent_spaces;
        let FunSig {
            inputs,
            output,
            span,
        } = sig;
        let (self_param, params) = inputs;
        if let Some(SelfParam {
            kind: self_kind,
            span: self_span,
        }) = self_param
        {
            self.output
                .push_str(&format!("{}Self: {} - ", space(self.indent), self_span));
            match self_kind {
                SelfKind::Value(mutability) => {
                    self.output.push_str(&format!("{}\n", mutability));
                }
                SelfKind::Explicit(ty, mutability) => {
                    self.output
                        .push_str(&format!("{} {} - {}\n", mutability, ty.kind, ty.span));
                }
            };
        }

        for FunParam { span, ty, ident } in params {
            self.output
                .push_str(&format!("{}Param: {}\n", space(self.indent), span));
            self.indent += self.indent_spaces; //3
            self.output.push_str(&format!(
                "{}Ident: {} {}\n",
                space(self.indent),
                ident.name,
                ident.span,
            ));
            let kind = &ty.kind;
            match kind {
                TyKind::Never => {
                    self.output
                        .push_str(&format!("{}Type: Void {}\n", space(self.indent), span,));
                }
                TyKind::Path(path) => {
                    self.output
                        .push_str(&format!("{}Type:\n", space(self.indent),));
                    self.indent += self.indent_spaces;
                    self.visit_path(&path);
                    self.indent -= self.indent_spaces;
                }
            }
            self.indent -= self.indent_spaces;
        }
        self.indent -= self.indent_spaces;

        self.output
            .push_str(&format!("{}Output:\n", space(self.indent),));
        self.indent += self.indent_spaces;
        match output {
            FunRetTy::Default(span) => {
                self.output
                    .push_str(&format!("{}Void {}\n", space(self.indent), span))
            }
            FunRetTy::Ty(ty) => {
                let kind = &ty.kind;
                match kind {
                    TyKind::Never => {
                        self.output
                            .push_str(&format!("{}Void {}\n", space(self.indent), span,));
                    }
                    TyKind::Path(path) => {
                        self.indent += self.indent_spaces;
                        self.visit_path(&path);
                        self.indent -= self.indent_spaces;
                    }
                }
            }
        }
        self.indent -= self.indent_spaces;

        let stmts = if let Some(Stmt {
            kind: body_kind,
            span: _,
        }) = body
        {
            match body_kind {
                StmtKind::Block(stmts) => stmts,
                _ => &vec![],
            }
        } else {
            &vec![]
        };

        self.output
            .push_str(&format!("{}Body: \n", space(self.indent)));
        self.indent += self.indent_spaces;
        self.visit_stmt_block(&stmts);
        self.indent -= self.indent_spaces;
    }

    fn visit_item_class(&mut self, class: &'ast Class) {
        todo!()
    }

    fn visit_item_interface(&mut self, interface: &'ast Interface) {
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
