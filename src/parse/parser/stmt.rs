use crate::{
    ast::{BindingMode, Local, LocalKind, Mutability, Stmt, StmtKind, TokenKind},
    kw::Keyword,
};

use super::{PResult, Parser};

// program = statement* EOF
// block_statement = { statement* }
// statement = block_statement
//             | declaration_statement
//             | expression_statement
//             | if_statement
//             | print_statement (for testing only!)
//
// expression_statement = expression ';'
// expression = assignment_expression
//              | binary_expression
//              | unary_expression
//              | literal
//
// declaration_statement = declaration
// declaration = variable_declaration
//               | function_declaration
//               | class_declaration
//               | enum_declaration
//               | interface_declaration
//
// if_statement = 'if' expression block_statement ('elif' expression block_statement)* 'else'
//                block_statement
//
//
impl Parser<'_> {
    pub fn parse_stmt(&mut self) -> PResult<Box<Stmt>> {
        if self.token.is_keyword(Keyword::Var) {
            self.parse_var_declaration_stmt()
        } else if self.token.can_begin_expr() {
            self.parse_stmt_expr()
        } else {
            unreachable!();
        }
    }

    pub fn parse_stmt_if(&mut self) -> PResult<Box<Stmt>> {
        todo!();
    }

    pub fn parse_stmt_block(&mut self) -> PResult<Vec<Box<Stmt>>> {
        todo!();
    }

    pub fn parse_stmt_expr(&mut self) -> PResult<Box<Stmt>> {
        let expr = self.parse_expr()?;
        let span = expr.span;
        let stmt = Box::new(Stmt {
            kind: StmtKind::Expr(expr),
            span,
        });
        self.advance();
        Ok(stmt)
    }

    /// variable_declaration = 'var' 'mut'? identifier: type_specifier ('=' expression)? ';'
    pub fn parse_var_declaration_stmt(&mut self) -> PResult<Box<Stmt>> {
        debug_assert!(self.token.is_keyword(Keyword::Var));

        let start = self.token.span;
        let binding_mode = if self.is_keyword_ahead(&[Keyword::Mut]) {
            self.advance(); // 'mut'
            BindingMode(Mutability::Mutable)
        } else {
            BindingMode(Mutability::Immutable)
        };

        if !self.is_ident_ahead() {
            return Err("Expected identifier".into());
        }
        self.advance(); // identifier
        let ident = self.token.ident().unwrap().0;

        if !self.look_ahead(1, |tok| tok.kind == TokenKind::Colon) {
            return Err("Expected ':'".into());
        }
        self.advance(); // ':'

        self.advance(); // type
        let ty = self.parse_ty()?;

        let init = if self.token.kind == TokenKind::Eq {
            self.advance(); // expr
            Some(self.parse_expr()?)
        } else {
            None
        };

        if self.token.kind != TokenKind::Semicolon {
            return Err("Expected ';'".into());
        }
        let kind = if let Some(init) = init {
            LocalKind::Init(init)
        } else {
            LocalKind::Decl
        };
        let span = start.to(self.token.span);
        let local = Local {
            binding_mode,
            ident,
            ty,
            kind,
            span,
        };

        self.advance();
        Ok(Box::new(Stmt {
            kind: StmtKind::Var(Box::new(local)),
            span,
        }))
    }
}
