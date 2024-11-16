use std::str::FromStr;

use crate::{ast::{BindingMode, Local, LocalKind, Mutability, Ident, PrimitiveTy, Stmt, StmtKind, TokenKind, Ty, TyKind}, kw, kw::Keyword, session_global::SessionGlobal, span_encoding::Span};
use crate::ast::Delimiter;
use crate::kw::is_keyword;

// use crate::interner::Interner;
use super::{PResult, Parser, TokenType};

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
// variable_declaration = 'var' 'mut'? identifier: type_specifier ('=' expression)? ';'
// type_specifier = primitive_type | ...
// primitive_type = 'int' | 'float' | 'bool' | 'char'
//
impl Parser<'_> {
    pub fn parse_stmt(&mut self) -> PResult<Box<Stmt>> {
        if self.token.is_keyword(Keyword::Var) {
            self.parse_var_declaration_stmt()
        } else if self.token.is_keyword(Keyword::If) {
            println!("debug: ");
            self.parse_stmt_if()
        } else if self.token.is_ident() {
            self.parse_stmt_if()
        } else if self.token.can_begin_expr() {
            self.parse_stmt_expr()
        } else {
            unreachable!();
        }
    }

    pub fn parse_stmt_if(&mut self) -> PResult<Box<Stmt>> {
        // self.expected_tokens.push(TokenType::Keyword(Interner::intern(&mut Interner::new(), kw::Keyword::If.into())));

        let start_span = self.token.span;
        let mut end_span = self.token.span;

        self.advance(); // Eat token after "if"
        // Parse the condition expression.
        println!("{}",self.token.span);
        let condition = self.parse_expr()?;

        // Parse the block for the `if` statement.
        // println!("{}", self.look_ahead(1, |tok|tok.kind == TokenKind::OpenDelim(Delimiter::Brace)));//TODO!
        // self.advance(); // Eat '{'
        let if_block = self.parse_stmt_block()?;
        // println!("{}", self.look_ahead(1, |tok|tok.kind == TokenKind::CloseDelim(Delimiter::Brace)));//TODO!
        // self.advance(); // Eat '}'

        let mut if_branches = vec![(condition, if_block)];

        // Parse any `elif` branches.
        while self.token.is_keyword(Keyword::Elif) {
            self.advance(); // Eat token after `elif`
            println!("{}",self.token.span);
            let elif_condition = self.parse_expr()?;

            let mut elif_block = Vec::new();

            if self.token.kind == TokenKind::OpenDelim(Delimiter::Brace) {
                elif_block = self.parse_stmt_block()?;
            } else {
                todo!()
            };

            if_branches.push((elif_condition, elif_block));
        }

        // Optionally parse an `else` block.
        let else_block = if self.token.is_keyword(Keyword::Else) {
            self.advance(); // Eat token after `else`
            let block = self.parse_stmt_block()?;
            Some(block)
        } else {
            None
        };
        if self.token.kind == TokenKind::Eof {
            end_span = self.prev_token.span;
        } else {
            end_span = self.token.span;
        }

        // Construct the `If` statement node.
        let stmt = Stmt {
            kind: StmtKind::If(if_branches, else_block),
            span: self.mk_stmt_sp(&start_span,end_span), // Create the span for the whole `if` statement.
        };
        Ok(Box::new(stmt))
    }

    pub fn mk_stmt_sp(&self,start:&Span,end:Span) -> Span {
        start.to(end)
    }

    pub fn parse_stmt_block(&mut self) -> PResult<Vec<Box<Stmt>>> {
        if self.token.kind != TokenKind::OpenDelim(Delimiter::Brace) {
            todo!()
        };
        self.advance(); // Eat token after '{'

        let mut stmts = Vec::new();
        while self.token.kind != TokenKind::CloseDelim(Delimiter::Brace) {
            let stmt= self.parse_stmt()?;
            stmts.push(stmt);
            self.advance();
        }

        self.advance(); // Eat token after '}'
        Ok(stmts)
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
