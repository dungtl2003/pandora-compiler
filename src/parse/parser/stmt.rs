use crate::ast::{Delimiter, Ident};
use crate::span_encoding::Span;
use crate::symbol::Symbol;
use crate::{
    ast::{BindingMode, Local, LocalKind, Mutability, Stmt, StmtKind, TokenKind},
    kw::Keyword,
};
use miette::{SourceOffset, SourceSpan};
// use crate::interner::Interner;
use super::{PError, PResult, Parser};

// block_statement = { statement* }
// statement = block_statement
//             | declaration_statement
//             | expression_statement
//             | if_statement
//             | loop_statement
//
//
// loop_statement = predicate_loop_statement
//                 | iterator_loop_statement
//
// predicate_loop_statement = 'while' expression block_statement
// iterator_loop_statement = 'for' identifier 'in' expression block_statement
//
//
// expression_statement = expression ';'
// expression = assignment_expression
//              | binary_expression
//              | unary_expression
//              | literal
//
//
// declaration_statement = declaration
// declaration = variable_declaration
//               | function_declaration
//               | class_declaration
//               | enum_declaration
//               | interface_declaration
//
//
// variable_declaration = 'var' 'mut'? identifier: type_specifier ('=' expression)? ';'
// type_specifier = path
// path = segment ('::' segment)*
// segment = identifier ('<' generic_args '>')?
// generic_args = angle_bracketed_args
// angle_bracketed_args = '<' angle_bracketed_arg (',' angle_bracketed_arg)* '>'
// angle_bracketed_arg = generic_arg
// generic_arg = type_specifier

impl Parser<'_> {
    pub fn parse_stmt(&mut self) -> PResult<Box<Stmt>> {
        if self.token.is_keyword(Keyword::Var) {
            self.parse_stmt_var_decl()
        } else if self.token.is_keyword(Keyword::If) {
            self.parse_stmt_if()
        } else if self.token.can_begin_expr() {
            self.parse_stmt_expr()
        } else if self.token.kind == TokenKind::OpenDelim(Delimiter::Brace) {
            self.parse_stmt_block()
        } else if self.token.is_keyword(Keyword::While) {
            self.parse_stmt_while()
        } else if self.token.is_keyword(Keyword::For) {
            self.parse_stmt_for()
        } else if self.token.is_keyword(Keyword::Return) {
            self.parse_stmt_return()
        } else if self.token.can_begin_item() {
            self.parse_stmt_item()
        } else if self.token.kind == TokenKind::Semicolon {
            self.parse_stmt_empty()
        } else {
            Err(self.parse_stmt_recover().unwrap())
        }
    }

    pub fn can_begin_stmt(&self) -> bool {
        self.token.is_keyword(Keyword::Var)
            || self.token.is_keyword(Keyword::If)
            || self.token.can_begin_expr()
            || self.token.kind == TokenKind::OpenDelim(Delimiter::Brace)
            || self.token.is_keyword(Keyword::While)
            || self.token.is_keyword(Keyword::For)
            || self.token.is_keyword(Keyword::Return)
            || self.token.can_begin_item()
            || self.token.kind == TokenKind::Semicolon
    }

    pub fn parse_stmt_recover(&mut self) -> Option<PError> {
        // if self.token.kind == TokenKind::Eof || self.can_begin_stmt() {
        //     return None;
        // };

        let start_span = self.token.span;
        let mut end_span = self.token.span;

        while self.token.kind != TokenKind::Eof {
            if self.can_begin_stmt() {
                break;
            } else {
                end_span = self.token.span;
                self.advance();
            }
        }
        let span = start_span.to(end_span);

        self.perrs.push(PError::ExpectedStmt {
            span: SourceSpan::new(SourceOffset::from(span.offset as usize), span.length),
        });

        Some(PError::ExpectedStmt {
            span: SourceSpan::new(SourceOffset::from(span.offset as usize), span.length),
        })
    }

    pub fn parse_stmt_item(&mut self) -> PResult<Box<Stmt>> {
        let item = self.parse_item()?;
        let span = item.span;
        let kind = StmtKind::Item(item);
        Ok(Box::new(Stmt { kind, span }))
    }

    pub fn parse_stmt_return(&mut self) -> PResult<Box<Stmt>> {
        let start_span = self.token.span;
        let kind = if self.look_ahead(1, |tok| tok.can_begin_expr()) {
            self.advance(); // Eat token after "return"
            let expr = self.parse_expr()?;
            self.advance(); // Eat token after ';'
            StmtKind::Return(Some(expr))
        } else if self.look_ahead(1, |tok| tok.kind == TokenKind::Semicolon) {
            self.advance(); // Eat token after "return" <=> ';'
            self.advance(); // Eat token after ';'
            StmtKind::Return(None)
        } else {
            return Err(PError::ExpectedToken {
                expected: format!("{}", "';'"),
                found: format!("{}", self.token),
                span: SourceSpan::new(
                    SourceOffset::from(self.token.span.offset as usize),
                    self.token.span.length,
                ),
            });
        };
        Ok(Box::new(Stmt {
            kind,
            span: start_span.to(self.prev_token.span),
        }))
    }

    /// predicate_loop_statement = 'while' expression block_statement
    pub fn parse_stmt_while(&mut self) -> PResult<Box<Stmt>> {
        debug_assert!(self.token.is_keyword(Keyword::While));

        let start_span = self.token.span;
        self.advance();

        let condition = self.parse_expr();
        let block = self.parse_stmt_block();
        let end_span = self.prev_token.span;
        let span = start_span.to(end_span);

        if condition.is_err() || block.is_err(){
            return Err(PError::Normal);
        }

        let kind = StmtKind::While(condition?, block?);
        let stmt = Box::new(Stmt { kind, span });

        Ok(stmt)
    }

    /// iterator_loop_statement = 'for' identifier 'in' expression block_statement
    pub fn parse_stmt_for(&mut self) -> PResult<Box<Stmt>> {
        debug_assert!(self.token.is_keyword(Keyword::For));

        let start_span = self.token.span;

        self.advance();
        let ident = if !self.token.is_ident() || self.token.is_keyword(Keyword::In) {
            self.perrs.push(PError::ExpectedToken {
                expected: format!("{}", "identifier"),
                found: format!("{}", self.token),
                span: SourceSpan::new(
                    SourceOffset::from(self.token.span.offset as usize),
                    self.token.span.length,
                ),
            });
            None
        } else {//if !self.token.is_keyword(Keyword::In)
            self.advance();
            Some(self.prev_token.ident().unwrap().0)
        };

        if !self.token.is_keyword(Keyword::In) {
            self.perrs.push(PError::ExpectedToken {
                expected: format!("{}", "\"in\""),
                found: format!("{}", self.token),
                span: SourceSpan::new(
                    SourceOffset::from(self.token.span.offset as usize),
                    self.token.span.length,
                ),
            });
        } else {
            self.advance();
        }

        let expr = self.parse_expr();
        let block = self.parse_stmt_block();
        let end_span = self.prev_token.span;
        let span = start_span.to(end_span);

        if ident.is_none() || expr.is_err() || block.is_err() {
            return Err(PError::Normal);
        };

        let kind = StmtKind::For(ident.unwrap(), expr?, block?);
        let stmt = Box::new(Stmt { kind, span });

        Ok(stmt)
    }

    /// if_statement = 'if' expression block_statement ('else' (block_statement | if_statement))?
    pub fn parse_stmt_if(&mut self) -> PResult<Box<Stmt>> {
        debug_assert!(self.token.is_keyword(Keyword::If));

        let start_span = self.token.span;

        self.advance(); // Eat token after "if"
                        // Parse the condition expression.
        let condition = self.parse_expr();

        // Parse the block for the `if` statement.
        let if_block = self.parse_stmt_block();

        // Optionally parse an `else` block.
        let else_block = if self.token.is_keyword(Keyword::Else) {
            self.advance(); // Eat token after `else`
            if self.token.is_keyword(Keyword::If) {
                let else_block = self.parse_stmt_if()?;
                Some(else_block)
            } else if self.token.kind == TokenKind::OpenDelim(Delimiter::Brace) {
                let else_block = self.parse_stmt_block()?;
                Some(else_block)
            } else {
                self.perrs.push(PError::ExpectedToken {
                    expected: format!("{}", "\"if\" or '{'"),
                    found: format!("{}", self.token),
                    span: SourceSpan::new(
                        SourceOffset::from(self.token.span.offset as usize),
                        self.token.span.length,
                    ),
                });
                return Err(PError::Normal)
            }
        } else {
            None
        };

        if condition.is_err() || if_block.is_err() {
            return Err(PError::Normal)
        }

        let end_span = self.prev_token.span;
        let span = start_span.to(end_span);
        let kind = StmtKind::If(condition?, if_block?, else_block);
        let stmt = Box::new(Stmt { kind, span });

        Ok(stmt)
    }

    /// block_statement = '{' statement* '}'
    pub fn parse_stmt_block(&mut self) -> PResult<Box<Stmt>> {
        if self.token.kind != TokenKind::OpenDelim(Delimiter::Brace) {
            self.perrs.push(PError::ExpectedToken {
                expected: format!("{}", "'{'"),
                found: format!("{}", self.token),
                span: SourceSpan::new(
                    SourceOffset::from(self.token.span.offset as usize),
                    self.token.span.length,
                ),
            });
            return Err(PError::Normal)
        }

        let start = self.token.span;
        self.advance(); // Eat token after '{'

        let mut stmts = Vec::new();
        while self.token.kind != TokenKind::CloseDelim(Delimiter::Brace) {
            let stmt = self.parse_stmt();
            if stmt.is_ok() {
                stmts.push(stmt?)
            };
        }

        let end = self.token.span;
        let span = start.to(end);
        let kind = StmtKind::Block(stmts);
        let stmt = Box::new(Stmt { kind, span });

        self.advance(); // Eat token after '}'
        Ok(stmt)
    }

    /// expression_statement = expression ';'
    pub fn parse_stmt_expr(&mut self) -> PResult<Box<Stmt>> {
        let expr = self.parse_expr()?;
        let mut span = expr.span;
        match self.parse_stmt_empty() {
            Ok(stmt) => {span = span.to(stmt.span);},
            Err(err) => {return Err(err)},
        }

        let stmt = Box::new(Stmt {
            kind: StmtKind::Expr(expr),
            span,
        });


        Ok(stmt)
    }

    /// variable_declaration = 'var' 'mut'? identifier: type_specifier ('=' expression)? ';'
    pub fn parse_stmt_var_decl(&mut self) -> PResult<Box<Stmt>> {
        debug_assert!(self.token.is_keyword(Keyword::Var));

        let start = self.token.span;
        let binding_mode = if self.is_keyword_ahead(&[Keyword::Mut]) {
            self.advance(); // 'mut'
            BindingMode(Mutability::Mutable)
        } else {
            BindingMode(Mutability::Immutable)
        };

        self.advance(); // identifier
        let ident = if !self.token.is_ident() {
            self.perrs.push(PError::ExpectedToken {
                expected: format!("{}", "identifier"),
                found: format!("{}", self.token.kind),
                span: SourceSpan::new(
                    SourceOffset::from(self.token.span.offset as usize),
                    self.token.span.length,
                ),
            });
            None
        } else {
            self.advance();
            Some(self.prev_token.ident().unwrap().0)
        };

        if self.token.kind != TokenKind::Colon {
            self.perrs.push(PError::ExpectedToken {
                expected: format!("{}", "':'"),
                found: format!("{}", self.token.kind),
                span: SourceSpan::new(
                    SourceOffset::from(self.token.span.offset as usize),
                    self.token.span.length,
                ),
            });
        } else {
            self.advance(); // ident to type
        }

        let ty = self.parse_ty()?;

        let init = if self.token.kind == TokenKind::Eq {
            self.advance(); // expr
            if !self.token.can_begin_expr() {
                self.perrs.push(PError::ExpectedExpr {
                    found: format!("{}", self.token),
                    span: SourceSpan::new(
                        SourceOffset::from(self.token.span.offset as usize),
                        self.token.span.length,
                    ),
                });
                return Err(PError::Normal);
            } else {
                Some(self.parse_expr()?)
            }
        } else {
            None
        };
        match self.parse_stmt_empty() {
            Ok(_) => {},
            Err(err) => {return Err(err)},
        }

        let span = start.to(self.prev_token.span);

        let kind = if let Some(init) = init {
            LocalKind::Init(init)
        } else {
            LocalKind::Decl
        };

        if ident.is_none() {
            return Err(PError::Normal);
        };

        let local = Local {
            binding_mode,
            ident:ident.unwrap(),
            ty,
            kind,
            span,
        };
        let kind = StmtKind::Var(Box::new(local));
        let stmt = Box::new(Stmt { kind, span });

        Ok(stmt)
    }

    fn parse_stmt_empty(&mut self) -> PResult<Box<Stmt>> {
        if self.token.kind != TokenKind::Semicolon {
            self.perrs.push(PError::ExpectedToken {
                expected: "';'".to_string(),
                found: format!("{}", self.token.kind),
                span: SourceSpan::new(
                    SourceOffset::from(self.token.span.offset as usize),
                    self.token.span.length,
                ),
            });
            return Err(PError::Normal);
        }

        let span = self.token.span;
        self.advance();
        Ok(Box::new(Stmt {
            kind: StmtKind::Empty,
            span,
        }))
    }
}
