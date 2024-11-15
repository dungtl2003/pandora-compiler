use std::str::FromStr;

use crate::{ast::{Ident, PrimitiveTy, Stmt, StmtKind, TokenKind, Ty, TyKind}, kw, kw::Keyword, session_global::SessionGlobal, span_encoding::Span};
use crate::ast::Delimiter;
use crate::interner::Interner;
// use crate::{
//     ast::{BinOp, BinOpKind, BinOpToken, Delimiter, Stmt, StmtKind, Lit, TokenKind, UnOp},
//     parse::util::parser::{AssocOp, Fixity},
//     span_encoding::Span,
// };

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
impl Parser {
    pub fn parse_stmt(&mut self) -> PResult<Box<Stmt>> {
        if self.is_keyword_ahead(&[Keyword::Var]) {
            self.parse_var_declaration_stmt()
        } else if self.is_keyword_ahead(&[Keyword::If]) {
            self.parse_stmt_if()
        } else if self.token.is_ident() {
            self.parse_stmt_if()
        }
        else {
            todo!();
        }
    }

    pub fn parse_stmt_if(&mut self) -> PResult<Box<Stmt>> {
        // self.expected_tokens.push(TokenType::Keyword(Interner::intern(&mut Interner::new(), kw::Keyword::If.into())));

        let start_span = self.token.span;
        let mut end_span = self.token.span;

        // println!("{}", self.token.is_keyword(Keyword::If));

        // println!("{}", self.look_ahead(1, |tok|tok.kind == TokenKind::OpenDelim(Delimiter::Brace)));//TODO!
        self.advance(); // Eat token after "if"
        // Parse the condition expression.
        let condition = self.parse_expr()?;

        // Parse the block for the `if` statement.
        // println!("{}", self.look_ahead(1, |tok|tok.kind == TokenKind::OpenDelim(Delimiter::Brace)));//TODO!
        // self.advance(); // Eat '{'
        let if_block = self.parse_stmt_block()?;
        // println!("{}", self.look_ahead(1, |tok|tok.kind == TokenKind::CloseDelim(Delimiter::Brace)));//TODO!
        // self.advance(); // Eat '}'
        end_span = self.token.span;
        let mut if_branches = vec![(condition, if_block)];



        // Parse any `elif` branches.
        while self.look_ahead(1, |tok|tok.is_ident()){
            self.advance(); // Eat `elif`
            self.advance(); // Eat token after `elif`
            let elif_condition = self.parse_expr()?;

            // println!("{}", self.look_ahead(1, |tok|tok.kind == TokenKind::OpenDelim(Delimiter::Brace)));//TODO!
            // self.look_ahead(1, |tok|tok.kind == TokenKind::OpenDelim(Delimiter::Brace));//TODO!
            // self.expect(TokenKind::OpenDelim(Delimiter::Brace))?;
            // self.advance(); // Eat '{'

            let elif_block = self.parse_stmt_block()?;

            // println!("debug:{}", self.look_ahead(1, |tok|tok.kind == TokenKind::CloseDelim(Delimiter::Brace)));//TODO!
            // self.look_ahead(1, |tok|tok.kind == TokenKind::CloseDelim(Delimiter::Brace));//TODO!
            // self.advance(); // Eat '}'
            end_span = self.token.span;

            if_branches.push((elif_condition, elif_block));
        }

        // Optionally parse an `else` block.
        let else_block = if self.token.is_keyword(Keyword::Else) {

            self.advance(); // Eat `else`

            self.look_ahead(1, |tok|tok.kind == TokenKind::OpenDelim(Delimiter::Brace));//TODO!
            self.advance(); // Eat '{'

            let block = self.parse_stmt_block()?;

            end_span = self.token.span;

            Some(block)
        } else {
            None
        };
        end_span = self.token.span;
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
        // let start_span = self.token.span;
        // let mut end_span = self.token.span;
        let mut stmts = Vec::new();
        while self.look_ahead(1, |tok|tok.kind != TokenKind::CloseDelim(Delimiter::Brace)) {
            self.advance();
            let stmt= self.parse_stmt_if()?;
            stmts.push(stmt);
        }

        self.advance();
        // println!("{}",self.token.kind == TokenKind::CloseDelim(Delimiter::Brace));
        // self.look_ahead(1, |tok|tok.kind == TokenKind::CloseDelim(Delimiter::Brace)); //TODO!

        // self.advance(); // Eat '}'
        // end_span = self.token.span;

        Ok(stmts)
    }

    /// variable_declaration = 'var' 'mut'? identifier: type_specifier ('=' expression)? ';'
    pub fn parse_var_declaration_stmt(&mut self) -> PResult<Box<Stmt>> {
        assert!(self.look_ahead(1, |tok| tok.is_keyword(Keyword::Var)));
        self.advance(); // `var`

        if self.look_ahead(1, |tok| tok.is_keyword(Keyword::Mut)) {
            self.advance(); // `mut`
        }

        if !self.look_ahead(1, |tok| tok.is_ident()) {
            return Err("Expected identifier".into());
        }
        self.advance(); // identifier
        let (ident, _) = self.token.ident().unwrap();

        if !self.look_ahead(1, |tok| tok.kind == TokenKind::Colon) {
            return Err("Expected `:`".into());
        }

        self.advance(); // `:`

        todo!();
    }
}
