use std::str::FromStr;

use crate::{
    ast::{Ident, PrimitiveTy, Stmt, TokenKind, Ty, TyKind},
    kw::Keyword,
    session_global::SessionGlobal,
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
impl Parser {
    pub fn parse_stmt(&mut self) -> PResult<Box<Stmt>> {
        if self.is_keyword_ahead(&[Keyword::Var]) {
            self.parse_var_declaration_stmt()
        } else {
            todo!();
        }
    }

    pub fn parse_stmt_if(&mut self) -> PResult<Box<Stmt>> {
        todo!();
    }

    pub fn parse_stmt_block(&mut self) -> PResult<Vec<Box<Stmt>>> {
        todo!();
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
