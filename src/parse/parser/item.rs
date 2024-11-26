use super::{PResult, Parser};
use crate::ast::{
    BinOpToken, Class, ClassBody, ClassField, ClassFieldKind, Delimiter, ExtClause, Fun, FunParam,
    FunRetTy, FunSig, GenericParam, ImplClause, Interface, InterfaceBody, Item, ItemKind,
    Mutability, SelfKind, SelfParam, Stmt, Token, TokenKind, Visibility, VisibilityKind,
};
use crate::kw::Keyword;

impl Parser<'_> {
    pub fn parse_item(&mut self) -> PResult<Box<Item>> {
        if self.token.is_keyword(Keyword::Pub) {
            self.advance(); // Eat token after "pub"
        };

        if self.token.is_keyword(Keyword::Class) {
            self.parse_item_class()
        } else if self.token.is_keyword(Keyword::Fun) {
            self.parse_item_function()
        } else if self.token.is_keyword(Keyword::Interface) {
            self.parse_item_interface()
        } else {
            todo!()
        }
    }

    fn parse_item_function(&mut self) -> PResult<Box<Item>> {
        debug_assert!(self.token.is_keyword(Keyword::Fun));
        self.advance();

        let mut start_span = self.token.span;

        let vis = self.check_item_vis();
        if vis.is_some() {
            start_span = self.prev_token.span;
        };

        let ident = self.parse_ident()?;

        let generics = self.parse_item_generics()?;
        let sig = self.parse_item_function_sig()?;
        let body = self.parse_item_function_body()?;

        let span = start_span.to(self.prev_token.span);

        Ok(Box::new(Item {
            span,
            kind: ItemKind::Fun(Box::new(Fun {
                generics,
                sig,
                body,
            })),
            vis,
            ident,
        }))
    }

    // (mut? self:Type?, a:A, b:B)
    fn parse_item_function_sig(&mut self) -> PResult<FunSig> {
        if self.token.kind != TokenKind::OpenDelim(Delimiter::Parenthesis) {
            return Err("Expected parenthesis".into());
        }
        let start_span = self.token.span;

        // self_param
        let self_param = if self.is_keyword_ahead(&[Keyword::Mut]) {
            self.advance(); // Eat "mut"
            let start_self_param_span = self.token.span;
            if !self.is_keyword_ahead(&[Keyword::SelfLower]) {
                return Err("Expected self".into());
            }
            self.advance(); // Eat "self"
            if self.look_ahead(1, |tok| tok.kind == TokenKind::Colon) {
                self.advance(); // Eat ':'
                if !self.is_ident_ahead() {
                    return Err("Expected identifier".into());
                }
                self.advance(); // Eat ident
                let ty = self.parse_ty()?;
                Some(SelfParam {
                    kind: SelfKind::Explicit(ty, Mutability::Mutable),
                    span: start_self_param_span.to(self.token.span),
                })
            } else {
                Some(SelfParam {
                    kind: SelfKind::Value(Mutability::Mutable),
                    span: start_self_param_span.to(self.token.span),
                })
            }
        } else if self.is_keyword_ahead(&[Keyword::SelfLower]) {
            let start_self_param_span = self.token.span;
            self.advance(); // Eat "self"
            if self.look_ahead(1, |tok| tok.kind == TokenKind::Colon) {
                self.advance(); // Eat ':'
                if !self.is_ident_ahead() {
                    return Err("Expected identifier".into());
                }
                self.advance(); // Eat ident
                let ty = self.parse_ty()?;
                Some(SelfParam {
                    kind: SelfKind::Explicit(ty, Mutability::Immutable),
                    span: start_self_param_span.to(self.token.span),
                })
            } else {
                Some(SelfParam {
                    kind: SelfKind::Value(Mutability::Immutable),
                    span: start_self_param_span.to(self.token.span),
                })
            }
        } else {
            None
        };
        if self.look_ahead(1, |tok| tok.kind == TokenKind::Comma) {
            self.advance();
        }

        // fun_params
        let mut fun_params = Vec::new();
        self.advance(); // Eat token after '(' or ','

        while self.token.kind != TokenKind::CloseDelim(Delimiter::Parenthesis) {
            if !self.token.is_ident() {
                return Err("Expected identifier".into());
            }

            let pram = self.parse_item_function_param()?;
            fun_params.push(pram);

            if self.token.kind == TokenKind::Comma && self.is_ident_ahead() {
                self.advance(); // Eat ident
            } else {
                break;
            };
        }
        if self.token.kind != TokenKind::CloseDelim(Delimiter::Parenthesis) {
            return Err("Expected ')'".into());
        }

        if self.look_ahead(1, |tok| tok.kind != TokenKind::RArrow) {
            return Err("Expected ->".into());
        }

        self.advance(); // Eat "->"

        if !self.is_ident_ahead() {
            return Err("Expected type".into());
        }

        let output = if self.is_keyword_ahead(&[Keyword::Void]) {
            self.advance(); // Eat "void"
            self.advance(); // Eat token after "void"
            FunRetTy::Default(self.prev_token.span)
        } else if self.is_ident_ahead() {
            self.advance(); // Eat ident
            FunRetTy::Ty(self.parse_ty()?)
        } else {
            return Err("Expected a type".into());
        };

        let span = start_span.to(self.prev_token.span);

        let inputs = (self_param, fun_params);
        Ok(FunSig {
            inputs,
            output,
            span,
        })
    }

    fn parse_item_function_param(&mut self) -> PResult<FunParam> {
        if !self.token.is_ident() {
            return Err("Expected identifier".into());
        }
        let start_span = self.token.span;

        let ident = self.parse_ident()?;
        if self.token.kind == TokenKind::Colon {
            self.advance();
        } else {
            return Err("Expected ':'".into());
        }
        let ty = self.parse_ty()?;

        let span = start_span.to(self.prev_token.span);

        Ok(FunParam { ty, ident, span })
    }

    fn parse_item_function_body(&mut self) -> PResult<Option<Stmt>> {
        if self.token.kind == TokenKind::Semicolon {
            self.advance(); // Eat token after ';'
            return Ok(None);
        }
        if self.token.kind != TokenKind::OpenDelim(Delimiter::Brace) {
            return Err("Expected '{'".into());
        }

        let stmt = self.parse_stmt_block()?;

        Ok(Some(Stmt {
            kind: stmt.kind,
            span: stmt.span,
        }))
    }

    fn parse_item_class(&mut self) -> PResult<Box<Item>> {
        if !self.token.is_keyword(Keyword::Class) {
            return Err("Expected class".into());
        }

        if !self.is_ident_ahead() {
            return Err("Expected identifier".into());
        }

        let mut start_span = self.token.span;
        let vis = self.check_item_vis();

        if vis.is_some() {
            start_span = self.prev_token.span;
        };

        self.advance(); // Eat ident
        let ident = self.parse_ident()?;

        let generics = if self.token.kind == TokenKind::Lt {
            self.parse_item_generics()?
        } else {
            vec![]
        };
        let ext_clause = if self.token.is_keyword(Keyword::Extends) {
            self.parse_item_class_ext_clause()?
        } else {
            None
        };
        let impl_clause = if self.token.is_keyword(Keyword::Impl) {
            self.parse_item_class_impl_clause()?
        } else {
            None
        };
        let body = self.parse_item_class_body()?;

        let span = start_span.to(self.prev_token.span);
        Ok(Box::new(Item {
            span,
            kind: ItemKind::Class(Box::new(Class {
                generics,
                ext_clause,
                impl_clause,
                body,
            })),
            vis,
            ident,
        }))
    }

    fn parse_item_class_ext_clause(&mut self) -> PResult<Option<ExtClause>> {
        if self.token.is_keyword(Keyword::Extends) {
            return Ok(None);
        };
        if !self.is_ident_ahead() {
            return Err("Expected identifier".into());
        }
        self.advance(); // Eat ident
        let start_span = self.token.span;
        let ty = self.parse_ty()?;
        let span = start_span.to(self.prev_token.span);
        Ok(Some(ExtClause { ty, span }))
    }

    fn parse_item_class_impl_clause(&mut self) -> PResult<Option<ImplClause>> {
        if self.token.is_keyword(Keyword::Impl) {
            return Ok(None);
        };
        if !self.is_ident_ahead() {
            return Err("Expected identifier".into());
        }
        self.advance(); // Eat ident
        let start_span = self.token.span;

        let mut tys = Vec::new();

        let ty = self.parse_ty()?;
        tys.push(ty);

        while self.token.kind == TokenKind::BinOp(BinOpToken::Plus) {
            let new_ty = self.parse_ty()?;
            tys.push(new_ty);
        }

        let span = start_span.to(self.prev_token.span);
        Ok(Some(ImplClause { tys, span }))
    }

    fn parse_item_class_body(&mut self) -> PResult<ClassBody> {
        if self.token.kind != TokenKind::OpenDelim(Delimiter::Brace) {
            return Err("Expected '{'".into());
        }
        let mut fields = Vec::new();
        let mut methods = Vec::new();

        if self.is_keyword_ahead(&[Keyword::Fun])
            || self.look_ahead(2, |tok| tok.is_keyword(Keyword::Fun))
        {
            self.advance(); // Eat "fun" or "pub"
            methods = self.parse_item_class_methods()?;
        } else {
            self.advance(); // Eat "pub" or "var" or "const"
            fields = self.parse_item_class_fields()?;
            if self.is_keyword_ahead(&[Keyword::Fun]) || self.token.is_keyword(Keyword::Fun) {
                self.advance(); // Eat "fun" or "pub"
                methods = self.parse_item_class_methods()?;
            }
        };

        self.advance(); // Eat token after '}'

        Ok(ClassBody { fields, methods })
    }

    // pub? const age:int = 10 + 10, pub? var age:int;
    fn parse_item_class_fields(&mut self) -> PResult<Vec<ClassField>> {
        let mut class_fields = Vec::new();

        while !self.is_keyword_ahead(&[Keyword::Fun])
            && !self.token.is_keyword(Keyword::Fun)
            && self.token.kind != TokenKind::CloseDelim(Delimiter::Brace)
        {
            // Current token is "pub"|"const"|"var"

            if self.token.is_keyword(Keyword::Pub) {
                self.advance()
            };

            let vis = self.check_item_vis();

            // pub? const age = 10 + 10;
            let kind = if self.token.is_keyword(Keyword::Const) {
                if !self.is_ident_ahead() {
                    return Err("Expected identifier".into());
                }
                self.advance(); // Eat ident
                let ident = self.parse_ident()?;

                if self.token.kind != TokenKind::Colon {
                    return Err("Expected ':'".into());
                }

                if !self.is_ident_ahead() {
                    return Err("Expected identifier".into());
                }
                self.advance(); // Eat ident after ':'
                let ty = self.parse_ty()?;

                if self.token.kind != TokenKind::Eq {
                    return Err("Expected '='".into());
                }

                self.advance(); // Eat token after '='
                let expr = self.parse_expr()?;

                ClassFieldKind::Const(ident, ty, expr)
            } else if self.token.is_keyword(Keyword::Var) {
                // pub? var age:int;
                self.advance(); // Eat "var"
                if !self.is_ident_ahead() {
                    return Err("Expected identifier".into());
                }
                self.advance(); // Eat ident
                let ident = self.parse_ident()?;

                if self.token.kind != TokenKind::Colon {
                    return Err("Expected ':'".into());
                }

                if !self.is_ident_ahead() {
                    return Err("Expected identifier".into());
                }
                self.advance(); // Eat ident after ':'
                let ty = self.parse_ty()?;

                ClassFieldKind::Var(ident, ty)
            } else {
                return Err("Expected const or var".into());
            };

            let class_field = ClassField { vis, kind };

            class_fields.push(class_field);
            // current token is ';' or '}'
            self.advance(); // Eat "pub" | "const" | "var" | "fun" | '}'
        }

        Ok(class_fields)
    }

    fn parse_item_class_methods(&mut self) -> PResult<Vec<Item>> {
        let mut methods = Vec::new();

        while self.is_keyword_ahead(&[Keyword::Fun]) || self.token.is_keyword(Keyword::Fun) {
            if self.token.is_keyword(Keyword::Pub) {
                self.advance();
            }
            let method = self.parse_item_function()?;
            methods.push(*method);
        }

        Ok(methods)
    }

    fn parse_item_interface(&mut self) -> PResult<Box<Item>> {
        if !self.token.is_keyword(Keyword::Interface) {
            return Err("Expected interface".into());
        };

        if !self.is_ident_ahead() {
            return Err("Expected identifier".into());
        }

        let mut start_span = self.token.span;
        let vis = if self.prev_token.is_keyword(Keyword::Pub) {
            start_span = self.prev_token.span;
            Some(Visibility {
                kind: VisibilityKind::Public,
                span: self.prev_token.span,
            })
        } else {
            None
        };

        self.advance(); // Eat ident
        let ident = self.parse_ident()?;

        let generics = self.parse_item_generics()?;
        let ext_clause = self.parse_item_class_ext_clause()?;
        let body = self.parse_item_interface_body()?;

        let span = start_span.to(self.prev_token.span);

        Ok(Box::new(Item {
            vis,
            kind: ItemKind::Interface(Box::new(Interface {
                generics,
                ext_clause,
                body,
            })),
            ident,
            span,
        }))
    }

    fn parse_item_interface_body(&mut self) -> PResult<InterfaceBody> {
        if self.token.kind != TokenKind::OpenDelim(Delimiter::Brace) {
            return Err("Expected '{'".into());
        }
        let methods = Vec::new();

        while self.token.kind != TokenKind::CloseDelim(Delimiter::Brace) {
            if !self.is_keyword_ahead(&[Keyword::Pub]) {
                return Err("Expected pub".into());
            }
            self.advance(); // Eat "pub"
            let _method = self.parse_item_function()?;
            if self.prev_token.kind != TokenKind::Semicolon {
                return Err("This is not a function without body".into());
            }
        }

        self.advance(); // Eat token after '}'

        Ok(InterfaceBody { methods })
    }

    // <T ext Type1<T> + Type2<T>, V ext Type1<V> + Type2<V>>
    fn parse_item_generics(&mut self) -> PResult<Vec<GenericParam>> {
        self.expect_lt()?;

        let mut generic_params = Vec::new();
        let is_args_end = |token: &Token| {
            matches!(
                token.kind,
                TokenKind::Gt | TokenKind::BinOp(BinOpToken::Shr) | TokenKind::Ge
            )
        };

        loop {
            if self.look_ahead(0, is_args_end) {
                break;
            }

            let ident = self.parse_ident()?;
            let mut bounds = Vec::new();

            if self.token.is_keyword(Keyword::Extends) {
                self.advance();
                bounds.push(self.parse_ty()?);

                while self.token.kind == TokenKind::BinOp(BinOpToken::Plus) {
                    self.advance(); // Eat '+'
                    bounds.push(self.parse_ty()?);
                }
            }

            let generic_param = GenericParam { ident, bounds };
            generic_params.push(generic_param);

            if self.token.kind == TokenKind::Comma {
                self.advance(); // Eat ident
            }
        }

        self.expect_gt()?;
        Ok(generic_params)
    }

    fn check_item_vis(&mut self) -> Option<Visibility> {
        // current token is a token after pub, check before and return vis
        if self.prev_token.is_keyword(Keyword::Pub) {
            Some(Visibility {
                kind: VisibilityKind::Public,
                span: self.prev_token.span,
            })
        } else {
            None
        }
    }
}
