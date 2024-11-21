use crate::ast::{BinOpToken, Class, ClassBody, ClassField, ClassFieldKind, Delimiter, ExtClause, Fun, FunParam, FunRetTy, FunSig, GenericParam, Ident, ImplClause, Interface, InterfaceBody, Item, ItemKind, Mutability, SelfKind, SelfParam, Stmt, StmtKind, Token, TokenKind, Visibility, VisibilityKind};
use crate::kw::Keyword;
use super::{PResult, Parser};

impl Parser<'_> {
    pub fn parse_item(&mut self) -> PResult<Item> {
        if self.token.is_keyword(Keyword::Pub){
            self.advance(); // Eat token after "pub"
        };
        if self.token.is_keyword(Keyword::Class) {
            self.parse_item_class()
        } else if self.token.is_keyword(Keyword::Fun)  {
            self.parse_item_class()
        } else if self.token.is_keyword(Keyword::Interface)  {
            self.parse_item_class()
        } else {
            todo!()
        }
    }

    pub fn parse_item_function(&mut self) -> PResult<Item> {
        if !self.token.kind != Keyword::Fun {
            return Err("Expected function".into());
        }

        let mut start_span = self.token.span;
        let vis = if self.prev_token.is_keyword(Keyword::Pub) {
            start_span = self.prev_token.span;
            Visibility{
                kind:VisibilityKind::Public,
                span:self.prev_token.span,
            }
        } else {
            Visibility{
                kind:VisibilityKind::Private,
                span:self.token.span,
            }
        };

        if !self.is_ident_ahead(){
            return Err("Expected identifier".into());
        }

        self.advance(); // Eat ident
        let ident = self.parse_item_ident()?;

        let generics = self.parse_item_generics()?;
        let sig = self.parse_item_function_sig()?;
        let body = self.parse_item_function_body()?;


        let span = start_span.to(self.prev_token.span);

        Ok(Item{
            span,
            kind:ItemKind::Fun(Box::new(Fun {
                generics,
                sig,
                body,
            })),
            vis ,
            ident,
        })

    }

    // (mut? self:Type?, a:A, b:B)
    pub fn parse_item_function_sig(&mut self) -> PResult<FunSig> {
        if !self.token.kind != TokenKind::OpenDelim(Delimiter::Parenthesis) {
            return Err("Expected parenthesis".into());
        }
        let start_span = self.token.span;

        // self_param
        let self_param = if self.is_keyword_ahead(&[Keyword::Mut]) {
            self.advance(); // Eat "mut"
            let start_self_param_span = self.token.span;
            if !self.is_keyword_ahead(&[Keyword::SelfLower]) {
                return Err("Expected SelfLower".into());
            }
            self.advance(); // Eat "self"
            if self.look_ahead(1,|tok|tok.kind == TokenKind::Colon) {
                self.advance(); // Eat ':'
                if !self.is_ident_ahead(){
                    return Err("Expected identifier".into());
                }
                self.advance(); // Eat ident
                let ty = self.parse_ty()?;
                Some(SelfParam{
                    kind:SelfKind::Explicit(ty, Mutability::Mutable),
                    span:start_self_param_span.to(self.token.span),
                })
            } else {
                Some(SelfParam{
                    kind:SelfKind::Value(Mutability::Mutable),
                    span:start_self_param_span.to(self.token.span),
                })
            }
        } else {
            None
        };

        // fun_params
        let mut fun_params = Vec::new();
        self.advance(); // Eat token after '('
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

        if self.look_ahead(1,|tok|tok.kind != TokenKind::BinOp(BinOpToken::Minus))
            || self.look_ahead(2,|tok|tok.kind != TokenKind::Gt) {
            return Err("Expected '-''>'".into());
        }

        self.advance(); // Eat '-'
        self.advance(); // Eat '>'
        if !self.is_ident_ahead() {
            return Err("Expected type".into());

        }

        let output = if self.is_keyword_ahead(&[Keyword::Void]){
            self.advance(); // Eat "void"
            self.advance(); // Eat token after "void"
            FunRetTy::Default(self.prev_token.span)
        } else if self.is_ident_ahead(){
            self.advance(); // Eat ident
            FunRetTy::Ty(self.parse_ty()?)
        } else {
            return Err("Expected a type".into());
        };

        let span = start_span.to(self.prev_token.span);

        let inputs = (self_param,fun_params);
        Ok(
            FunSig{
                inputs,
                output,
                span,
            }
        )
    }

    pub fn parse_item_function_param(&mut self) -> PResult<FunParam> {
        if !self.token.is_ident() {
            return Err("Expected identifier".into());
        }
        let start_span = self.token.span;

        let ident = self.parse_item_ident()?;
        let ty = self.parse_ty()?;

        let span = start_span.to(self.prev_token.span);

        Ok(
            FunParam{
                ty,
                ident,
                span
            }
        )
    }

    pub fn parse_item_function_body(&mut self) -> PResult<Option<Stmt>> {
        if self.token.kind == TokenKind::Semicolon {
            self.advance(); // Eat token after ';'
            return Ok(None);
        }
        if !self.token.kind != TokenKind::OpenDelim(Delimiter::Brace) {
            return Err("Expected '{'".into());
        }

        let Stmt {kind,span} = self.parse_stmt_block();

        Ok(Some(Stmt{
            kind,
            span
        }))
    }

    pub fn parse_item_class(&mut self) -> PResult<Item> {
        if !self.token.kind != Keyword::Class {
            return Err("Expected function".into());
        }

        if !self.is_ident_ahead(){
            return Err("Expected identifier".into());
        }

        let mut start_span = self.token.span;
        let vis = if self.prev_token.is_keyword(Keyword::Pub) {
            start_span = self.prev_token.span;
            Visibility{
                kind:VisibilityKind::Public,
                span:self.prev_token.span,
            }
        } else {
            Visibility{
                kind:VisibilityKind::Private,
                span:self.token.span,
            }
        };

        self.advance(); // Eat ident
        let ident = self.parse_item_ident()?;

        let generics = self.parse_item_generics()?;
        let ext_clause= self.parse_item_class_ext_clause()?;
        let impl_clause= self.parse_item_class_impl_clause()?;
        let body = self.parse_item_class_body()?;

        let span = start_span.to(self.prev_token.span);
        Ok(Item{
            span,
            kind:ItemKind::Class(Box::new(Class {
                generics,
                ext_clause,
                impl_clause,
                body
            })),
            vis,
            ident
        })
    }

    pub fn parse_item_class_ext_clause(&mut self) -> PResult<Option<ExtClause>> {
        if !self.token.kind != Keyword::Extends {
            return Ok(None);
        };
        if !self.is_ident_ahead() {
            return Err("Expected identifier".into());
        }
        self.advance(); // Eat ident
        let start_span = self.token.span;
        let ty = self.parse_ty()?;
        let span = start_span.to(self.prev_token.span);
        Ok(
            Some(ExtClause{
                ty,
                span,
            })
        )
    }

    pub fn parse_item_class_impl_clause(&mut self) -> PResult<Option<ImplClause>> {
        if !self.token.kind != Keyword::Impl {
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
        Ok(
            Some(ImplClause{
                tys,
                span,
            })
        )
    }

    pub fn parse_item_class_body(&mut self) -> PResult<ClassBody> {
        if !self.token.kind != TokenKind::OpenDelim(Delimiter::Brace) {
            return Err("Expected '{'".into());
        }
        let mut fields = Vec::new();
        let mut methods = Vec::new();

        if self.is_keyword_ahead(&[Keyword::Fun]) || self.look_ahead(2,|tok|tok.is_keyword(Keyword::Fun)){
            self.advance(); // Eat "fun" or "pub"
            methods = self.parse_item_class_methods()?;
        } else {
            self.advance(); // Eat "pub" or "var" or "const"
            fields = self.parse_item_class_fields()?;
            if self.is_keyword_ahead(&[Keyword::Fun]) || self.token.is_keyword(Keyword::Fun){
                self.advance(); // Eat "fun" or "pub"
                methods = self.parse_item_class_methods()?;
            }
        };

        self.advance(); // Eat token after '}'

        Ok(ClassBody{
            fields,
            methods,
        })
    }

    // pub? const age:int = 10 + 10, pub? var age:int;
    pub fn parse_item_class_fields(&mut self) -> PResult<Vec<ClassField>> {
        let mut class_fields = Vec::new();

        while !self.is_keyword_ahead(&[Keyword::Fun]) && !self.token.is_keyword(Keyword::Fun) && !self.token.kind != TokenKind::CloseDelim(Delimiter::Brace) {
            // Current token is "pub"|"const"|"var"

            let vis = if self.is_keyword(Keyword::Pub) {
                self.advance(); // Eat token after "pub"
                Visibility{
                    kind:VisibilityKind::Public,
                    span:self.prev_token.span,
                }
            } else {
                Visibility{
                    kind:VisibilityKind::Private,
                    span:self.token.span,
                }
            };

            // pub? const age = 10 + 10;
            let kind = if self.token.is_keyword(Keyword::Const) {
                if !self.is_ident_ahead() {
                    return Err("Expected identifier".into());
                }
                self.advance(); // Eat ident
                let ident = self.parse_item_ident()?;

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
                let  expr = self.parse_expr()?;

                ClassFieldKind::Const(ident,ty,expr)
            } else if self.token.is_keyword(Keyword::Var) {// pub? var age:int;
                self.advance(); // Eat "var"
                if !self.is_ident_ahead() {
                    return Err("Expected identifier".into());
                }
                self.advance(); // Eat ident
                let ident = self.parse_item_ident()?;

                if self.token.kind != TokenKind::Colon {
                    return Err("Expected ':'".into());
                }

                if !self.is_ident_ahead() {
                    return Err("Expected identifier".into());
                }
                self.advance(); // Eat ident after ':'
                let ty = self.parse_ty()?;

                ClassFieldKind::Var(ident,ty)
            } else {
                return  Err("Expected const or var".into());
            };

            let class_field = ClassField{
                vis,
                kind
            };

            class_fields.push(class_field);
            // current token is ';' or '}'
            self.advance(); // Eat "pub" | "const" | "var" | "fun" | '}'
        }

        Ok(class_fields)
    }

    pub fn parse_item_class_methods(&mut self) -> PResult<Vec<Item>> {
        let mut methods = Vec::new();

        while self.is_keyword_ahead(&[Keyword::Fun]) || self.token.is_keyword(Keyword::Fun) {
            if self.token.is_keyword(Keyword::Pub) {
                self.advance();
            }
            let method = self.parse_item_function()?;
            methods.push(method);
        }

        Ok(methods)
    }

    pub fn parse_item_interface(&mut self) -> PResult<Item> {
        if !self.token.is_keyword(Keyword::Interface)  {
            return Err("Expected interface".into());
        };

        if !self.is_ident_ahead(){
            return Err("Expected identifier".into());
        }

        let mut start_span = self.token.span;
        let vis = if self.prev_token.is_keyword(Keyword::Pub) {
            start_span = self.prev_token.span;
            Visibility{
                kind:VisibilityKind::Public,
                span:self.prev_token.span,
            }
        } else {
            Visibility{
                kind:VisibilityKind::Private,
                span:self.token.span,
            }
        };

        self.advance(); // Eat ident
        let ident = self.parse_item_ident()?;

        let generics = self.parse_item_generics()?;
        let ext_clause= self.parse_item_class_ext_clause()?;
        let body = self.parse_item_interface_body()?;

        let span = start_span.to(self.prev_token.span);


        Ok(Item{
            vis,
            kind:ItemKind::Interface(Box::new(Interface{
                generics,
                ext_clause,
                body,
            })),
            ident,
            span,
        })
    }

    pub fn parse_item_interface_body(&mut self) -> PResult<InterfaceBody> {
        if self.token.kind != TokenKind::OpenDelim(Delimiter::Brace) {
            return Err("Expected '{'".into());
        }
        let mut methods = Vec::new();

        while self.token.kind != TokenKind::CloseDelim(Delimiter::Brace) {
            if !self.is_keyword_ahead(&[Keyword::Pub]) {
                return Err("Expected pub".into());
            }
            self.advance(); // Eat "pub"
            let method = self.parse_item_function()?;
            if self.prev_token.kind != TokenKind::Semicolon {
                return Err("This is not a function without body".into());
            }
        }

        self.advance(); // Eat token after '}'

        Ok(InterfaceBody{
            methods,
        })
    }

    pub fn parse_item_ident(&mut self) -> PResult<Ident> {
        if !self.token.is_ident() {
            return Err("Expected identifier".into());
        }
        let ident = self.token.ident().unwrap().0;
        self.advance(); // Eat token after ident
        Ok(ident)
    }

    // <T ext Type1<T> + Type2<T>, V ext Type1<V> + Type2<V>>
    pub fn parse_item_generics(&mut self) -> PResult<Vec<GenericParam>> {
        if !self.token.kind != TokenKind::Lt {
            return Err("Expected function".into());
        }

        let mut generic_params = Vec::new();
        self.advance(); // Eat token after '<'
        while self.token.kind != TokenKind::Gt {
            if !self.is_ident(){
                return Err("Expected identifier".into());
            }
            let ident = self.parse_item_ident()?;
            let mut bounds = Vec::new();

            if self.token.is_keyword(Keyword::Extends) {
                self.advance(); // Eat token after "ext"
                bounds.push(self.parse_ty()?);
            }

            while self.token.kind == TokenKind::BinOp(BinOpToken::Plus) {
                self.advance(); // Eat '+'
                bounds.push(self.parse_ty()?);
            }

            let generic_param = GenericParam{
                ident,
                bounds,
            };

            generic_params.push(generic_param);

            if self.token.kind == TokenKind::Comma && self.is_ident_ahead() {
                self.advance(); // Eat ident
            } else {
                break;
            };
        }

        if self.token.kind != TokenKind::Gt {
            return Err("Expected gt".into());
        }

        self.advance(); // Eat token after '>'

        Ok(generic_params)

    }
}
