use ast::ast::{Declaration, Param};

use crate::{
    lexer::token::{LitInner, LitKind, TokenKind},
    parser::{ParseResult, Parser, error::ParseErr},
};

impl<'a> Parser<'a> {
    // parse a function, class, var declaration or an include
    pub fn parse_top_level(&mut self) -> Result<Option<Declaration>, ParseErr> {
        let decl = if self.is_func_def() {
            self.parse_func()?
        } else {
            match &self.curr_tok.kind {
                TokenKind::Pound if self.look_ahead(1, |t| matches!(t, &TokenKind::Include)) => {
                    self.parse_include()?
                }
                _ => return Ok(None),
            }
            // return Ok(None);
        };

        Ok(Some(decl))
    }

    fn is_func_def(&self) -> bool {
        self.curr_tok.is_type_tok() && self.look_ahead(2, |t| matches!(t, &TokenKind::LParen))
    }

    fn parse_func(&mut self) -> Result<Declaration, ParseErr> {
        let start = self.curr_tok.span;
        let ret_ty = self.parse_ty()?; // TODO handle bad case here
        let fn_name = self.parse_ident()?;

        if !self.eat_no_expect(&TokenKind::LParen) {
            // return expected lparen
        }

        let params = Parser::series_of(
            self,
            &|parser| Parser::parse_fn_param(parser),
            Some(&TokenKind::Comma),
        )?;

        if !self.eat_no_expect(&TokenKind::RParen) {
            // handle
        }

        let body = self.parse_block()?;

        // while !self.eat_no_expect(&TokenKind::RCurly) {
        //     self.bump();
        // }

        let fn_decl = Declaration::Func {
            span: start.merge(self.prev_tok.span),
            name: fn_name,
            ret_ty,
            params,
            body,
        };

        Ok(fn_decl)
    }

    fn parse_fn_param(&mut self) -> Result<Option<Param>, ParseErr> {
        if self.check_no_expect(&TokenKind::RParen) {
            return Ok(None);
        }

        let pstart = self.curr_tok.span;
        let ty = self.parse_ty()?;
        let name = self.parse_ident()?;

        Ok(Some(Param {
            span: pstart.merge(self.prev_tok.span),
            name,
            ty,
        }))
    }

    fn parse_include(&mut self) -> ParseResult<Declaration> {
        if !self.eat_no_expect(&TokenKind::Pound) {
            // err
        }
        if !self.eat_no_expect(&TokenKind::Include) {
            // err
        }

        let label = if let TokenKind::Literal(LitInner { symbol, .. }) = &self.curr_tok.kind {
            symbol.clone()
        } else {
            todo!() // err here
        };

        let fp = label.strip_prefix('"').unwrap().strip_suffix('"').unwrap();

        self.bump();
        println!("Now curr: {:?}", self.curr_tok);

        Ok(Declaration::Include {
            name: fp.into(),
            as_name: None,
        })
    }
}
