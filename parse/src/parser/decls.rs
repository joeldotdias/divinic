use ast::{
    ast::{Declaration, Param, Stmt},
    span::DUMMY_SPAN,
};

use crate::{
    lexer::token::TokenKind,
    parser::{Parser, error::ParseErr},
};

impl<'a> Parser<'a> {
    // parse a function, class, var declaration or an include
    pub fn parse_top_level(&mut self) -> Result<Option<Declaration>, ParseErr> {
        // println!("{:?} | {:?}", self.prev_tok, self.curr_tok);
        let decl = if self.is_func_def() {
            self.parse_func()?
        } else {
            return Ok(None);
        };

        Ok(Some(decl))
    }

    fn is_func_def(&self) -> bool {
        self.curr_tok.is_type_tok() && self.look_ahead(2, |t| matches!(t, &TokenKind::LParen))
    }

    fn parse_func(&mut self) -> Result<Declaration, ParseErr> {
        let start = self.curr_tok.span;
        println!(
            "Inside parse_func\n{:?} | {:?}",
            self.prev_tok, self.curr_tok
        );
        let ret_ty = self.parse_ty()?; // TODO handle bad case here
        // self.bump(); // this line should go into the parse_ty function
        println!(
            "Parsed ty: {:?}\n now => {:?} | {:?}",
            ret_ty, self.prev_tok, self.curr_tok
        );
        let fn_name = self.parse_ident()?;
        println!("Parsed name: {}\n now => {:?}", fn_name, self.curr_tok);

        if !self.eat_no_expect(&TokenKind::LParen) {
            // return expected lparen
        }

        let params = Parser::series_of(
            self,
            &|parser| Parser::parse_fn_param(parser),
            Some(&TokenKind::Comma),
        )?;
        println!("Params => {:?}", params);

        if !self.eat_no_expect(&TokenKind::RParen) {
            // handle
        }

        println!("{:?}", self.curr_tok);

        let body = self.parse_block()?;

        // while !self.eat_no_expect(&TokenKind::RCurly) {
        //     self.bump();
        // }
        println!("After eating => {:?}", self.curr_tok);

        let fn_decl = Declaration::Func {
            span: start.merge(self.prev_tok.span),
            name: fn_name,
            ret_ty,
            params,
            body,
        };
        println!("Parsed func ");
        dbg!(&fn_decl);

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
}
