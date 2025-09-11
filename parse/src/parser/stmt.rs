use ast::{ast::Stmt, span::DUMMY_SPAN};

use crate::{
    lexer::token::TokenKind,
    parser::{Parser, error::ParseErr},
};

impl<'a> Parser<'a> {
    pub fn parse_block(&mut self) -> Result<Stmt, ParseErr> {
        let start = self.curr_tok.span;
        if !self.eat_no_expect(&TokenKind::LCurly) {
            // handle err
        }

        let mut stmts = Vec::new();
        // while !self.look_ahead(1, |t| t == &TokenKind::RCurly || t == &TokenKind::Eof) {
        while !self.curr_tok.is_block_ender() {
            stmts.push(self.parse_statement()?);
        }

        if !self.eat_no_expect(&TokenKind::RCurly) {
            //handle
        }

        Ok(Stmt::Block {
            span: start.merge(self.prev_tok.span),
            stmts,
        })
    }

    fn parse_statement(&mut self) -> Result<Stmt, ParseErr> {
        use TokenKind::*;
        match &self.curr_tok.kind {
            Return => self.parse_return(),
            If => self.parse_if(),

            tok if tok.is_type_tok() => self.parse_var_decl(),

            _ => {
                let start = self.curr_tok.span;
                let expr = self.parse_expr()?;
                Ok(Stmt::Expr {
                    span: start.merge(self.prev_tok.span),
                    expr,
                })
            }
        }
    }

    fn parse_var_decl(&mut self) -> Result<Stmt, ParseErr> {
        let ty = self.parse_ty()?;
        let name = self.parse_ident_no_recover()?;

        // println!("Ty: {:?} | Name: {}\nNow {:?}", ty, name, self.curr_tok);

        let expr = match self.curr_tok.kind {
            TokenKind::Eq => {
                self.bump();
                Some(self.parse_expr()?)
            }
            TokenKind::Semi => None,
            _ => todo!(), // unexpected token
        };

        if !self.eat_no_expect(&TokenKind::Semi) {
            // err
        }

        // panic!(
        //     "Ty: {:?} | Name: {}\nExpr: {:?}\n\nNow {:?}",
        //     ty, name, expr, self.curr_tok
        // );
        //
        Ok(Stmt::VarDecl {
            span: DUMMY_SPAN,
            name,
            ty,
            init: expr,
        })
    }

    fn parse_if(&mut self) -> Result<Stmt, ParseErr> {
        if !self.eat_no_expect(&TokenKind::If) {
            // handle err
        }
        if !self.eat_no_expect(&TokenKind::LParen) {
            // handle err
        }
        let cond = self.parse_expr()?;
        if !self.eat_no_expect(&TokenKind::RParen) {
            // handle err
        }
        println!("\n\nHere\n\n");
        let then_branch = self.parse_block()?;
        let else_branch = match self.curr_tok.kind {
            TokenKind::Else => {
                self.bump();
                println!("In else {:?}", self.curr_tok.kind);
                Some(Box::new(self.parse_block()?))
            }
            _ => None,
        };
        println!("Cond: {:?}\n\nCurr => {:?}", cond, self.curr_tok);

        // let

        Ok(Stmt::If {
            span: DUMMY_SPAN,
            cond_then_ladder: vec![(cond, then_branch)],
            else_branch: else_branch,
        })
    }

    fn parse_return(&mut self) -> Result<Stmt, ParseErr> {
        let start = self.curr_tok.span;
        if !self.eat_no_expect(&TokenKind::Return) {
            // handle
        }

        println!("In return => {:?}", self.curr_tok);

        let expr = if self.curr_tok.kind == TokenKind::Semi {
            None
        } else {
            Some(self.parse_expr()?)
        };
        println!(
            "\n\nBack to return => {:?} with \n expr => {:?}",
            self.curr_tok, expr
        );
        if !self.eat_no_expect(&TokenKind::Semi) {
            // handle
        }

        Ok(Stmt::Return {
            span: start.merge(self.prev_tok.span),
            expr,
        })
    }
}
