use ast::ast::{Expr, Stmt};

use crate::{
    lexer::token::{LitKind, TokenKind},
    parser::{Parser, error::ParseErr},
};

impl<'a> Parser<'a> {
    pub fn parse_block(&mut self) -> Result<Stmt, ParseErr> {
        let start = self.curr_tok.span;
        if !self.eat_no_expect(&TokenKind::LCurly) {
            // handle err
        }

        let mut stmts = Vec::new();
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
        println!("\n\nGoing to parse new statement: {:?}", self.curr_tok);
        println!("Next: {:?}", self.peek_token());

        match &self.curr_tok.kind {
            TokenKind::Return => self.parse_return(),
            TokenKind::If => self.parse_if(),
            TokenKind::For => self.parse_for(),
            TokenKind::While => self.parse_while(),
            TokenKind::Break => self.parse_break(),
            TokenKind::Continue => self.parse_continue(),

            tok if tok.is_type_tok() => self.parse_var_decl(),

            TokenKind::Literal(lit) if lit.kind == LitKind::Str => self.parse_printf_call(),

            _ => {
                println!(
                    "Parsing expression statement, curr_tok: {:?}",
                    self.curr_tok
                );
                let start = self.curr_tok.span;
                let expr = self.parse_expr()?;
                println!("Parsed expr: {:?}, now curr_tok: {:?}", expr, self.curr_tok);
                if !self.eat_no_expect(&TokenKind::Semi) {
                    println!("Expected semicolon but found: {:?}", self.curr_tok);
                    // handle
                }
                println!("Successfully parsed expression statement");
                Ok(Stmt::Expr {
                    span: start.merge(self.prev_tok.span),
                    expr,
                })
            }
        }
    }

    fn parse_var_decl(&mut self) -> Result<Stmt, ParseErr> {
        let start = self.curr_tok.span;
        let ty = self.parse_ty()?;
        let name = self.parse_ident_no_recover()?;

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

        Ok(Stmt::VarDecl {
            span: start.merge(self.prev_tok.span),
            name,
            ty,
            init: expr,
        })
    }

    fn parse_if(&mut self) -> Result<Stmt, ParseErr> {
        let start = self.curr_tok.span;
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
        let then_branch = self.parse_block()?;

        let mut cond_then_ladder = vec![(cond, then_branch)];

        while self.check_no_expect(&TokenKind::Else) {
            if self.look_ahead(1, |tok| tok == &TokenKind::If) {
                // consume else and if
                self.bump();
                self.bump();

                if !self.eat_no_expect(&TokenKind::LParen) {
                    // handle err
                }

                let cond = self.parse_expr()?;
                if !self.eat_no_expect(&TokenKind::RParen) {
                    // handle err
                }
                let then_branch = self.parse_block()?;
                cond_then_ladder.push((cond, then_branch));
            } else {
                break;
            }
        }

        let else_branch = match self.curr_tok.kind {
            TokenKind::Else => {
                self.bump();
                Some(Box::new(self.parse_block()?))
            }
            _ => None,
        };

        Ok(Stmt::If {
            span: start.merge(self.prev_tok.span),
            cond_then_ladder,
            else_branch,
        })
    }

    fn parse_for(&mut self) -> Result<Stmt, ParseErr> {
        let start = self.curr_tok.span;
        if !self.eat_no_expect(&TokenKind::For) {
            // handle
        }

        if !self.eat_no_expect(&TokenKind::LParen) {
            // handle
        }

        let init = if !self.eat_no_expect(&TokenKind::Semi) {
            Some(Box::new(self.parse_statement()?))
        } else {
            None
        };

        println!("\n\nInit: {:?}\nAnd now: {:?}", init, self.curr_tok);

        let cond = if !self.eat_no_expect(&TokenKind::Semi) {
            let cond = Some(self.parse_expr()?);
            if !self.eat_no_expect(&TokenKind::Semi) {
                // handle
            }
            cond
        } else {
            None
        };

        let step = if !self.eat_no_expect(&TokenKind::RParen) {
            let step = Some(self.parse_expr()?);
            if !self.eat_no_expect(&TokenKind::RParen) {
                // handle
            }
            step
        } else {
            None
        };

        let body = self.parse_block()?;

        Ok(Stmt::For {
            span: start.merge(self.prev_tok.span),
            init,
            cond,
            step,
            body: Box::new(body),
        })
    }

    fn parse_while(&mut self) -> Result<Stmt, ParseErr> {
        let start = self.curr_tok.span;
        if !self.eat_no_expect(&TokenKind::While) {
            // handle
        }

        if !self.eat_no_expect(&TokenKind::LParen) {
            // handle
        }

        let cond = self.parse_expr()?;
        if !self.eat_no_expect(&TokenKind::RParen) {
            // handle
        }

        let body = self.parse_block()?;

        Ok(Stmt::While {
            span: start.merge(self.prev_tok.span),
            cond,
            body: Box::new(body),
        })
    }

    fn parse_return(&mut self) -> Result<Stmt, ParseErr> {
        let start = self.curr_tok.span;
        if !self.eat_no_expect(&TokenKind::Return) {
            // handle
        }

        let expr = if self.curr_tok.kind == TokenKind::Semi {
            None
        } else {
            Some(self.parse_expr()?)
        };
        if !self.eat_no_expect(&TokenKind::Semi) {
            // handle
        }

        Ok(Stmt::Return {
            span: start.merge(self.prev_tok.span),
            expr,
        })
    }

    fn parse_printf_call(&mut self) -> Result<Stmt, ParseErr> {
        let start = self.curr_tok.span;

        let args = Parser::series_of(
            self,
            &|parser: &mut Parser| parser.parse_expr().map(Some),
            Some(&TokenKind::Comma),
        )?;

        let pspan = start.merge(self.prev_tok.span);
        if !self.eat_no_expect(&TokenKind::Semi) {
            println!("Expected semicolon but found: {:?}", self.curr_tok);
            // handle
        }

        Ok(Stmt::Expr {
            span: pspan.merge(self.prev_tok.span),
            expr: Expr::Call {
                span: pspan,
                func: "printf".into(),
                args,
            },
        })
    }

    fn parse_break(&mut self) -> Result<Stmt, ParseErr> {
        let start = self.curr_tok.span;
        if !self.eat_no_expect(&TokenKind::Break) {
            // handle
        }

        if !self.eat_no_expect(&TokenKind::Semi) {
            // handle
        }

        Ok(Stmt::Break {
            span: start.merge(self.prev_tok.span),
        })
    }

    fn parse_continue(&mut self) -> Result<Stmt, ParseErr> {
        let start = self.curr_tok.span;
        if !self.eat_no_expect(&TokenKind::Continue) {
            // handle
        }

        if !self.eat_no_expect(&TokenKind::Semi) {
            // handle
        }

        Ok(Stmt::Continue {
            span: start.merge(self.prev_tok.span),
        })
    }
}
