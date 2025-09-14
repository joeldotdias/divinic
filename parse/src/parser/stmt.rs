use ast::ast::{Constant, Expr, Stmt, Type};

use crate::{
    lexer::token::{LitKind, TokenKind},
    parser::{
        Parser,
        error::{ParseErr, ParseErrKind, mk_parse_err},
    },
};

impl<'a> Parser<'a> {
    pub fn parse_block(&mut self) -> Result<Stmt, ParseErr> {
        let start = self.curr_tok.span;
        if !self.eat_no_expect(&TokenKind::LCurly) {
            return Err(mk_parse_err(
                ParseErrKind::FailedExpectation {
                    found: self.curr_tok.kind.clone(),
                    expected: TokenKind::LCurly,
                    context: "beginning of block",
                },
                self.curr_tok.span,
            ));
        }

        let mut stmts = Vec::new();
        while !self.curr_tok.is_block_ender() {
            stmts.push(self.parse_statement()?);
        }

        if !self.eat_no_expect(&TokenKind::RCurly) {
            return Err(mk_parse_err(
                ParseErrKind::FailedExpectation {
                    found: self.curr_tok.kind.clone(),
                    expected: TokenKind::RCurly,
                    context: "statement block",
                },
                self.curr_tok.span,
            ));
        }

        Ok(Stmt::Block {
            span: start.merge(self.prev_tok.span),
            stmts,
        })
    }

    fn parse_statement(&mut self) -> Result<Stmt, ParseErr> {
        match &self.curr_tok.kind {
            TokenKind::Return => self.parse_return(),
            TokenKind::If => self.parse_if(),
            TokenKind::For => self.parse_for(),
            TokenKind::While => self.parse_while(),
            TokenKind::Switch => self.parse_switch(),
            TokenKind::Break => self.parse_break(),
            TokenKind::Continue => self.parse_continue(),

            tok if tok.is_type_tok() => self.parse_var_decl(),

            TokenKind::Literal(lit) if lit.kind == LitKind::Str => self.parse_printf_call(),

            _ => {
                let start = self.curr_tok.span;
                let expr = self.parse_expr()?;
                if !self.eat_no_expect(&TokenKind::Semi) {
                    return Err(mk_parse_err(
                        ParseErrKind::FailedExpectation {
                            found: self.curr_tok.kind.clone(),
                            expected: TokenKind::Semi,
                            context: "statement block",
                        },
                        self.curr_tok.span,
                    ));
                }
                Ok(Stmt::Expr {
                    span: start.merge(self.prev_tok.span),
                    expr,
                })
            }
        }
    }

    fn parse_var_decl(&mut self) -> Result<Stmt, ParseErr> {
        let start = self.curr_tok.span;
        let mut ty = self.parse_ty()?;
        let name = self.parse_ident_no_recover()?;

        if self.eat_no_expect(&TokenKind::LBracket) {
            let count = self.parse_expr()?;
            ty = Type::Array(Box::new(ty), Some(count));

            if !self.eat_no_expect(&TokenKind::RBracket) {
                //err
            }
        }

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
            panic!("if should always start with if");
        }
        if !self.eat_no_expect(&TokenKind::LParen) {
            return Err(mk_parse_err(
                ParseErrKind::FailedExpectation {
                    found: self.curr_tok.kind.clone(),
                    expected: TokenKind::LParen,
                    context: "if condition",
                },
                self.curr_tok.span,
            ));
        }

        let cond = self.parse_expr()?;
        if !self.eat_no_expect(&TokenKind::RParen) {
            return Err(mk_parse_err(
                ParseErrKind::FailedExpectation {
                    found: self.curr_tok.kind.clone(),
                    expected: TokenKind::RParen,
                    context: "if condition",
                },
                self.curr_tok.span,
            ));
        }
        let then_branch = self.parse_block()?;

        let mut cond_then_ladder = vec![(cond, then_branch)];

        while self.check_no_expect(&TokenKind::Else) {
            if self.look_ahead(1, |tok| tok == &TokenKind::If) {
                // consume else and if
                self.bump();
                self.bump();

                if !self.eat_no_expect(&TokenKind::LParen) {
                    return Err(mk_parse_err(
                        ParseErrKind::FailedExpectation {
                            found: self.curr_tok.kind.clone(),
                            expected: TokenKind::LParen,
                            context: "if condition",
                        },
                        self.curr_tok.span,
                    ));
                }

                let cond = self.parse_expr()?;
                if !self.eat_no_expect(&TokenKind::RParen) {
                    return Err(mk_parse_err(
                        ParseErrKind::FailedExpectation {
                            found: self.curr_tok.kind.clone(),
                            expected: TokenKind::RParen,
                            context: "if condition",
                        },
                        self.curr_tok.span,
                    ));
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
            panic!("should always be a for");
        }

        if !self.eat_no_expect(&TokenKind::LParen) {
            return Err(mk_parse_err(
                ParseErrKind::FailedExpectation {
                    found: self.curr_tok.kind.clone(),
                    expected: TokenKind::LParen,
                    context: "for clause",
                },
                self.curr_tok.span,
            ));
        }

        let init = if !self.eat_no_expect(&TokenKind::Semi) {
            Some(Box::new(self.parse_statement()?))
        } else {
            None
        };

        let cond = if !self.eat_no_expect(&TokenKind::Semi) {
            let cond = Some(self.parse_expr()?);
            if !self.eat_no_expect(&TokenKind::Semi) {
                return Err(mk_parse_err(
                    ParseErrKind::FailedExpectation {
                        found: self.curr_tok.kind.clone(),
                        expected: TokenKind::Semi,
                        context: "for condition",
                    },
                    self.curr_tok.span,
                ));
            }
            cond
        } else {
            None
        };

        let step = if !self.eat_no_expect(&TokenKind::RParen) {
            let step = Some(self.parse_expr()?);
            if !self.eat_no_expect(&TokenKind::RParen) {
                return Err(mk_parse_err(
                    ParseErrKind::FailedExpectation {
                        found: self.curr_tok.kind.clone(),
                        expected: TokenKind::Semi,
                        context: "for step",
                    },
                    self.curr_tok.span,
                ));
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
            panic!("should only start with while");
        }

        if !self.eat_no_expect(&TokenKind::LParen) {
            return Err(mk_parse_err(
                ParseErrKind::FailedExpectation {
                    found: self.curr_tok.kind.clone(),
                    expected: TokenKind::LParen,
                    context: "while condition",
                },
                self.curr_tok.span,
            ));
        }

        let cond = self.parse_expr()?;
        if !self.eat_no_expect(&TokenKind::RParen) {
            return Err(mk_parse_err(
                ParseErrKind::FailedExpectation {
                    found: self.curr_tok.kind.clone(),
                    expected: TokenKind::RParen,
                    context: "while condition",
                },
                self.curr_tok.span,
            ));
        }

        let body = self.parse_block()?;

        Ok(Stmt::While {
            span: start.merge(self.prev_tok.span),
            cond,
            body: Box::new(body),
        })
    }

    fn parse_switch(&mut self) -> Result<Stmt, ParseErr> {
        let start = self.curr_tok.span;
        if !self.eat_no_expect(&TokenKind::Switch) {
            panic!("should always start with switch");
        }

        let nobounds = if self.eat_no_expect(&TokenKind::LParen) {
            false
        } else if self.eat_no_expect(&TokenKind::LBracket) {
            true
        } else {
            return Err(mk_parse_err(
                ParseErrKind::UnexpectedToken {
                    found: self.curr_tok.kind.clone(),
                    context: "switch subject",
                },
                self.curr_tok.span,
            ));
        };

        let subject = self.parse_expr()?;
        if nobounds {
            if self.eat_no_expect(&TokenKind::RBracket) {
                return Err(mk_parse_err(
                    ParseErrKind::FailedExpectation {
                        found: self.curr_tok.kind.clone(),
                        expected: TokenKind::RBracket,
                        context: "switch subject",
                    },
                    self.curr_tok.span,
                ));
            }
        } else {
            if !self.eat_no_expect(&TokenKind::RParen) {
                return Err(mk_parse_err(
                    ParseErrKind::FailedExpectation {
                        found: self.curr_tok.kind.clone(),
                        expected: TokenKind::RParen,
                        context: "switch subject",
                    },
                    self.curr_tok.span,
                ));
            }
        }

        if self.eat_no_expect(&TokenKind::LCurly) {
            return Err(mk_parse_err(
                ParseErrKind::FailedExpectation {
                    found: self.curr_tok.kind.clone(),
                    expected: TokenKind::LCurly,
                    context: "switch block",
                },
                self.curr_tok.span,
            ));
        }

        let cases = Parser::series_of(self, &Parser::parse_case, None)?;

        let default = if self.eat_no_expect(&TokenKind::Default) {
            if !self.eat_no_expect(&TokenKind::Colon) {
                return Err(mk_parse_err(
                    ParseErrKind::FailedExpectation {
                        found: self.curr_tok.kind.clone(),
                        expected: TokenKind::Colon,
                        context: "default case",
                    },
                    self.curr_tok.span,
                ));
            }
            let mut dstmts = Vec::new();
            while !self.curr_tok.is_case_ender() {
                dstmts.push(self.parse_statement()?);
            }
            Some(dstmts)
        } else {
            None
        };

        if self.eat_no_expect(&TokenKind::RCurly) {
            return Err(mk_parse_err(
                ParseErrKind::FailedExpectation {
                    found: self.curr_tok.kind.clone(),
                    expected: TokenKind::RCurly,
                    context: "switch block",
                },
                self.curr_tok.span,
            ));
        }

        Ok(Stmt::Switch {
            span: start.merge(self.prev_tok.span),
            subject,
            cases,
            default,
            nobounds,
        })
    }

    fn parse_case(&mut self) -> Result<Option<(Constant, Vec<Stmt>)>, ParseErr> {
        if !self.check_no_expect(&TokenKind::Case) {
            return Ok(None);
        }

        self.bump();

        let cval = if let Expr::Constant { value, .. } = self.parse_expr()? {
            value
        } else {
            todo!() // err here
        };

        if !self.eat_no_expect(&TokenKind::Colon) {
            return Err(mk_parse_err(
                ParseErrKind::FailedExpectation {
                    found: self.curr_tok.kind.clone(),
                    expected: TokenKind::Colon,
                    context: "case clause",
                },
                self.curr_tok.span,
            ));
        }

        let mut body = Vec::new();

        while !self.curr_tok.is_case_ender() {
            body.push(self.parse_statement()?);
        }

        Ok(Some((cval, body)))
    }

    fn parse_return(&mut self) -> Result<Stmt, ParseErr> {
        let start = self.curr_tok.span;
        if !self.eat_no_expect(&TokenKind::Return) {
            panic!("should always start with return");
        }

        let expr = if self.curr_tok.kind == TokenKind::Semi {
            None
        } else {
            Some(self.parse_expr()?)
        };
        if !self.eat_no_expect(&TokenKind::Semi) {
            return Err(mk_parse_err(
                ParseErrKind::FailedExpectation {
                    found: self.curr_tok.kind.clone(),
                    expected: TokenKind::Semi,
                    context: "return",
                },
                self.curr_tok.span,
            ));
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
            return Err(mk_parse_err(
                ParseErrKind::FailedExpectation {
                    found: self.curr_tok.kind.clone(),
                    expected: TokenKind::Semi,
                    context: "function call",
                },
                self.curr_tok.span,
            ));
        }

        Ok(Stmt::Expr {
            span: pspan.merge(self.prev_tok.span),
            expr: Expr::Call {
                span: pspan,
                func: "Print".into(),
                args,
            },
        })
    }

    fn parse_break(&mut self) -> Result<Stmt, ParseErr> {
        let start = self.curr_tok.span;
        if !self.eat_no_expect(&TokenKind::Break) {
            panic!("How did this even happen");
        }

        if !self.eat_no_expect(&TokenKind::Semi) {
            return Err(mk_parse_err(
                ParseErrKind::FailedExpectation {
                    found: self.curr_tok.kind.clone(),
                    expected: TokenKind::Semi,
                    context: "break",
                },
                self.curr_tok.span,
            ));
        }

        Ok(Stmt::Break {
            span: start.merge(self.prev_tok.span),
        })
    }

    fn parse_continue(&mut self) -> Result<Stmt, ParseErr> {
        let start = self.curr_tok.span;
        if !self.eat_no_expect(&TokenKind::Continue) {
            panic!("How did this even happen");
        }

        if !self.eat_no_expect(&TokenKind::Semi) {
            return Err(mk_parse_err(
                ParseErrKind::FailedExpectation {
                    found: self.curr_tok.kind.clone(),
                    expected: TokenKind::Semi,
                    context: "continue",
                },
                self.curr_tok.span,
            ));
        }

        Ok(Stmt::Continue {
            span: start.merge(self.prev_tok.span),
        })
    }
}
