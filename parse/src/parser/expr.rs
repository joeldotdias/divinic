use ast::{
    ast::{BinaryOp, Constant, Expr, UnaryOp},
    span::DUMMY_SPAN,
};
use ecow::EcoString;

use crate::{
    lexer::token::{LitKind, TokenKind},
    parser::{ParseResult, Parser},
};

#[derive(Clone, Copy, Debug, PartialEq, PartialOrd)]
pub enum Precedence {
    Lowest,
    Assign,   // = += -= *= /= %= ^= &= |= <<= >>=
    Ternary,  // ? :
    LOr,      // ||
    LAnd,     // &&
    BitOr,    // |
    BitXor,   // ^
    BitAnd,   // &
    Equality, // == !=
    Compare,  // < > <= >=
    Shift,    // << >>
    Sum,      // + -
    Product,  // * / %
    Prefix,   // unary - + * & ! ~ ++ --
    Postfix,  // () [] . -> ++ --
}

// left_binding, right_binding
type PrecInfo = (Precedence, Precedence);

impl<'a> Parser<'a> {
    pub fn parse_expr(&mut self) -> ParseResult<Expr> {
        let (expr, _) = self.parse_expr_with_context(Precedence::Lowest)?;
        Ok(expr)
    }

    pub fn parse_expr_with_context(
        &mut self,
        prec_threshold: Precedence,
    ) -> ParseResult<(Expr, Option<BinaryOp>)> {
        let start = self.curr_tok.span;

        let mut expr = if let Some(prefix_op) = self.curr_tok.maybe_prefix_op() {
            self.bump();
            let (operand, _) = self.parse_expr_with_context(Precedence::Prefix)?;
            Expr::Unary {
                span: start.merge(self.prev_tok.span),
                op: prefix_op,
                expr: Box::new(operand),
            }
        } else {
            self.parse_suffixed_atom()?
        };

        let mut curr_infix = self.curr_tok.maybe_infix_op();

        while let Some(infix_op) = curr_infix {
            let prec_info = prec_info_from_infix(infix_op);
            if prec_info.0 < prec_threshold {
                break;
            }

            self.bump(); // move past infix token

            if infix_op.is_assignment_op() {
                let (right_expr, next_infix) = self.parse_expr_with_context(prec_info.0)?;
                expr = Expr::Assign {
                    span: DUMMY_SPAN,
                    op: infix_op,
                    lhs: Box::new(expr),
                    rhs: Box::new(right_expr),
                };
                curr_infix = next_infix;
            } else {
                let (right_expr, next_infix) = self.parse_expr_with_context(prec_info.1)?;
                expr = Expr::Binary {
                    span: DUMMY_SPAN,
                    op: infix_op,
                    lhs: Box::new(expr),
                    rhs: Box::new(right_expr),
                };
                curr_infix = next_infix;
            };
        }

        Ok((expr, curr_infix))
    }

    fn parse_suffixed_atom(&mut self) -> ParseResult<Expr> {
        let start = self.curr_tok.span;
        let mut expr = self.parse_base_expr()?;

        loop {
            match &self.curr_tok.kind {
                TokenKind::LParen => {
                    // // func call
                    // todo!()
                    expr = self.parse_func_call(expr)?;
                }
                TokenKind::LBracket => {
                    self.bump();
                    let index = self.parse_expr()?;
                    if !self.eat_no_expect(&TokenKind::RBracket) {}
                    expr = Expr::Index {
                        span: start.merge(self.prev_tok.span),
                        base: Box::new(expr),
                        index: Box::new(index),
                    }
                }
                TokenKind::Dot => {
                    self.bump();
                    let field = self.parse_expr()?;
                    expr = Expr::Member {
                        span: start.merge(self.prev_tok.span),
                        base: Box::new(expr),
                        field: Box::new(field),
                        arrow: false,
                    }
                }
                TokenKind::Arrow => {
                    self.bump();
                    let field = self.parse_expr()?;
                    expr = Expr::Member {
                        span: start.merge(self.prev_tok.span),
                        base: Box::new(expr),
                        field: Box::new(field),
                        arrow: true,
                    }
                }
                TokenKind::PlusPlus => {
                    self.bump();
                    expr = Expr::Unary {
                        span: start.merge(self.prev_tok.span),
                        op: UnaryOp::PostInc,
                        expr: Box::new(expr),
                    }
                }
                TokenKind::MinusMinus => {
                    self.bump();
                    expr = Expr::Unary {
                        span: start.merge(self.prev_tok.span),
                        op: UnaryOp::PostDec,
                        expr: Box::new(expr),
                    }
                }
                _ => break,
            }
        }

        Ok(expr)
    }

    fn parse_base_expr(&mut self) -> ParseResult<Expr> {
        let start = self.curr_tok.span;
        match &self.curr_tok.kind {
            TokenKind::LParen => {
                self.bump();
                let inner = self.parse_expr()?;
                if !self.eat_no_expect(&TokenKind::RParen) {
                    // handle
                }
                Ok(inner)
            }
            TokenKind::LCurly => {
                self.bump();
                let elems = Parser::series_of(
                    self,
                    &|parser: &mut Parser| parser.parse_expr().map(Some),
                    Some(&TokenKind::Comma),
                )?;
                if !self.eat_no_expect(&TokenKind::RCurly) {
                    //err
                }

                Ok(Expr::ArrElems {
                    span: start.merge(self.prev_tok.span),
                    elems,
                })
            }
            TokenKind::Ident(_) => {
                let name = self.parse_ident()?;
                Ok(Expr::Ident {
                    span: start.merge(self.prev_tok.span),
                    name,
                })
            }
            TokenKind::Literal(lit) => {
                let sym = lit.symbol.clone();
                match lit.kind {
                    LitKind::Bool => todo!(),
                    LitKind::Char => todo!(),
                    LitKind::Integer => self.parse_int_literal(sym),
                    LitKind::Float => todo!(),
                    LitKind::Str => self.parse_string_literal(sym),
                }
            }
            _ => {
                println!("\n\nHuh: {:?}", self.curr_tok);
                todo!() //
            }
        }
    }

    fn parse_func_call(&mut self, label: Expr) -> ParseResult<Expr> {
        let start = self.curr_tok.span;
        let fn_name = if let Expr::Ident { name, .. } = label {
            name
        } else {
            todo!() // err
        };

        if !self.eat_no_expect(&TokenKind::LParen) {}

        let args = if self.eat_no_expect(&TokenKind::RParen) {
            vec![]
        } else {
            let args = Parser::series_of(
                self,
                &|parser: &mut Parser| parser.parse_expr().map(Some),
                Some(&TokenKind::Comma),
            )?;
            if !self.eat_no_expect(&TokenKind::RParen) {}
            args
        };

        Ok(Expr::Call {
            span: start.merge(self.prev_tok.span),
            func: fn_name,
            args,
        })
    }

    /*literal parsing will be improved*/
    fn parse_int_literal(&mut self, sym: EcoString) -> ParseResult<Expr> {
        let span = self.curr_tok.span;
        self.bump();
        Ok(Expr::Constant {
            span,
            value: Constant::Int(str::parse::<i64>(&sym).unwrap()),
        })
    }

    fn parse_string_literal(&mut self, sym: EcoString) -> ParseResult<Expr> {
        let span = self.curr_tok.span;
        self.bump();
        // the expects here are only because if this happens its a problem with the lexer
        // better to panic so i know there's something wrong that to report this
        let raw_str = sym
            .strip_prefix('"')
            .expect("string should always open with a quote")
            .strip_suffix('"')
            .expect("string should always close with a quote");
        let str_contents = process_escape_sequences(raw_str);

        Ok(Expr::Constant {
            span,
            value: Constant::String(str_contents),
        })
    }
}

pub fn process_escape_sequences(s: &str) -> String {
    let mut result = String::new();
    let mut chars = s.chars().peekable();

    while let Some(c) = chars.next() {
        if c == '\\' {
            // Handle escape sequence
            if let Some(next_char) = chars.next() {
                match next_char {
                    'n' => result.push('\n'),
                    't' => result.push('\t'),
                    'r' => result.push('\r'),
                    '0' => result.push('\0'),
                    '\\' => result.push('\\'),
                    '"' => result.push('"'),
                    '\'' => result.push('\''),
                    _ => {
                        // Unknown escape sequence, keep both characters
                        result.push('\\');
                        result.push(next_char);
                    }
                }
            } else {
                // Backslash at end of string
                result.push('\\');
            }
        } else {
            result.push(c);
        }
    }

    result
}

pub fn prec_info_from_infix(op: BinaryOp) -> PrecInfo {
    use Precedence::*;

    match op {
        BinaryOp::Eq
        | BinaryOp::AddEq
        | BinaryOp::SubEq
        | BinaryOp::MulEq
        | BinaryOp::DivEq
        | BinaryOp::ModEq
        | BinaryOp::BitAndEq
        | BinaryOp::BitXorEq
        | BinaryOp::BitOrEq
        | BinaryOp::ShlEq
        | BinaryOp::ShrEq => (Assign, Lowest),
        BinaryOp::Or => (LOr, LOr),
        BinaryOp::And => (LAnd, LAnd),
        BinaryOp::BitOr => (BitOr, BitOr),
        BinaryOp::BitXor => (BitXor, BitOr),
        BinaryOp::BitAnd => (BitAnd, BitAnd),
        BinaryOp::EqEq | BinaryOp::Ne => (Equality, Equality),
        BinaryOp::Lt | BinaryOp::Le | BinaryOp::Gt | BinaryOp::Ge => (Compare, Compare),
        BinaryOp::Shl | BinaryOp::Shr => (Shift, Shift),
        BinaryOp::Add | BinaryOp::Sub => (Sum, Sum),
        BinaryOp::Mul | BinaryOp::Div | BinaryOp::Mod => (Product, Product),
    }
}
