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
        let mut expr = if let Some(prefix_op) = self.curr_tok.maybe_prefix_op() {
            self.bump();
            let (operand, _) = self.parse_expr_with_context(Precedence::Prefix)?;
            Expr::Unary {
                span: DUMMY_SPAN,
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

            if is_assignment_op(infix_op) {
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
        let mut expr = self.parse_base_expr()?;

        loop {
            match &self.curr_tok.kind {
                TokenKind::LParen => {
                    // func call
                    todo!()
                }
                TokenKind::LBracket => {
                    // index expr
                    todo!()
                }
                TokenKind::Dot => {
                    // field expr
                    todo!()
                }
                TokenKind::Arrow => {
                    // pointer field expr
                    todo!()
                }
                TokenKind::PlusPlus => {
                    self.bump();
                    // Expr
                    expr = Expr::Unary {
                        span: DUMMY_SPAN,
                        op: UnaryOp::PostInc,
                        expr: Box::new(expr),
                    }
                }
                TokenKind::MinusMinus => {
                    self.bump();
                    // Expr
                    expr = Expr::Unary {
                        span: DUMMY_SPAN,
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
        match &self.curr_tok.kind {
            TokenKind::LParen => {
                self.bump();
                let inner = self.parse_expr()?;
                if !self.eat_no_expect(&TokenKind::RParen) {
                    // handle
                }
                Ok(inner)
            }
            TokenKind::Ident(_) => {
                let name = self.parse_ident()?;
                Ok(Expr::Ident {
                    span: DUMMY_SPAN,
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

    /*literal parsing will be improved*/
    fn parse_int_literal(&mut self, sym: EcoString) -> ParseResult<Expr> {
        self.bump();
        Ok(Expr::Constant {
            span: DUMMY_SPAN,
            value: Constant::Int(str::parse::<i64>(&sym).unwrap()),
        })
    }

    fn parse_string_literal(&mut self, sym: EcoString) -> ParseResult<Expr> {
        self.bump();
        Ok(Expr::Constant {
            span: DUMMY_SPAN,
            value: Constant::String(sym.to_string()),
        })
    }
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

pub fn is_assignment_op(op: BinaryOp) -> bool {
    matches!(
        op,
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
            | BinaryOp::ShrEq
    )
}
