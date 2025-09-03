use ast::span::Span;
use cursor::{
    Cursor,
    rawtoken::{LiteralKind, RawTokenKind},
};
use ecow::EcoString;

use crate::{
    lexer::{
        token::{Delimiter, LitInner, LitKind, Token, TokenKind},
        tokentree::{TokenStream, UnmatchedDelim},
    },
    session::ParseSess,
};

pub mod token;
pub mod tokentree;

pub(crate) fn lex_token_trees<'sesh, 'src>(
    psesh: &'sesh ParseSess,
    source: &'src str,
) -> TokenStream {
    let cursor = Cursor::new(source);

    let mut lexer = Lexer {
        psesh,
        source,
        cursor,
        token: Token::dummy(),
        pos: 0,
        tt_diag: TokenTreeDiagInfo::default(),
    };

    let res = lexer.lex_token_trees(false);

    match res {
        Ok(ts) => {
            println!("Result:\n{:#?}", ts);
            ts
        }
        Err(_errs) => {
            todo!()
        }
    }
}

pub struct Lexer<'sesh, 'src> {
    psesh: &'sesh ParseSess,
    source: &'src str,
    cursor: Cursor<'src>,
    token: Token,
    pos: u32,
    tt_diag: TokenTreeDiagInfo,
}

impl<'sesh, 'src> Lexer<'sesh, 'src> {
    fn next_token_from_cursor(&mut self) -> Token {
        loop {
            let raw_tok = self.cursor.next_token();
            let start = self.pos;
            self.pos = self.pos + raw_tok.len;

            let kind = match raw_tok.kind {
                RawTokenKind::Ident => self.ident_or_kw(start),

                RawTokenKind::Literal { kind } => {
                    let end = start + raw_tok.len;
                    let (kind, symbol) = self.literal(start, end, kind);
                    TokenKind::Literal(LitInner { kind, symbol })
                }

                RawTokenKind::LParen => TokenKind::LParen,
                RawTokenKind::RParen => TokenKind::RParen,
                RawTokenKind::LCurly => TokenKind::LCurly,
                RawTokenKind::RCurly => TokenKind::RCurly,
                RawTokenKind::LBracket => TokenKind::LBracket,
                RawTokenKind::RBracket => TokenKind::RBracket,

                RawTokenKind::Plus => TokenKind::Plus,
                RawTokenKind::Minus => TokenKind::Minus,
                RawTokenKind::Star => TokenKind::Star,
                RawTokenKind::Slash => TokenKind::Slash,
                RawTokenKind::Percent => TokenKind::Percent,
                RawTokenKind::Caret => TokenKind::Caret,
                RawTokenKind::Eq => TokenKind::Eq,
                RawTokenKind::Bang => TokenKind::Bang,
                RawTokenKind::Lt => TokenKind::Lt,
                RawTokenKind::Gt => TokenKind::Gt,
                RawTokenKind::And => TokenKind::And,
                RawTokenKind::Or => TokenKind::Or,
                RawTokenKind::Tilde => TokenKind::Tilde,
                RawTokenKind::Pound => TokenKind::Pound,
                RawTokenKind::Semi => TokenKind::Semi,
                RawTokenKind::Colon => TokenKind::Colon,
                RawTokenKind::Comma => TokenKind::Comma,
                RawTokenKind::Dot => TokenKind::Dot,
                RawTokenKind::Question => TokenKind::Question,

                // TODO might just skip these
                RawTokenKind::LineComment { .. } => todo!(),
                RawTokenKind::BlockComment { .. } => todo!(),

                RawTokenKind::Whitespace => {
                    continue;
                }

                RawTokenKind::Eof => TokenKind::Eof,

                // TODO emit errs here
                RawTokenKind::Unknown => {
                    todo!()
                }
            };

            // let span = Span::new(start, self.pos);
            let span = self.make_span(start, self.pos);
            return Token::new(kind, span);
        }
    }

    fn ident_or_kw(&self, start: u32) -> TokenKind {
        let label = self.str_from(start).to_string();
        match label.as_str() {
            "Bool" => TokenKind::Bool,
            "U0" => TokenKind::U0,
            "U8" => TokenKind::U8,
            "I8" => TokenKind::I8,
            "U16" => TokenKind::U16,
            "I16" => TokenKind::I16,
            "U32" => TokenKind::U32,
            "I32" => TokenKind::I32,
            "U64" => TokenKind::U64,
            "I64" => TokenKind::I64,
            "F64" => TokenKind::F64,

            "class" => TokenKind::Class,
            "if" => TokenKind::If,
            "else" => TokenKind::Else,
            "elif" => TokenKind::Elif,
            "for" => TokenKind::For,
            "while" => TokenKind::While,
            "break" => TokenKind::Break,
            "continue" => TokenKind::Continue,
            "return" => TokenKind::Return,
            "include" => TokenKind::Include,
            "define" => TokenKind::Define,

            _ => TokenKind::Ident(label),
        }
    }

    fn literal(&self, start: u32, end: u32, kind: LiteralKind) -> (LitKind, EcoString) {
        // TODO error handling everywhere
        let (kind, sym) = match kind {
            LiteralKind::Int { base: _ } => {
                let kind = LitKind::Integer;
                let sym = self.str_from_to(start, end);
                (kind, sym)
            }
            LiteralKind::Float => {
                let kind = LitKind::Float;
                let sym = self.str_from_to(start, end);
                (kind, sym)
            }
            LiteralKind::Char { terminated } => {
                if !terminated {
                    // err handle
                }
                let sym = self.str_from_to(start, end);
                (LitKind::Char, sym)
            }
            LiteralKind::Str { terminated } => {
                if !terminated {
                    // err handle
                }
                let sym = self.str_from_to(start, end);
                (LitKind::Str, sym)
            }
        };

        (kind, EcoString::from(sym))
    }

    fn str_from(&self, start: u32) -> &'src str {
        self.str_from_to(start, self.pos)
    }

    fn str_from_to(&self, start: u32, end: u32) -> &'src str {
        &self.source[start as usize..end as usize]
    }

    fn make_span(&self, lo: u32, hi: u32) -> Span {
        Span::new_from_file(lo, hi, self.psesh.curr)
    }
}

#[derive(Default)]
pub struct TokenTreeDiagInfo {
    // stack of open delims
    pub open_delims: Vec<(Delimiter, Span)>,

    // collecting all the unmatched delimiters found during parsing
    // their reporting are defered for now so we can just bail instead
    // of the parser doesn't blow up at the first unmatched delim
    pub unmacthed_delims: Vec<UnmatchedDelim>,

    // this is just for error-ing at EOF
    // not sure if this needs to be tracked in state
    // might move this to just a function later
    pub latest_unclosed_span: Option<Span>,
}
