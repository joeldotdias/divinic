use ast::span::Span;
use cursor::{
    Cursor,
    rawtoken::{LiteralKind, RawTokenKind},
};
use ecow::EcoString;

use crate::{
    error::{LexicalErr, LexicalErrKind, mk_lexical_err},
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
) -> Result<TokenStream, Vec<LexicalErr>> {
    let cursor = Cursor::new(source);

    let mut lexer = Lexer {
        psesh,
        source,
        cursor,
        token: Token::dummy(),
        pos: 0,
        open_delims: Vec::new(),
        unmatched_delims: Vec::new(),
        latest_unclosed_span: None,
        errs: Vec::new(),
    };

    let lexed = lexer.lex_token_trees(false);

    match lexed {
        Ok(ts) => {
            if lexer.errs.is_empty() {
                println!("Result:\n{:#?}", ts);
                Ok(ts)
            } else {
                Err(lexer.errs)
            }
        }
        Err(mut lex_errs) => {
            lex_errs.extend(lexer.errs);
            Err(lex_errs)
        }
    }
}

pub struct Lexer<'sesh, 'src> {
    // ongoing parse session
    psesh: &'sesh ParseSess,
    // source text we're tokenizing rn
    source: &'src str,
    // cursor for getting raw tokens
    cursor: Cursor<'src>,

    // current state tracking
    token: Token,
    pos: u32,

    // stack of open delims
    pub open_delims: Vec<(Delimiter, Span)>,

    // collecting all the unmatched delimiters found during parsing
    // their reporting are defered for now so we can just bail instead
    // of the parser doesn't blow up at the first unmatched delim
    pub unmatched_delims: Vec<UnmatchedDelim>,

    // this is just for error-ing at EOF
    // not sure if this needs to be tracked in state
    // might move this to just a function later
    pub latest_unclosed_span: Option<Span>,
    pub errs: Vec<LexicalErr>,
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
                    let culprit = self.str_from(start).chars().next().unwrap();
                    self.errs.push(mk_lexical_err(
                        LexicalErrKind::UnrecognizedToken(culprit),
                        self.make_span(start, self.pos),
                    ));
                    continue;
                }
            };

            // let span = Span::new(start, self.pos);
            let span = self.make_span(start, self.pos);
            return Token::new(kind, span);
        }
    }

    fn ident_or_kw(&self, start: u32) -> TokenKind {
        let label = self.str_from(start);

        match label {
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

            _ => TokenKind::Ident(label.into()),
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
        Span::new(lo, hi, self.psesh.curr)
    }
}
