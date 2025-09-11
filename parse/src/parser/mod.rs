use std::{fs, mem};

use ast::ast::{InbuiltType, Module, Type};
use ecow::EcoString;

use crate::{
    lexer::{
        token::{Token, TokenKind, is_reserved_keyword},
        tokentree::{TokenCursor, TokenStream, TokenTree},
    },
    parser::error::{ParseErr, ParseErrKind, mk_parse_err},
    session::ParseSess,
};

pub mod decls;
pub mod error;
pub mod expr;
pub mod stmt;

pub type ParseResult<T> = Result<T, ParseErr>;

pub struct Parser<'a> {
    pub psess: &'a ParseSess,
    curr_tok: Token,
    prev_tok: Token,
    cursor: TokenCursor,
    pub errs: Vec<ParseErr>,
}

// this section is just meant to be for the parser building blocks
// TODO remove ast node specific stuff from this
impl<'a> Parser<'a> {
    pub fn new(psess: &'a ParseSess, stream: TokenStream) -> Self {
        let mut parser = Parser {
            psess,
            curr_tok: Token::dummy(),
            prev_tok: Token::dummy(),
            cursor: TokenCursor::new(stream),
            errs: Vec::new(),
        };

        parser.bump();

        parser
    }

    pub fn parse_module(&mut self) -> Result<Module, ParseErr> {
        // let is_void_ty = self.look_ahead(0, |t| t == &TokenKind::U0);
        // let is_lparen = self.look_ahead(2, |t| t == &TokenKind::LParen);
        // let is_rparen = self.look_ahead(3, |t| t == &TokenKind::RParen);
        // println!("is_u0_ty: {}", is_void_ty);
        // println!("is_lparen: {}", is_lparen);
        // println!("is_rparen: {}", is_rparen);
        //
        // self.bump();
        // println!("After bumping");
        // let is_lparen_after_bump = self.look_ahead(1, |t| t == &TokenKind::LParen);
        // println!("is_lparen after bump: {}", is_lparen_after_bump);
        // self.bump();
        // self.bump();
        // self.bump();
        // println!("After 3 bumps: {:?}", self.curr_tok);
        // let is_i64_ty = self.look_ahead(1, |t| t == &TokenKind::I64);
        // println!("is_i64_ty after 3 bumps: {}", is_i64_ty);

        // let mut decls = Vec::new();
        // let decl = self.parse_top_level()?;
        // decls.push(decl);

        let decls = Parser::series_of(self, &Parser::parse_top_level, None)?;
        // let dbg_str = format!("{:#?}", decls);
        // fs::write("ast_dump.txt", dbg_str).unwrap();

        Ok(Module { decls })
    }

    pub fn parse_ty(&mut self) -> Result<Type, ParseErr> {
        use TokenKind::*;
        let ty = match &self.curr_tok.kind {
            Ident(label) => Type::Named(label.clone()),
            U0 => Type::Inbuilt(InbuiltType::U0),
            U8 => Type::Inbuilt(InbuiltType::U8),
            U16 => Type::Inbuilt(InbuiltType::U16),
            U32 => Type::Inbuilt(InbuiltType::U32),
            U64 => Type::Inbuilt(InbuiltType::U64),
            I8 => Type::Inbuilt(InbuiltType::I8),
            I16 => Type::Inbuilt(InbuiltType::I16),
            I32 => Type::Inbuilt(InbuiltType::I32),
            I64 => Type::Inbuilt(InbuiltType::I64),
            F64 => Type::Inbuilt(InbuiltType::F64),
            Bool => Type::Inbuilt(InbuiltType::Bool),
            _ => todo!(),
        };
        self.bump();

        Ok(ty)
    }

    fn series_of<N>(
        &mut self,
        subparser: &impl Fn(&mut Self) -> Result<Option<N>, ParseErr>,
        sep: Option<&TokenKind>,
    ) -> Result<Vec<N>, ParseErr> {
        let mut parsed = Vec::new();

        loop {
            match subparser(self)? {
                Some(node) => parsed.push(node),
                None => break,
            }

            if let Some(sep_kind) = sep {
                if !self.eat_no_expect(sep_kind) {
                    break;
                }
            }
        }

        Ok(parsed)
    }

    fn peek_token(&self) -> TokenKind {
        self.look_ahead(1, |tok| tok.clone())
    }

    pub fn look_ahead<T>(&self, dist: usize, looker: impl FnOnce(&TokenKind) -> T) -> T {
        // idk why distance would ever be zero in a look ahead
        // but just in case, we compare the current token
        if dist == 0 {
            return looker(&self.curr_tok.kind);
        }

        // this much work is just because dist = 1 is the most common case
        if dist == 1 {
            match self.cursor.curr.curr() {
                Some(tree) => match tree {
                    TokenTree::Token(token) => return looker(&token.kind),
                    &TokenTree::Delimited(delim, _, _) => {
                        return looker(&delim.as_open_token_kind());
                    }
                },
                None => {
                    if let Some(last) = self.cursor.stack.last()
                        && let Some(&TokenTree::Delimited(delim, _, _)) = last.curr()
                    {
                        return looker(&delim.as_close_token_kind());
                    }
                }
            }
        }

        // dist > 1 is extremely unlikely so this isn't the most optimal
        // this will just clone the cursor and keep next-ing
        let mut cursor = self.cursor.clone();
        let mut i = 0;
        let mut token_kind = TokenKind::Eof; // dummy
        while i < dist {
            token_kind = cursor.next().kind;
            i += 1;
        }
        looker(&token_kind)
    }

    pub fn look_above_tree<T>(
        &self,
        dist: usize,
        looker: impl FnOnce(&TokenTree) -> T,
    ) -> Option<T> {
        assert_ne!(dist, 0);
        self.cursor.curr.look_ahead(dist - 1).map(looker)
    }

    #[inline]
    fn eat_no_expect(&mut self, tok: &TokenKind) -> bool {
        let is_present = self.check_no_expect(tok);
        if is_present {
            self.bump();
        }
        is_present
    }

    #[inline]
    fn check_no_expect(&self, tok: &TokenKind) -> bool {
        // having a one liner be a separate func and then inline
        // is rather weird but this might need to be used separately
        self.curr_tok == *tok
    }

    fn parse_ident(&mut self) -> Result<EcoString, ParseErr> {
        self.parse_ident_common(true)
    }

    fn parse_ident_no_recover(&mut self) -> Result<EcoString, ParseErr> {
        self.parse_ident_common(false)
    }

    fn parse_ident_common(&mut self, recover: bool) -> Result<EcoString, ParseErr> {
        let ident = self.ident_or_err(recover)?;

        if is_reserved_keyword(&ident) {
            let err = mk_parse_err(
                ParseErrKind::ReservedKeywordAsIdent { kw: ident.clone() },
                self.curr_tok.span,
            );

            println!("Problem {ident}");
            if recover {
                self.errs.push(err);
            } else {
                // just fail if we shouldn't recover
                return Err(err);
            }
        }

        self.bump();
        Ok(ident)
    }

    fn ident_or_err(&mut self, recover: bool) -> Result<EcoString, ParseErr> {
        match &self.curr_tok.kind {
            TokenKind::Ident(label) => Ok(label.clone()),
            _ => self.expected_ident_found(recover),
        }
    }

    fn expected_ident_found(&mut self, recover: bool) -> Result<EcoString, ParseErr> {
        let err = mk_parse_err(
            ParseErrKind::ExpectedIdent {
                found: self.curr_tok.kind.clone(),
                context: "identifier", // TODO improve this non sense
            },
            self.curr_tok.span,
        );

        if recover {
            let recovered_name = match &self.curr_tok.kind {
                TokenKind::If => "if",
                TokenKind::While => "while",
                TokenKind::For => "for",
                TokenKind::Return => "return",
                TokenKind::Break => "break",
                TokenKind::Continue => "continue",

                _ => "stupidly_named_func",
            };
            Ok(recovered_name.into())
        } else {
            Err(err)
        }
    }

    // fn expected_ident_found_err(&self) -> ParseErr {
    //     // ParseErrKind::
    // }

    pub fn bump(&mut self) {
        let next = self.cursor.next();
        self.prev_tok = mem::replace(&mut self.curr_tok, next);
    }
}
