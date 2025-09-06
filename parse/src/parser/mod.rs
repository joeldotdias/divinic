use std::mem;

use ast::ast::Module;

use crate::{
    lexer::{
        token::{Token, TokenKind},
        tokentree::{TokenCursor, TokenStream, TokenTree},
    },
    parser::error::ParseErr,
    session::ParseSess,
};

pub mod error;

pub struct Parser<'a> {
    pub psess: &'a ParseSess,
    curr_tok: Token,
    prev_tok: Token,
    cursor: TokenCursor,
}

impl<'a> Parser<'a> {
    pub fn new(psess: &'a ParseSess, stream: TokenStream) -> Self {
        let mut parser = Parser {
            psess,
            curr_tok: Token::dummy(),
            prev_tok: Token::dummy(),
            cursor: TokenCursor::new(stream),
        };

        parser.bump();

        parser
    }

    pub fn parse_module(&mut self) -> Result<Module, ParseErr> {
        let is_void_ty = self.look_ahead(0, |t| t == &TokenKind::U0);
        let is_lparen = self.look_ahead(2, |t| t == &TokenKind::LParen);
        let is_rparen = self.look_ahead(3, |t| t == &TokenKind::RParen);
        println!("is_u0_ty: {}", is_void_ty);
        println!("is_lparen: {}", is_lparen);
        println!("is_rparen: {}", is_rparen);

        self.bump();
        println!("After bumping");
        let is_lparen_after_bump = self.look_ahead(1, |t| t == &TokenKind::LParen);
        println!("is_lparen after bump: {}", is_lparen_after_bump);
        self.bump();
        self.bump();
        self.bump();
        println!("After 3 bumps: {:?}", self.curr_tok);
        let is_i64_ty = self.look_ahead(1, |t| t == &TokenKind::I64);
        println!("is_i64_ty after 3 bumps: {}", is_i64_ty);

        Ok(Module { decls: Vec::new() })
    }

    fn series_of<N>(
        &mut self,
        parse: &impl Fn(&mut Self) -> Result<Option<N>, ParseErr>,
        sep: Option<&TokenKind>,
    ) -> Result<Vec<N>, ParseErr> {
        todo!()
    }

    fn series_of_has_trailing_separator<N>(
        &mut self,
        parser: &impl Fn(&mut Self) -> Result<Option<N>, ParseErr>,
        sep: Option<&Token>,
    ) -> Result<(Vec<N>, bool), ParseErr> {
        // let mut results = vec![];
        // let sep_at_end = None;

        todo!()
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

    pub fn bump(&mut self) {
        let next = self.cursor.next();
        self.prev_tok = mem::replace(&mut self.curr_tok, next);
    }
}
