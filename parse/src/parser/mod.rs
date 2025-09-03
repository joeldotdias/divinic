use std::mem;

use crate::{
    lexer::{
        token::Token,
        tokentree::{TokenCursor, TokenStream},
    },
    session::ParseSess,
};

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

    pub fn bump(&mut self) {
        let next = self.cursor.next();
        self.prev_tok = mem::replace(&mut self.curr_tok, next);
    }
}
