use std::mem;

use ast::span::{DUMMY_SPAN, DelimSpan, Span};

use crate::lexer::{
    Lexer,
    error::{LexicalErr, LexicalErrKind, mk_lexical_err},
    token::{Delimiter, Token, TokenKind},
};

#[derive(Debug, Clone)]
pub struct TokenStream(Vec<TokenTree>);

impl TokenStream {
    pub fn new(tts: Vec<TokenTree>) -> TokenStream {
        TokenStream(tts)
    }

    pub fn get(&self, index: usize) -> Option<&TokenTree> {
        self.0.get(index)
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }
}

#[derive(Debug, Clone)]
pub enum TokenTree {
    Token(Token),
    Delimited(Delimiter, DelimSpan, TokenStream),
}

impl<'sesh, 'src> Lexer<'sesh, 'src> {
    pub fn lex_token_trees(&mut self, is_delimited: bool) -> Result<TokenStream, Vec<LexicalErr>> {
        self.bump_past_delim();

        let mut tt_buf = Vec::new();

        loop {
            if let Some(delim) = self.token.kind.open_delim() {
                tt_buf.push(match self.lex_token_tree_opened(delim) {
                    Ok(tt) => tt,
                    Err(errs) => return Err(errs),
                });
            } else if let Some(delim) = self.token.kind.close_delim() {
                return if is_delimited {
                    Ok(TokenStream::new(tt_buf))
                } else {
                    Err(vec![self.closed_delim_error(delim)])
                };
            } else if self.token.kind == TokenKind::Eof {
                return if is_delimited {
                    Err(vec![self.eof_error()])
                } else {
                    Ok(TokenStream::new(tt_buf))
                };
            } else {
                let next_tok = self.bump();
                tt_buf.push(TokenTree::Token(next_tok));
            }
        }
    }

    fn lex_token_tree_opened(
        &mut self,
        open_delim: Delimiter,
    ) -> Result<TokenTree, Vec<LexicalErr>> {
        // span for opening of the delimited section
        let pre_span = self.token.span;

        self.open_delims.push((open_delim, self.token.span));

        let tts = self.lex_token_trees(true)?;
        let delim_span = DelimSpan::from_pair(pre_span, self.token.span);

        if let Some(close_delim) = self.token.kind.close_delim() {
            if close_delim == open_delim {
                // correct closing delim found so we can pop the open delim
                self.open_delims.pop().unwrap();
                // move past closing delim
                self.bump_past_delim();
            } else {
                // bad delim
                let (_, open_span) = self.open_delims.pop().unwrap();
                let close_span = self.token.span;

                // just record this for later instead of emitting an err rn
                self.unmatched_delims.push(UnmatchedDelim {
                    cause: Some(close_delim),
                    cause_span: self.token.span,
                    unclosed_span: Some(open_span),
                });

                let should_consume = !self.open_delims.iter().any(|(d, _)| *d == close_delim);
                if should_consume {
                    self.bump_past_delim();
                }

                return Err(vec![mk_lexical_err(
                    LexicalErrKind::MismatchedDelim {
                        expected: open_delim,
                        found: close_delim,
                        open_span,
                    },
                    // self.token.span,
                    close_span,
                )]);
            }
        } else {
            assert_eq!(self.token.kind, TokenKind::Eof);
        }

        Ok(TokenTree::Delimited(open_delim, delim_span, tts))
    }

    fn bump(&mut self) -> Token {
        let next_tok = loop {
            let next_tok = self.next_token_from_cursor();

            if let Some(glued) = self.token.glue(&next_tok) {
                self.token = glued;
            } else {
                break next_tok;
            }
        };

        let this_tok = std::mem::replace(&mut self.token, next_tok);
        this_tok
    }

    fn eof_error(&mut self) -> LexicalErr {
        mk_lexical_err(LexicalErrKind::UnexpectedEof, self.token.span)
    }

    fn closed_delim_error(&mut self, delim: Delimiter) -> LexicalErr {
        mk_lexical_err(LexicalErrKind::UnexpectedDelimClose(delim), self.token.span)
    }

    fn bump_past_delim(&mut self) {
        let next_tok = self.next_token_from_cursor();
        self.token = next_tok;
    }
}

#[derive(Clone, Debug)]
pub struct UnmatchedDelim {
    pub cause: Option<Delimiter>,
    pub cause_span: Span,
    pub unclosed_span: Option<Span>,
}

#[derive(Debug, Clone)]
pub struct TokenCursor {
    // current (innermost) token stream
    pub curr: TokenTreeCursor,
    // parent token streams we need to return to
    pub stack: Vec<TokenTreeCursor>,
}

impl TokenCursor {
    pub fn new(stream: TokenStream) -> Self {
        TokenCursor {
            curr: TokenTreeCursor::new(stream),
            stack: Vec::new(),
        }
    }

    pub fn next(&mut self) -> Token {
        loop {
            if let Some(tree) = self.curr.curr() {
                match tree {
                    TokenTree::Token(token) => {
                        let ntok = token.clone();
                        self.curr.bump();
                        return ntok;
                    }
                    &TokenTree::Delimited(delim, delim_span, ref tts) => {
                        let trees = TokenTreeCursor::new(tts.clone());
                        self.stack.push(mem::replace(&mut self.curr, trees));
                        return self.mk_open_delim_tok(delim, &delim_span);
                    }
                }
            } else if let Some(parent) = self.stack.pop() {
                let Some(TokenTree::Delimited(delim, delim_span, _)) = parent.curr() else {
                    panic!("parent should be delimited");
                };
                let delim = *delim;
                let span = *delim_span;
                self.curr = parent;
                self.curr.bump(); // move past delimited section

                return self.mk_close_delim_tok(delim, &span);
            } else {
                // everything's probably over at this point
                return self.mk_eof_tok();
            }
        }
    }

    pub fn peek(&self) -> Token {
        let mut cursor = self.clone();
        cursor.next()
    }

    pub fn look_ahead(&self, n: usize) -> Token {
        let mut cursor = self.clone();
        for _ in 0..=n {
            cursor.next();
        }
        cursor.next()
    }

    fn mk_open_delim_tok(&self, delim: Delimiter, span: &DelimSpan) -> Token {
        Token {
            kind: delim.as_open_token_kind(),
            span: span.open,
        }
    }

    fn mk_close_delim_tok(&self, delim: Delimiter, span: &DelimSpan) -> Token {
        Token {
            kind: delim.as_close_token_kind(),
            span: span.close,
        }
    }

    fn mk_eof_tok(&self) -> Token {
        Token::new(TokenKind::Eof, DUMMY_SPAN)
    }
}

#[derive(Debug, Clone)]
pub struct TokenTreeCursor {
    stream: TokenStream,
    index: usize,
}

impl TokenTreeCursor {
    pub fn new(stream: TokenStream) -> Self {
        TokenTreeCursor { stream, index: 0 }
    }

    pub fn curr(&self) -> Option<&TokenTree> {
        self.stream.get(self.index)
    }

    pub fn look_ahead(&self, n: usize) -> Option<&TokenTree> {
        self.stream.get(self.index + n)
    }

    pub fn bump(&mut self) {
        self.index += 1;
    }

    pub fn reached_end(&self) -> bool {
        self.index >= self.stream.len()
    }
}
