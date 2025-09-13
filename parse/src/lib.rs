use std::collections::HashMap;

use ast::ast::{Label, Module, Type};
use ecow::EcoString;

pub mod lexer;
pub mod parser;
pub mod session;
pub mod symtab;

#[derive(Clone, Debug)]
pub struct AnnotatedModule {
    pub name: EcoString,
    pub ast: Module,
    pub resolved_symbols: HashMap<Label, Type>,
}

#[cfg(test)]
mod tests {
    use crate::{
        lexer::{lex_token_trees, token::TokenKind, tokentree::TokenCursor},
        parser::Parser,
        session::ParseSess,
    };

    #[test]
    fn it_works() {
        // let src = include_str!("../../testdata/small.HC");
        // let src = include_str!("../../testdata/shreerang.HC");
        let src = include_str!("../../testdata/switcheroo.HC");
        let psess = ParseSess::default();
        let stream = lex_token_trees(&psess, src).expect("shouldn't have been an error here");
        // let mut cursor = TokenCursor::new(stream);
        //
        // loop {
        //     let token = cursor.next();
        //     if matches!(token.kind, TokenKind::Eof) {
        //         break;
        //     }
        //     println!("{:?}", token);
        // }

        // let stream = match lex_token_trees(&self, &f.src) {
        //     Ok(ts) => ts,
        //     Err(errs) => {
        //         for e in errs {
        //             let (filename, source) = self.src_file(e.loc.fid as usize);
        //             e.report(filename.to_str().unwrap(), source);
        //         }
        //         return;
        //     }
        // };

        let mut parser = Parser::new(&psess, stream);
        let hmod = parser.parse_module().unwrap();
        println!("{:#?}", hmod);
        assert_eq!(1, 1);
    }
}
