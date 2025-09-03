pub mod diagnostic;
pub mod lexer;
pub mod parser;
pub mod session;

#[cfg(test)]
mod tests {
    use crate::{
        lexer::{lex_token_trees, token::TokenKind, tokentree::TokenCursor},
        session::ParseSess,
    };

    #[test]
    fn it_works() {
        let src = r#"U0 main() { U32 a = 7++; }"#;
        let psess = ParseSess::default();
        let stream = lex_token_trees(&psess, src);
        let mut cursor = TokenCursor::new(stream);

        loop {
            let token = cursor.next();
            if matches!(token.kind, TokenKind::Eof) {
                break;
            }
            println!("{:?}", token);
        }
        assert_eq!(1, 1);
    }
}
