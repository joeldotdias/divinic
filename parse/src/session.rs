use std::{fs, io, path::PathBuf};

use crate::{
    diagnostic::DiagCtx,
    lexer::{lex_token_trees, token::TokenKind, tokentree::TokenCursor},
};

#[derive(Default, Debug)]
pub struct ParseSess {
    pub dcx: DiagCtx,
    pub source_files: Vec<SourceFile>,
    pub curr: u16,
}

impl ParseSess {
    pub fn new(fnames: Vec<String>) -> Self {
        let source_files = fnames.into_iter().map(|f| SourceFile::from(f)).collect();

        ParseSess {
            dcx: DiagCtx::default(),
            source_files,
            curr: 0,
        }
    }

    pub fn mk_ast(&mut self) {
        for (i, f) in self.source_files.iter().enumerate() {
            self.curr = i as u16;

            let stream = lex_token_trees(&self, &f.src);
            let mut cursor = TokenCursor::new(stream);

            loop {
                let token = cursor.next();
                if matches!(token.kind, TokenKind::Eof) {
                    break;
                }
                println!("{:?}", token);
            }
        }
    }

    pub fn src_file(&self, idx: usize) -> (PathBuf, &str) {
        let f = &self.source_files[idx];
        (f.name.clone(), f.src.as_str())
    }
}

// pub struct SourceMap {
//     pub files: Vec<SourceFile>,
// }

#[derive(Debug)]
pub struct SourceFile {
    pub name: PathBuf,
    pub src: String,
    pub line_starts: Vec<usize>,
}

impl From<String> for SourceFile {
    fn from(value: String) -> Self {
        let path = PathBuf::from(value);
        let src = fs::read_to_string(&path).expect("Couldn't read file");

        SourceFile {
            name: path,
            src,
            line_starts: Vec::new(),
        }
    }
}
