use std::{fs, path::PathBuf};

use crate::{
    lexer::{lex_token_trees, token::TokenKind, tokentree::TokenCursor},
    parser::Parser,
};

#[derive(Default, Debug)]
pub struct ParseSess {
    pub source_files: Vec<SourceFile>,
    pub curr: u16,
}

impl ParseSess {
    pub fn new(fnames: Vec<String>) -> Self {
        let source_files = fnames.into_iter().map(|f| SourceFile::from(f)).collect();

        ParseSess {
            source_files,
            curr: 0,
        }
    }

    pub fn mk_ast(&mut self) {
        for (i, f) in self.source_files.iter().enumerate() {
            self.curr = i as u16;

            let stream = match lex_token_trees(&self, &f.src) {
                Ok(ts) => ts,
                Err(errs) => {
                    for e in errs {
                        let (filename, source) = self.src_file(e.loc.fid as usize);
                        e.report(filename.to_str().unwrap(), source);
                    }
                    return;
                }
            };

            // let mut cursor = TokenCursor::new(stream);
            // loop {
            //     let token = cursor.next();
            //     if matches!(token.kind, TokenKind::Eof) {
            //         break;
            //     }
            //     println!("{:?}", token);
            // }

            let mut parser = Parser::new(&self, stream);
            parser.parse_module().unwrap();
            for e in parser.errs {
                let (filename, source) = self.src_file(e.loc.fid as usize);
                e.report(filename.to_str().unwrap(), source);
            }
        }
    }

    pub fn src_file(&self, idx: usize) -> (PathBuf, &str) {
        let f = &self.source_files[idx];
        (f.name.clone(), f.src.as_str())
    }
}

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
