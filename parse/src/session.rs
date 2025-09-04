use std::{fs, io, path::PathBuf};

use crate::diagnostic::DiagCtx;

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

    pub fn mk_ast(&self) {}
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
