use std::path::PathBuf;

use crate::diagnostic::DiagCtx;

#[derive(Default)]
pub struct ParseSess {
    pub dcx: DiagCtx,
    pub source_files: Vec<SourceFile>,
    pub curr: u16,
}

pub struct SourceMap {
    pub files: Vec<SourceFile>,
}

pub struct SourceFile {
    pub name: PathBuf,
    pub src: String,
    pub line_starts: Vec<usize>,
}
