use ast::span::Span;

#[derive(Debug, PartialEq, Clone)]
pub struct LexicalErr {
    pub err: LexicalErrKind,
    pub location: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub enum LexicalErrKind {}
