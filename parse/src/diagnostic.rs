use ast::span::Span;

#[derive(Default)]
pub struct DiagCtx {
    diagnostics: Vec<Diagnostic>,
    // the counts are just to show before printing errors
    err_count: u32,
    warning_count: u32,
}

impl DiagCtx {
    pub fn push_err(&mut self, diag: Diagnostic) {
        self.diagnostics.push(diag);
        self.err_count += 1;
    }

    pub fn push_warn(&mut self, diag: Diagnostic) {
        self.diagnostics.push(diag);
        self.warning_count += 1;
    }
}

pub struct Diagnostic {
    pub level: DiagLevel,
    pub message: String,
    pub label: String,
    pub span: Span,
}

impl Diagnostic {
    pub fn err(message: String, label: String, span: Span) -> Diagnostic {
        Diagnostic {
            level: DiagLevel::Error,
            message,
            label,
            span,
        }
    }

    // pub fn warn() -> Diagnostic{}
}

pub enum DiagLevel {
    Error,
    Warning,
    // change this stupid name or remove the variant
    // Help seems like a good option
    Note,
}
