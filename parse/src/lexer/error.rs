use std::ops::Range;

use ast::span::Span;

use crate::lexer::token::Delimiter;

#[derive(Debug, PartialEq, Clone)]
pub struct LexicalErr {
    pub err: LexicalErrKind,
    pub loc: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub enum LexicalErrKind {
    UnrecognizedToken(char),
    UnexpectedDelimClose(Delimiter),
    MismatchedDelim {
        expected: Delimiter,
        found: Delimiter,
        open_span: Span,
    },
    UnexpectedEof,
}

impl LexicalErr {
    pub fn report(&self, filename: &str, source: &str) {
        use ariadne::{Color, Label, Report, ReportKind, Source};

        let r_span = self.loc.to_range();
        let (err_msg, labels, sugg) = self.err.err_meta(r_span.clone());

        let mut r = Report::build(ReportKind::Error, (filename, r_span)).with_message(err_msg);

        for (lspan, ltext) in labels {
            r = r.with_label(
                Label::new((filename, lspan))
                    .with_message(ltext)
                    .with_color(Color::Red),
            );
        }

        if let Some(s) = sugg {
            r = r.with_note(s);
        }

        r.finish().print((filename, Source::from(source))).unwrap();
    }
}

impl LexicalErrKind {
    // returns (err_msg, labels, suggestion)
    pub fn err_meta(
        &self,
        span: Range<usize>,
    ) -> (&'static str, Vec<(Range<usize>, String)>, Option<String>) {
        match self {
            LexicalErrKind::UnrecognizedToken(tok) => (
                "Unrecognized token found",
                vec![(span, format!("Unrecognized token: `{tok}`"))],
                None,
            ),
            LexicalErrKind::UnexpectedEof => todo!(),
            LexicalErrKind::UnexpectedDelimClose(delim) => {
                let delim_str = delim.to_close_str();
                (
                    "Unexpected closing delimiter",
                    vec![(
                        span,
                        format!("Unexpected `{delim_str}` - no matching opening delimiter"),
                    )],
                    Some("Check if you have a missing opening delimiter or an extra closing delimiter".to_string()),
                )
            }
            LexicalErrKind::MismatchedDelim {
                expected,
                found,
                open_span,
            } => {
                let expected_str = expected.to_open_str();
                let found_str = found.to_close_str();
                let expected_close = expected.to_close_str();
                let open_range = open_span.to_range();

                (
                    "Mismatched Delimeter",
                    vec![
                        (open_range, format!("Opening `{expected_str}` here")),
                        (
                            span,
                            format!("Expected `{expected_close}`, found `{found_str}`"),
                        ),
                    ],
                    Some(format!(
                        "This `{found_str}` doesn't match the opening `{expected_str}`"
                    )),
                )
            }
        }
    }
}

pub fn mk_lexical_err(kind: LexicalErrKind, span: Span) -> LexicalErr {
    LexicalErr {
        err: kind,
        loc: span,
    }
}
