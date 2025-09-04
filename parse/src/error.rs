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
    UnexpectedEof,
}

impl LexicalErr {
    pub fn report(&self, filename: &str, source: &str) {
        use ariadne::{Color, Label, Report, ReportKind, Source};

        let ar_lo = self.loc.lo as usize;
        let ar_hi = ar_lo + self.loc.len as usize;
        let ar_span = ar_lo..ar_hi;

        let (err_msg, labels, sugg) = self.err.metatext(ar_span.clone());

        let mut r = Report::build(ReportKind::Error, (filename, ar_span)).with_message(err_msg);

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

        // let r = Report::build(ReportKind::Error, (filename, ar_lo..ar_hi)).with_message(err_msg);
        // for l in labels {
        //     r.with_label(Label::new(span))
        // }

        // let r = Report::build(ReportKind::Error, (filename, ar_lo..ar_hi))
        //     .with_message(format!("Incompatible types"))
        //     .with_label(
        //         Label::new(("sample.tao", 32..33))
        //             .with_message(format!("This is of type {}", "Nat".fg(a)))
        //             .with_color(a),
        //     )
        //     .with_label(
        //         Label::new(("sample.tao", 42..45))
        //             .with_message(format!("This is of type {}", "Str".fg(b)))
        //             .with_color(b),
        //     )
        //     .with_label(
        //         Label::new(("sample.tao", 11..48))
        //             .with_message(format!(
        //                 "The values are outputs of this {} expression",
        //                 "match".fg(out),
        //             ))
        //             .with_color(out),
        //     )
        //     .with_note(format!(
        //         "Outputs of {} expressions must coerce to the same type",
        //         "match".fg(out)
        //     ))
        //     .finish();
        // .print(("sample.tao", Source::from(include_str!("sample.tao"))))
        // .unwrap();

        // match self.err {
        //     LexicalErrKind::UnrecognizedToken(tok) => {}
        //     LexicalErrKind::UnexpectedDelimClose(delimiter) => todo!(),
        //     LexicalErrKind::UnexpectedEof => todo!(),
        // }
    }
}

impl LexicalErrKind {
    // returns (err_msg, labels, suggestion)
    pub fn metatext(
        &self,
        span: Range<usize>,
    ) -> (&'static str, Vec<(Range<usize>, String)>, Option<String>) {
        match self {
            LexicalErrKind::UnrecognizedToken(tok) => (
                "Unrecognized token found",
                vec![(span, format!("Unrecognized token: `{tok}`"))],
                None,
            ),
            LexicalErrKind::UnexpectedDelimClose(delimiter) => todo!(),
            LexicalErrKind::UnexpectedEof => todo!(),
        }
    }
}

pub fn mk_lexical_err(kind: LexicalErrKind, span: Span) -> LexicalErr {
    LexicalErr {
        err: kind,
        loc: span,
    }
}
