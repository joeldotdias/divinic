use std::ops::Range;

use ast::span::Span;
use ecow::EcoString;

use crate::lexer::token::TokenKind;

#[derive(Clone, Debug, PartialEq)]
pub struct ParseErr {
    pub err: ParseErrKind,
    pub loc: Span,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ParseErrKind {
    ExpectedIdent {
        found: TokenKind,
        context: &'static str,
    },
    ReservedKeywordAsIdent {
        kw: EcoString,
    },
    UnexpectedToken {
        found: TokenKind,
        context: &'static str,
    },
    FailedExpectation {
        expected: TokenKind,
        found: TokenKind,
        context: &'static str,
    },
    BadFunctionCall {
        reason: &'static str,
    },
    InvalidInteger {
        value: EcoString,
        reason: String,
    },
}

impl ParseErr {
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

impl ParseErrKind {
    pub fn err_meta(
        &self,
        span: Range<usize>,
    ) -> (&'static str, Vec<(Range<usize>, String)>, Option<String>) {
        match self {
            ParseErrKind::ReservedKeywordAsIdent { kw } => (
                "Cannot use reserved keyword as identifier",
                vec![(
                    span,
                    format!(
                        "`{}` is a reserved keyword and cannot be used an identifier",
                        kw
                    ),
                )],
                Some(format!("Try using a different name instead of `{}`", kw)),
            ),
            ParseErrKind::ExpectedIdent { found, context } => (
                "Expected idententifier",
                vec![(
                    span,
                    format!(
                        "Expected identifier for `{}`, but got `{}`",
                        context, found.to_str()
                    ),
                )],
                Some("Identifiers must start with a letter or underscore and contain only letters, digits, and underscores".to_string()),
            ),
            ParseErrKind::UnexpectedToken { found, context } => (
                "Encountered unexpected token",
                vec![(
                    span,
                    format!("Unexpected `{}` in {}", found.to_str(), context))
                ],
                None
            ),
            ParseErrKind::FailedExpectation { expected, found, context } => (
                "Failed expectation",
                vec![(
                    span,
                    format!("Expected `{}` after {}, but found {}", expected.to_str(), context, found.to_str()))
                ],
                Some(format!("Try replacing `{}` with `{}`", found.to_str(), expected.to_str()))
            ),
            ParseErrKind::BadFunctionCall { reason } => (
                "Bad functionc call",
                vec![(span, reason.to_string())],
                None,
            ),
            ParseErrKind::InvalidInteger { value, reason } => (
                "Invalid integer literal",
                vec![(
                    span,
                    format!("Cannot parse `{}` as integer: {}", value, reason),
                )],
                Some("Integer literals must be valid decimal numbers".to_string()),
            )



            // _ => todo!()
        }
    }
}

pub fn mk_parse_err(kind: ParseErrKind, span: Span) -> ParseErr {
    ParseErr {
        err: kind,
        loc: span,
    }
}
