use super::{src::Span, Symbol};
use crate::lexer::Token;
use codespan_reporting::diagnostic::{Diagnostic, Label};
use lasso::RodeoResolver;
use std::fmt;

#[derive(Debug)]
pub enum ErrorKind {
    Custom(String),
    Eof,
    UnexpectedToken(Token),
    UnknownVariable(Symbol),
    Io(std::io::Error),
}

impl ErrorKind {
    pub fn display<'a>(&'a self, rodeo: &'a RodeoResolver) -> impl fmt::Display + 'a {
        struct ErrorDisplay<'a> {
            kind: &'a ErrorKind,
            rodeo: &'a RodeoResolver,
        }

        impl<'a> fmt::Display for ErrorDisplay<'a> {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                match self.kind {
                    ErrorKind::Custom(msg) => write!(f, "{}", msg),
                    ErrorKind::Eof => write!(f, "Unexpected end of file"),
                    ErrorKind::UnexpectedToken(token) => {
                        write!(f, "Unexpected token: {}", token.display(self.rodeo))
                    },
                    ErrorKind::UnknownVariable(symbol) => {
                        write!(f, "Unknown variable {}", self.rodeo.resolve(symbol))
                    },
                    ErrorKind::Io(err) => {
                        write!(f, "IO error: {}", err)
                    },
                }
            }
        }

        ErrorDisplay { kind: self, rodeo }
    }
}

#[derive(Debug)]
pub struct Error {
    kind: ErrorKind,
    spans: Vec<Span>,
    hints: Vec<String>,
}

impl Error {
    pub fn new(kind: ErrorKind) -> Self {
        Self {
            kind,
            spans: Vec::new(),
            hints: Vec::new(),
        }
    }

    pub fn custom(msg: String) -> Self { Self::new(ErrorKind::Custom(msg)) }

    pub fn eof(span: Span) -> Self { Self::new(ErrorKind::Eof).with_span(span) }

    pub fn unexpected_token(token: Token, span: Span) -> Self {
        Self::new(ErrorKind::UnexpectedToken(token)).with_span(span)
    }

    pub fn with_span(mut self, span: Span) -> Self {
        self.spans.push(span);
        self
    }

    pub fn with_hint(mut self, hint: String) -> Self {
        self.hints.push(hint);
        self
    }

    pub fn codespan_diagnostic<FileId: Copy>(
        self,
        file_id: FileId,
        rodeo: &RodeoResolver,
    ) -> Diagnostic<FileId> {
        Diagnostic::error()
            .with_message(self.kind.display(rodeo).to_string())
            .with_labels(
                self.spans
                    .into_iter()
                    .filter_map(|span| span.as_range())
                    .map(|span| Label::primary(file_id, span))
                    .collect(),
            )
            .with_notes(self.hints)
    }
}

impl From<std::io::Error> for Error {
    fn from(err: std::io::Error) -> Self { Error::new(ErrorKind::Io(err)) }
}
