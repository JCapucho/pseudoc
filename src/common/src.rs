use std::{fmt, ops::Range};

#[derive(Copy, Clone, Hash, PartialEq, Eq)]
pub enum Span {
    None,
    Range(usize, usize),
}

impl Span {
    pub const fn none() -> Self { Span::None }

    pub const fn single(loc: usize) -> Self { Span::Range(loc, loc + 1) }

    #[must_use]
    pub fn union(self, other: Self) -> Self {
        match (self, other) {
            (Span::None, b) => b,
            (a, Span::None) => a,
            (Span::Range(from_a, until_a), Span::Range(from_b, until_b)) => {
                Span::Range(from_a.min(from_b), until_a.max(until_b))
            },
        }
    }

    pub fn as_range(self) -> Option<Range<usize>> {
        match self {
            Span::None => None,
            Span::Range(start, end) => Some(start..end),
        }
    }
}

impl fmt::Debug for Span {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Span::None => write!(f, "none"),
            Span::Range(from, to) => write!(f, "{:?}..{:?}", from, to),
        }
    }
}

impl From<usize> for Span {
    fn from(pos: usize) -> Self { Span::single(pos) }
}

impl From<(usize, usize)> for Span {
    fn from((from, to): (usize, usize)) -> Self { Span::Range(from, to) }
}

impl From<Range<usize>> for Span {
    fn from(range: Range<usize>) -> Self { Span::Range(range.start, range.end) }
}

#[derive(Clone, Copy, Debug)]
pub struct Spanned<T> {
    pub node: T,
    pub span: Span,
}

impl<T: PartialEq> PartialEq for Spanned<T> {
    fn eq(&self, other: &Self) -> bool { self.node == other.node }
}

impl<T: Eq> Eq for Spanned<T> {}

impl<T: fmt::Display> fmt::Display for Spanned<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result { self.node.fmt(f) }
}
