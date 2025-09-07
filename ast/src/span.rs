use std::ops::Range;

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Span {
    pub lo: u32,
    pub len: u16,
    pub fid: u16,
}

pub const DUMMY_SPAN: Span = Span {
    lo: 0,
    len: 0,
    fid: 0,
};

impl Span {
    pub fn new(lo: u32, hi: u32, fid: u16) -> Self {
        Span {
            lo,
            len: (hi - lo) as u16,
            fid,
        }
    }

    pub fn extend(self, added: Span) -> Span {
        let new_len = self.len + added.len;
        Span {
            lo: self.lo,
            len: new_len,
            fid: 0,
        }
    }

    pub fn merge(&self, other: Span) -> Span {
        let start = self.lo.min(other.lo);
        let end = self.hi().max(other.hi());
        Span {
            lo: start,
            len: (end - start) as u16,
            fid: self.fid,
        }
    }

    pub fn to_range(&self) -> Range<usize> {
        let start = self.lo as usize;
        start..(start + self.len as usize)
    }

    pub fn hi(&self) -> u32 {
        self.lo + self.len as u32
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct DelimSpan {
    pub open: Span,
    pub close: Span,
}

impl DelimSpan {
    pub fn from_pair(open: Span, close: Span) -> Self {
        DelimSpan { open, close }
    }
}
