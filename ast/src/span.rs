#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Span {
    lo: u32,
    len: u16,
    fid: u16,
}

pub const DUMMY_SPAN: Span = Span {
    lo: 0,
    len: 0,
    fid: 0,
};

impl Span {
    pub fn new_from_file(lo: u32, hi: u32, fid: u16) -> Self {
        Span {
            lo,
            len: (hi - lo) as u16,
            fid: 0,
        }
    }

    pub fn new(lo: u32, hi: u32) -> Self {
        Span {
            lo,
            len: (hi - lo) as u16,
            fid: 0,
        }
    }

    pub fn extend(self, added: Span) -> Span {
        let new_len = self.len + added.len;
        Span {
            lo: self.lo,
            len: new_len,
            fid: 0,
        }
        // Span::new(self.lo, added.hi())
    }

    fn hi(&self) -> u32 {
        // self.lo + self.len as u32 - 1
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
