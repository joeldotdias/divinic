#[derive(Debug, PartialEq, Eq)]
pub struct RawToken {
    pub kind: RawTokenKind,
    pub len: u32,
}

impl RawToken {
    pub fn new(kind: RawTokenKind, len: u32) -> RawToken {
        RawToken { kind, len }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum RawTokenKind {
    Ident,

    Literal { kind: LiteralKind },

    /* delimeters */
    LParen,
    RParen,
    LCurly,
    RCurly,
    LBracket,
    RBracket,

    /* operators */
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Caret,

    Eq,
    Bang,
    Lt,
    Gt,
    And,
    Or,
    Tilde,
    Pound,

    /* punctuation */
    Semi,
    Colon,
    Comma,
    Dot,
    Question,

    LineComment { is_doc: bool },
    BlockComment { terminated: bool, is_doc: bool },

    Whitespace,
    Eof,

    Unknown,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum LiteralKind {
    Int { base: Base },
    Float,
    Char { terminated: bool },
    Str { terminated: bool },
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Base {
    Binary = 2,
    Octal = 8,
    Decimal = 10,
    Hexadecimal = 16,
}
