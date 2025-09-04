use ast::span::{DUMMY_SPAN, Span};
use ecow::EcoString;

#[derive(Clone, Debug, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq)]
pub enum TokenKind {
    Ident(EcoString),

    Literal(LitInner),

    /* delims */
    LParen,
    RParen,
    LCurly,
    RCurly,
    LBracket,
    RBracket,

    /* operators */
    Plus,
    PlusPlus,
    PlusEq,
    Minus,
    MinusMinus,
    MinusEq,
    Star,
    StarEq,
    Slash,
    SlashEq,
    Percent,
    PercentEq,
    Caret,
    CaretEq,

    Eq,
    EqEq,
    Bang,
    NEq,
    Lt,
    LtEq,
    Gt,
    GtEq,
    And,
    AndEq,
    AndAnd,
    Or,
    OrEq,
    OrOr,
    Tilde,
    Pound,

    Shl,
    ShlEq,
    Shr,
    ShrEq,

    /* punctuation */
    Semi,
    Colon,
    Comma,
    Dot,
    Question,
    Arrow,

    /* keywords */
    Bool,
    U0,
    U8,
    I8,
    U16,
    I16,
    U32,
    I32,
    U64,
    I64,
    F64,

    Class,
    If,
    Else,
    Elif,
    For,
    While,
    Break,
    Continue,
    Return,
    Include,
    Define,
    Asm,

    Eof,
}

#[derive(Clone, Debug, PartialEq)]
pub struct LitInner {
    pub kind: LitKind,
    pub symbol: EcoString,
}

#[derive(Clone, Debug, PartialEq)]
pub enum LitKind {
    Bool,
    Char,
    Integer,
    Float,
    Str,
}

impl Token {
    pub fn new(kind: TokenKind, span: Span) -> Self {
        Token { kind, span }
    }

    pub fn dummy() -> Self {
        Token::new(TokenKind::Semi, DUMMY_SPAN)
    }

    pub fn glue(&self, joint: &Token) -> Option<Token> {
        use TokenKind::*;

        let kind = match (&self.kind, &joint.kind) {
            (Eq, Eq) => EqEq,
            (Eq, _) => return None,

            (Lt, Eq) => LtEq,
            (Lt, Lt) => Shl,
            (Lt, LtEq) => ShlEq,
            (Lt, _) => return None,

            (Gt, Eq) => GtEq,
            (Gt, Gt) => Shr,
            (Gt, GtEq) => ShrEq,
            (Gt, _) => return None,

            (Bang, Eq) => NEq,
            (Bang, _) => return None,

            (Plus, Eq) => PlusEq,
            (Plus, Plus) => PlusPlus,
            (Plus, _) => return None,

            (Minus, Eq) => MinusEq,
            (Minus, Minus) => MinusMinus,
            (Minus, Gt) => Arrow,
            (Minus, _) => return None,

            (Star, Eq) => StarEq,
            (Star, _) => return None,

            (Slash, Eq) => SlashEq,
            (Slash, _) => return None,

            (Percent, Eq) => PercentEq,
            (Percent, _) => return None,

            (Caret, Eq) => CaretEq,
            (Caret, _) => return None,

            (And, Eq) => AndEq,
            (And, And) => AndAnd,
            (And, _) => return None,

            (Or, Eq) => OrEq,
            (Or, Or) => OrOr,
            (Or, _) => return None,

            (Shl, Eq) => ShlEq,
            (Shl, _) => return None,

            (Shr, Eq) => ShrEq,
            (Shr, _) => return None,

            _ => return None,
        };

        Some(Token::new(kind, self.span.extend(joint.span)))
    }
}

impl TokenKind {
    pub fn open_delim(&self) -> Option<Delimiter> {
        match *self {
            TokenKind::LParen => Some(Delimiter::Parenthesis),
            TokenKind::LCurly => Some(Delimiter::Curly),
            TokenKind::LBracket => Some(Delimiter::Bracket),
            _ => None,
        }
    }

    pub fn close_delim(&self) -> Option<Delimiter> {
        match *self {
            TokenKind::RParen => Some(Delimiter::Parenthesis),
            TokenKind::RCurly => Some(Delimiter::Curly),
            TokenKind::RBracket => Some(Delimiter::Bracket),
            _ => None,
        }
    }

    pub fn is_delim(&self) -> bool {
        self.open_delim().is_some() || self.close_delim().is_some()
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Delimiter {
    Parenthesis,
    Curly,
    Bracket,
}

impl Delimiter {
    pub fn as_open_token_kind(&self) -> TokenKind {
        match *self {
            Delimiter::Parenthesis => TokenKind::LParen,
            Delimiter::Curly => TokenKind::LCurly,
            Delimiter::Bracket => TokenKind::LBracket,
        }
    }

    pub fn as_close_token_kind(&self) -> TokenKind {
        match *self {
            Delimiter::Parenthesis => TokenKind::RParen,
            Delimiter::Curly => TokenKind::RCurly,
            Delimiter::Bracket => TokenKind::RBracket,
        }
    }
}

impl Token {
    pub fn to_str(&self) -> &'_ str {
        match &self.kind {
            TokenKind::Ident(s) => s,
            TokenKind::Literal(_) => todo!(),
            TokenKind::LParen => "(",
            TokenKind::RParen => ")",
            TokenKind::LCurly => "(",
            TokenKind::RCurly => ")",
            TokenKind::LBracket => "[",
            TokenKind::RBracket => "]",
            TokenKind::Plus => todo!(),
            TokenKind::PlusPlus => todo!(),
            TokenKind::PlusEq => todo!(),
            TokenKind::Minus => todo!(),
            TokenKind::MinusMinus => todo!(),
            TokenKind::MinusEq => todo!(),
            TokenKind::Star => todo!(),
            TokenKind::StarEq => todo!(),
            TokenKind::Slash => todo!(),
            TokenKind::SlashEq => todo!(),
            TokenKind::Percent => todo!(),
            TokenKind::PercentEq => todo!(),
            TokenKind::Caret => todo!(),
            TokenKind::CaretEq => todo!(),
            TokenKind::Eq => todo!(),
            TokenKind::EqEq => todo!(),
            TokenKind::Bang => todo!(),
            TokenKind::NEq => todo!(),
            TokenKind::Lt => todo!(),
            TokenKind::LtEq => todo!(),
            TokenKind::Gt => todo!(),
            TokenKind::GtEq => todo!(),
            TokenKind::And => todo!(),
            TokenKind::AndEq => todo!(),
            TokenKind::AndAnd => todo!(),
            TokenKind::Or => todo!(),
            TokenKind::OrEq => todo!(),
            TokenKind::OrOr => todo!(),
            TokenKind::Tilde => todo!(),
            TokenKind::Pound => todo!(),
            TokenKind::Shl => todo!(),
            TokenKind::ShlEq => todo!(),
            TokenKind::Shr => todo!(),
            TokenKind::ShrEq => todo!(),
            TokenKind::Semi => todo!(),
            TokenKind::Colon => todo!(),
            TokenKind::Comma => todo!(),
            TokenKind::Dot => todo!(),
            TokenKind::Question => todo!(),
            TokenKind::Arrow => todo!(),
            TokenKind::Eof => todo!(),
            TokenKind::Bool => todo!(),
            TokenKind::U0 => todo!(),
            TokenKind::U8 => todo!(),
            TokenKind::I8 => todo!(),
            TokenKind::U16 => todo!(),
            TokenKind::I16 => todo!(),
            TokenKind::U32 => todo!(),
            TokenKind::I32 => todo!(),
            TokenKind::U64 => todo!(),
            TokenKind::I64 => todo!(),
            TokenKind::F64 => todo!(),
            TokenKind::If => todo!(),
            TokenKind::Else => todo!(),
            TokenKind::Elif => todo!(),
            TokenKind::For => todo!(),
            TokenKind::While => todo!(),
            TokenKind::Break => todo!(),
            TokenKind::Continue => todo!(),
            TokenKind::Return => todo!(),
            TokenKind::Class => todo!(),
            TokenKind::Include => todo!(),
            TokenKind::Define => todo!(),
            TokenKind::Asm => todo!(),
        }
    }
}
