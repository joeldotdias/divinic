use ast::{
    ast::{BinaryOp, UnaryOp},
    span::{DUMMY_SPAN, Span},
};
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
    Switch,
    Case,
    Default,
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

    pub fn maybe_prefix_op(&self) -> Option<UnaryOp> {
        let prefix = match &self.kind {
            TokenKind::Plus => UnaryOp::Plus,
            TokenKind::PlusPlus => UnaryOp::PreInc,
            TokenKind::Minus => UnaryOp::Minus,
            TokenKind::MinusMinus => UnaryOp::PreDec,
            TokenKind::Bang => UnaryOp::LogNot,
            TokenKind::Tilde => UnaryOp::BitNot,
            TokenKind::Star => UnaryOp::Deref,
            TokenKind::And => UnaryOp::AddrOf,
            _ => return None,
        };
        Some(prefix)
    }

    pub fn maybe_postfix_op(&self) -> Option<UnaryOp> {
        let postfix = match &self.kind {
            TokenKind::PlusPlus => UnaryOp::PostInc,
            TokenKind::MinusMinus => UnaryOp::PostDec,
            _ => return None,
        };
        Some(postfix)
    }

    pub fn maybe_infix_op(&self) -> Option<BinaryOp> {
        let infix = match &self.kind {
            TokenKind::Plus => BinaryOp::Add,
            TokenKind::Minus => BinaryOp::Sub,
            TokenKind::Star => BinaryOp::Mul,
            TokenKind::Slash => BinaryOp::Div,
            TokenKind::Percent => BinaryOp::Mod,
            TokenKind::EqEq => BinaryOp::EqEq,
            TokenKind::NEq => BinaryOp::Ne,
            TokenKind::Lt => BinaryOp::Lt,
            TokenKind::LtEq => BinaryOp::Le,
            TokenKind::Gt => BinaryOp::Gt,
            TokenKind::GtEq => BinaryOp::Ge,
            TokenKind::AndAnd => BinaryOp::And,
            TokenKind::OrOr => BinaryOp::Or,
            TokenKind::And => BinaryOp::BitAnd,
            TokenKind::Or => BinaryOp::BitOr,
            TokenKind::Caret => BinaryOp::BitXor,
            TokenKind::Shl => BinaryOp::Shl,
            TokenKind::Shr => BinaryOp::Shr,
            /* i don't know if assign ops should be here */
            TokenKind::Eq => BinaryOp::Eq,
            TokenKind::PlusEq => BinaryOp::AddEq,
            TokenKind::MinusEq => BinaryOp::SubEq,
            TokenKind::StarEq => BinaryOp::MulEq,
            TokenKind::SlashEq => BinaryOp::DivEq,
            TokenKind::PercentEq => BinaryOp::ModEq,
            TokenKind::AndEq => BinaryOp::BitAndEq,
            TokenKind::OrEq => BinaryOp::BitOrEq,
            TokenKind::CaretEq => BinaryOp::BitXorEq,
            TokenKind::ShlEq => BinaryOp::ShlEq,
            TokenKind::ShrEq => BinaryOp::ShrEq,
            _ => return None,
        };
        Some(infix)
    }

    pub fn is_type_tok(&self) -> bool {
        use TokenKind::*;
        matches!(
            self.kind,
            U0 | U8 | U16 | U32 | U64 | I8 | I16 | I32 | I64 | F64 | Bool | Ident(_)
        )
    }

    pub fn is_block_ender(&self) -> bool {
        matches!(self.kind, TokenKind::RCurly | TokenKind::Eof)
    }

    pub fn is_case_ender(&self) -> bool {
        matches!(
            self.kind,
            TokenKind::Case | TokenKind::Default | TokenKind::RCurly
        )
    }

    pub fn to_str(&self) -> &'_ str {
        self.kind.to_str()
    }
}

impl PartialEq<TokenKind> for Token {
    #[inline]
    fn eq(&self, other: &TokenKind) -> bool {
        self.kind == *other
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

    pub fn is_type_tok(&self) -> bool {
        use TokenKind::*;
        matches!(
            self,
            U0 | U8 | U16 | U32 | U64 | I8 | I16 | I32 | I64 | F64 | Bool
        )
        // this will be impl when sym table is done
        // | Ident(_)
    }

    pub fn to_str(&self) -> &'_ str {
        match &self {
            TokenKind::Ident(s) => s,
            TokenKind::Literal(lit) => &lit.symbol,
            TokenKind::LParen => "(",
            TokenKind::RParen => ")",
            TokenKind::LCurly => "(",
            TokenKind::RCurly => ")",
            TokenKind::LBracket => "[",
            TokenKind::RBracket => "]",
            TokenKind::Plus => "+",
            TokenKind::PlusPlus => "++",
            TokenKind::PlusEq => "+=",
            TokenKind::Minus => "-",
            TokenKind::MinusMinus => "--",
            TokenKind::MinusEq => "-=",
            TokenKind::Star => "*",
            TokenKind::StarEq => "*=",
            TokenKind::Slash => "/",
            TokenKind::SlashEq => "/=",
            TokenKind::Percent => "%",
            TokenKind::PercentEq => "%=",
            TokenKind::Caret => "^",
            TokenKind::CaretEq => "^=",
            TokenKind::Eq => "=",
            TokenKind::EqEq => "==",
            TokenKind::Bang => "!",
            TokenKind::NEq => "!=",
            TokenKind::Lt => "<",
            TokenKind::LtEq => "<=",
            TokenKind::Gt => ">",
            TokenKind::GtEq => ">=",
            TokenKind::And => "&",
            TokenKind::AndEq => "&=",
            TokenKind::AndAnd => "&&",
            TokenKind::Or => "|",
            TokenKind::OrEq => "|=",
            TokenKind::OrOr => "||",
            TokenKind::Tilde => "~",
            TokenKind::Pound => "#",
            TokenKind::Shl => "<<",
            TokenKind::ShlEq => "<<=",
            TokenKind::Shr => ">>",
            TokenKind::ShrEq => ">>=",
            TokenKind::Semi => ";",
            TokenKind::Colon => ":",
            TokenKind::Comma => ",",
            TokenKind::Dot => ".",
            TokenKind::Question => "?",
            TokenKind::Arrow => "->",
            TokenKind::Eof => "EOF",
            TokenKind::Bool => "Bool",
            TokenKind::U0 => "U0",
            TokenKind::U8 => "U8",
            TokenKind::I8 => "I8",
            TokenKind::U16 => "U16",
            TokenKind::I16 => "I16",
            TokenKind::U32 => "U32",
            TokenKind::I32 => "I32",
            TokenKind::U64 => "U64",
            TokenKind::I64 => "I64",
            TokenKind::F64 => "F64",
            TokenKind::If => "if",
            TokenKind::Else => "else",
            TokenKind::Elif => "elif",
            TokenKind::For => "for",
            TokenKind::While => "while",
            TokenKind::Switch => "switch",
            TokenKind::Case => "case",
            TokenKind::Default => "default",
            TokenKind::Break => "break",
            TokenKind::Continue => "continue",
            TokenKind::Return => "return",
            TokenKind::Class => "class",
            TokenKind::Include => "include",
            TokenKind::Define => "define",
            TokenKind::Asm => "asm",
        }
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

    pub fn to_open_str(&self) -> &'static str {
        match self {
            Delimiter::Parenthesis => "(",
            Delimiter::Curly => "{",
            Delimiter::Bracket => "[",
        }
    }

    pub fn to_close_str(&self) -> &'static str {
        match self {
            Delimiter::Parenthesis => ")",
            Delimiter::Curly => "}",
            Delimiter::Bracket => "]",
        }
    }
}

pub fn is_reserved_keyword(s: &str) -> bool {
    matches!(
        s,
        "if" | "else"
            | "while"
            | "for"
            | "do"
            | "switch"
            | "case"
            | "default"
            | "return"
            | "break"
            | "continue"
            | "class"
            | "union"
            | "enum"
            | "U0"
            | "U8"
            | "U16"
            | "U32"
            | "U64"
            | "I8"
            | "I16"
            | "I32"
            | "I64"
            | "F32"
            | "F64"
            | "Bool"
    )
}
