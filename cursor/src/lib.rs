use std::str::Chars;

use crate::rawtoken::{Base, LiteralKind, RawToken, RawTokenKind};

pub mod rawtoken;

pub struct Cursor<'a> {
    chars: Chars<'a>,
    len_remaining: usize,
}

pub fn tokenize(source: &str) -> impl Iterator<Item = RawToken> {
    let mut cursor = Cursor::new(source);
    std::iter::from_fn(move || {
        let tok = cursor.next_token();
        if tok.kind != RawTokenKind::Eof {
            Some(tok)
        } else {
            None
        }
    })
}

const EOF_CHAR: char = '\0';

// Unicode's Pattern_White_Space
// Reference: https://www.unicode.org/L2/L2005/05012r-pattern.html
// 0009..000D    ; Pattern_White_Space # Cc   [5] <control-0009>..<control-000D>
// 0020          ; Pattern_White_Space # Zs       SPACE
// 0085          ; Pattern_White_Space # Cc       <control-0085>
// 200E..200F    ; Pattern_White_Space # Cf   [2] LEFT-TO-RIGHT MARK..RIGHT-TO-LEFT MARK
// 2028          ; Pattern_White_Space # Zl       LINE SEPARATOR
// 2029          ; Pattern_White_Space # Zp       PARAGRAPH SEPARATOR
pub fn is_pattern_whitespace(c: char) -> bool {
    matches!(
        c,
        //<control-0009>..<control-000D>
        '\u{0009}'
        |'\u{000A}'
        |'\u{000B}'
        |'\u{000C}'
        |'\u{000D}'


        // SPACE
        |'\u{0020}'

        //<control-0085>
        |'\u{0085}'

        // LEFT-TO-RIGHT MARK..RIGHT-TO-LEFT MARK
        |'\u{200E}'
        |'\u{200F}'

        | '\u{2028}' // LINE SEPARATOR
        | '\u{2029}' // PARAGRAPH SEPARATOR
    )
}

impl<'a> Cursor<'a> {
    pub fn new(input: &'a str) -> Cursor<'a> {
        Cursor {
            chars: input.chars(),
            len_remaining: input.len(),
        }
    }

    pub fn next_token(&mut self) -> RawToken {
        use RawTokenKind::*;

        let Some(leading_char) = self.bump() else {
            return RawToken::new(Eof, 0);
        };

        let token_kind = match leading_char {
            '(' => LParen,
            ')' => RParen,
            '{' => LCurly,
            '}' => RCurly,
            '[' => LBracket,
            ']' => RBracket,

            '+' => Plus,
            '-' => Minus,
            '*' => Star,
            '%' => Percent,
            '^' => Caret,
            '/' => match self.peek() {
                '/' => self.line_comment(),
                '*' => self.block_comment(),
                _ => Slash,
            },

            '=' => Eq,
            '!' => Bang,
            '<' => Lt,
            '>' => Gt,
            '&' => And,
            '|' => Or,
            '~' => Tilde,
            '#' => Pound,

            ';' => Semi,
            ':' => Colon,
            ',' => Comma,
            '.' => Dot,
            '?' => Question,

            c if is_pattern_whitespace(c) => {
                self.eat_while(is_pattern_whitespace);
                Whitespace
            }

            c if c.is_alphabetic() || c == '_' => {
                self.eat_while(|c| c.is_alphanumeric() || c == '_');
                Ident
            }

            '\'' => {
                let terminated = self.char_literal();
                Literal {
                    kind: LiteralKind::Char { terminated },
                }
            }

            c @ '0'..='9' => {
                let literal_kind = self.num_literal(c);
                Literal { kind: literal_kind }
            }

            '"' => {
                let terminated = self.string_literal();
                Literal {
                    kind: LiteralKind::Str { terminated },
                }
            }

            _ => Unknown,
        };

        let tok = RawToken::new(token_kind, self.consumed_len());
        self.reset_consumed_len();
        tok
    }

    fn string_literal(&mut self) -> bool {
        while let Some(c) = self.bump() {
            match c {
                '"' => {
                    return true;
                }
                // while lexing, these are the only 2 chars that'll a
                // difference in where the string ends
                '\\' if self.peek() == '\\' || self.peek() == '=' => {
                    self.bump();
                }
                _ => (),
            }
        }

        false
    }

    fn num_literal(&mut self, first_digit: char) -> LiteralKind {
        let mut base = Base::Decimal;
        if first_digit == '0' {
            // Attempt to parse encoding base.
            match self.peek() {
                'b' => {
                    base = Base::Binary;
                    self.bump();
                    self.eat_decimal_digits();
                }
                'o' => {
                    base = Base::Octal;
                    self.bump();
                    self.eat_decimal_digits();
                }
                'x' => {
                    base = Base::Hexadecimal;
                    self.bump();
                    self.eat_hex_digits();
                }
                // also not a base prefix; nothing more to do here.
                '.' | 'e' | 'E' => {}
                _ => return LiteralKind::Int { base },
            }
        } else {
            // no base prefix
            self.eat_decimal_digits();
        }

        // Only decimal numbers can be floats in HolyC
        if base == Base::Decimal {
            match self.peek() {
                '.' => {
                    self.bump();
                    if self.peek().is_ascii_digit() {
                        self.eat_decimal_digits();
                        match self.peek() {
                            'e' | 'E' => {
                                self.bump();
                                self.eat_float_exponent();
                            }
                            _ => (),
                        }
                    }
                    LiteralKind::Float
                }
                'e' | 'E' => {
                    self.bump();
                    self.eat_float_exponent();
                    LiteralKind::Float
                }
                _ => LiteralKind::Int { base },
            }
        } else {
            LiteralKind::Int { base }
        }
    }

    fn char_literal(&mut self) -> bool {
        // single character char literal. still need to check for escapes
        if self.peek2() == '\'' && self.peek() != '\\' {
            self.bump();
            self.bump();
            return true;
        }

        loop {
            match self.peek() {
                '\'' => {
                    // empty char
                    self.bump();
                    return true;
                }
                // going to new line without closing '
                // better for error reporting if we break here
                '\n' if self.peek2() != '\'' => break,
                EOF_CHAR if self.reached_eof() => break,

                _ => {
                    self.bump();
                }
            }
        }

        false
    }

    fn eat_decimal_digits(&mut self) {
        loop {
            match self.peek() {
                '0'..='9' => {
                    self.bump();
                }
                _ => break,
            }
        }
    }

    fn eat_hex_digits(&mut self) {
        loop {
            match self.peek() {
                '0'..='9' | 'a'..='f' | 'A'..='F' => {
                    self.bump();
                }
                _ => break,
            }
        }
    }

    fn eat_float_exponent(&mut self) {
        if self.peek() == '-' || self.peek() == '+' {
            self.bump();
        }
        self.eat_decimal_digits();
    }

    fn line_comment(&mut self) -> RawTokenKind {
        self.bump();
        let is_doc = self.peek() == '/' && self.peek2() != '/';

        self.eat_until('\n');
        RawTokenKind::LineComment { is_doc }
    }

    fn block_comment(&mut self) -> RawTokenKind {
        self.bump();

        let is_doc = self.peek() == '*' && !matches!(self.peek2(), '*' | '/');

        let mut depth = 1;
        while let Some(c) = self.bump() {
            match c {
                '/' if self.peek() == '*' => {
                    self.bump();
                    depth += 1;
                }
                '*' if self.peek() == '/' => {
                    self.bump();
                    depth -= 1;
                    if depth == 0 {
                        break;
                    }
                }
                _ => (),
            }
        }

        RawTokenKind::BlockComment {
            is_doc,
            terminated: depth == 0,
        }
    }

    pub fn peek(&self) -> char {
        self.chars.clone().next().unwrap_or(EOF_CHAR)
    }

    pub fn peek2(&self) -> char {
        let mut ichars = self.chars.clone();
        ichars.next();
        ichars.next().unwrap_or(EOF_CHAR)
    }

    fn eat_while(&mut self, mut predicate: impl FnMut(char) -> bool) {
        while predicate(self.peek()) && !self.reached_eof() {
            self.bump();
        }
    }

    fn eat_until(&mut self, c: char) {
        self.chars = match self.as_str().find(c) {
            Some(idx) => self.as_str()[idx..].chars(),
            None => "".chars(),
        }
    }

    fn bump(&mut self) -> Option<char> {
        let c = self.chars.next()?;
        Some(c)
    }

    fn consumed_len(&self) -> u32 {
        (self.len_remaining - self.as_str().len()) as u32
    }

    fn reset_consumed_len(&mut self) {
        self.len_remaining = self.as_str().len();
    }

    fn reached_eof(&self) -> bool {
        self.as_str().is_empty()
    }

    pub fn as_str(&self) -> &'a str {
        self.chars.as_str()
    }
}

#[cfg(test)]
mod tests {
    use super::{LiteralKind::*, RawTokenKind::*, *};

    macro_rules! test_tokens {
    ( $( ($kind:expr, $len:expr) ),* $(,)? ) => {
        vec![
            $(
                RawToken {
                    kind: $kind,
                    len: $len,
                }
            ),*
        ]
    };
}

    fn validate_tokens(src: &str, expected: Vec<RawToken>) {
        let tokens: Vec<_> = tokenize(src).collect();
        assert_eq!(tokens, expected);
    }

    #[test]
    fn tiny() {
        let src = "/* terryyyy */ U0 main() { U32 number = 0x10 + 20e-2; }";
        let expected = test_tokens![
            (
                BlockComment {
                    is_doc: false,
                    terminated: true
                },
                14
            ),
            (Whitespace, 1),
            (Ident, 2),
            (Whitespace, 1),
            (Ident, 4),
            (LParen, 1),
            (RParen, 1),
            (Whitespace, 1),
            (LCurly, 1),
            (Whitespace, 1),
            (Ident, 3),
            (Whitespace, 1),
            (Ident, 6),
            (Whitespace, 1),
            (Eq, 1),
            (Whitespace, 1),
            (
                Literal {
                    kind: Int {
                        base: Base::Hexadecimal
                    }
                },
                4
            ),
            (Whitespace, 1),
            (Plus, 1),
            (Whitespace, 1),
            (Literal { kind: Float }, 5),
            (Semi, 1),
            (Whitespace, 1),
            (RCurly, 1),
        ];
        validate_tokens(src, expected);
    }

    #[test]
    fn numeric() {
        let src = "123 0xFF 0b1010 0o77";
        let expected = test_tokens![
            (
                Literal {
                    kind: Int {
                        base: Base::Decimal
                    }
                },
                3
            ),
            (Whitespace, 1),
            (
                Literal {
                    kind: Int {
                        base: Base::Hexadecimal
                    }
                },
                4
            ),
            (Whitespace, 1),
            (
                Literal {
                    kind: Int { base: Base::Binary }
                },
                6
            ),
            (Whitespace, 1),
            (
                Literal {
                    kind: Int { base: Base::Octal }
                },
                4
            ),
        ];
        validate_tokens(src, expected);
    }

    #[test]
    fn doc_comment() {
        let src = r#"/// This is a doc comment
U32 normal = 7;"#;
        let expected = test_tokens![
            (LineComment { is_doc: true }, 25),
            (Whitespace, 1),
            (Ident, 3),
            (Whitespace, 1),
            (Ident, 6),
            (Whitespace, 1),
            (Eq, 1),
            (Whitespace, 1),
            (
                Literal {
                    kind: Int {
                        base: Base::Decimal
                    }
                },
                1
            ),
            (Semi, 1),
        ];
        validate_tokens(src, expected);
    }

    #[test]
    fn nested_block_comment() {
        let src = "/*Top /* Next level /* Another level  */*/ */ U32 n = 42 * 7;";
        let expected = test_tokens![
            (
                BlockComment {
                    is_doc: false,
                    terminated: true
                },
                45
            ),
            (Whitespace, 1),
            (Ident, 3),
            (Whitespace, 1),
            (Ident, 1),
            (Whitespace, 1),
            (Eq, 1),
            (Whitespace, 1),
            (
                Literal {
                    kind: Int {
                        base: Base::Decimal
                    }
                },
                2
            ),
            (Whitespace, 1),
            (Star, 1),
            (Whitespace, 1),
            (
                Literal {
                    kind: Int {
                        base: Base::Decimal
                    }
                },
                1
            ),
            (Semi, 1),
        ];
        validate_tokens(src, expected);
    }

    #[test]
    fn unterminated_block_comment() {
        let src = "/* Top /* Down */";
        let expected = test_tokens![(
            BlockComment {
                is_doc: false,
                terminated: false
            },
            17
        ),];
        validate_tokens(src, expected);
    }
}
