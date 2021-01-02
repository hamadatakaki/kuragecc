use super::error::LexerError;
use super::token::literal::{
    DelimiterKind, OperatorKind, ParenKind, ReservedLiteral, TerminalSymbol,
};
use super::token::{Token, TokenKind};
use super::{Inspector, Location, Position};

macro_rules! is_ope_kind {
    ($vec: expr, $c: expr) => {{
        let literals = $vec.iter().collect::<String>();
        let add = String::from($c);
        OperatorKind::contains(vec![literals, add].concat())
    }};
}

pub struct Lexer {
    code: Vec<char>,
    look: usize,
    position: Position,
    tokens: Vec<Token>,
    errors: Vec<LexerError>,
}

impl Lexer {
    pub fn new(code: &str) -> Self {
        let code = code.chars().collect::<Vec<char>>();
        Self {
            code,
            look: 0,
            position: Position::initialize(),
            tokens: Vec::new(),
            errors: Vec::new(),
        }
    }

    pub fn tokenize(&mut self) -> Result<Vec<Token>, Vec<LexerError>> {
        self.rec_tokenize();
        if self.errors.is_empty() {
            Ok(self.tokens.clone())
        } else {
            Err(self.errors.clone())
        }
    }

    fn rec_tokenize(&mut self) {
        if !self.at_end() {
            let looked = self.look_at().unwrap();
            let start = self.position;
            if looked.is_ascii_digit() {
                // Number
                self.tokenize_integer();
            } else if looked.is_ascii_alphabetic() {
                // Reserved or Identifier
                self.tokenize_symbol();
            } else if DelimiterKind::contains(looked) {
                // Delimiter
                self.tokenize_delimiter();
            } else if ParenKind::contains(looked) {
                // Paren
                self.tokenize_paren();
            } else if OperatorKind::contains(String::from(looked)) {
                // Operator
                self.tokenize_operator();
            } else if looked.is_ascii_whitespace() {
                // whitespace
                self.forward();
            } else {
                if looked == '\n' {
                    // indention
                    self.forward();
                } else {
                    // invalid character
                    self.forward();
                    let end = self.position;
                    let loc = Location::new(start, end);
                    let error = LexerError::invalid_char(looked, loc);
                    self.errors.push(error);
                }
            }
            self.rec_tokenize()
        }
    }

    fn tokenize_integer(&mut self) {
        let start = self.position;
        let mut nums = vec![];
        while !self.at_end() {
            let c = self.look_at().unwrap();
            if c.is_ascii_digit() {
                nums.push(c);
                self.forward();
            } else {
                break;
            }
        }
        let end = self.position;
        let loc = Location::new(start, end);
        let n = nums.iter().collect::<String>().parse::<u32>().unwrap();
        let kind = TokenKind::Integer(n);
        let token = Token::new(kind, loc, nums);
        self.tokens.push(token);
    }

    fn tokenize_symbol(&mut self) {
        let start = self.position;
        let mut words = vec![];
        while !self.at_end() {
            let c = self.look_at().unwrap();
            if c.is_ascii_alphanumeric() {
                words.push(c);
                self.forward();
            } else {
                break;
            }
        }
        let end = self.position;
        let loc = Location::new(start, end);
        let s = words.iter().collect::<String>();
        let kind = if ReservedLiteral::contains(s.clone()) {
            TokenKind::Reserved(ReservedLiteral::from_literal(s))
        } else {
            TokenKind::Identifier(s)
        };
        let token = Token::new(kind, loc, words);
        self.tokens.push(token);
    }

    fn tokenize_delimiter(&mut self) {
        let start = self.position;
        let looked = self.look_and_forward().unwrap();
        let end = self.position;
        let loc = Location::new(start, end);
        let kind = TokenKind::Delimiter(DelimiterKind::from_literal(looked));
        let words = vec![looked];
        let token = Token::new(kind, loc, words);
        self.tokens.push(token);
    }

    fn tokenize_paren(&mut self) {
        let start = self.position;
        let looked = self.look_and_forward().unwrap();
        let end = self.position;
        let loc = Location::new(start, end);
        let kind = TokenKind::Paren(ParenKind::from_literal(looked));
        let words = vec![looked];
        let token = Token::new(kind, loc, words);
        self.tokens.push(token);
    }

    fn tokenize_operator(&mut self) {
        let start = self.position;
        let mut words = vec![];
        while !self.at_end() {
            let c = self.look_at().unwrap();
            if OperatorKind::contains(String::from(c)) {
                if is_ope_kind!(&words, c) {
                    words.push(c);
                    self.forward();
                } else {
                    break;
                }
            } else {
                break;
            }
        }
        let end = self.position;
        let loc = Location::new(start, end);
        let s = words.iter().collect::<String>();
        let kind = TokenKind::Operator(OperatorKind::from_literal(s));
        let token = Token::new(kind, loc, words);
        self.tokens.push(token);
    }
}

impl Inspector for Lexer {
    type Yielded = char;

    fn forward(&mut self) {
        let looked = self.look_at().unwrap();
        if looked == '\n' {
            self.position.indention();
        } else {
            self.position.forward();
        }
        self.look += 1;
    }

    fn at_end(&self) -> bool {
        self.look >= self.code.len()
    }

    fn look_at(&self) -> Option<char> {
        self.code.get(self.look).map(|&c| c)
    }

    fn look_prev(&self) -> Option<char> {
        self.code.get(self.look - 1).map(|&c| c)
    }

    fn look_next(&self) -> Option<char> {
        self.code.get(self.look + 1).map(|&c| c)
    }

    fn look_and_forward(&mut self) -> Option<char> {
        self.forward();
        self.look_prev()
    }

    fn look_head(&self) -> Option<char> {
        self.code.get(0).map(|&c| c)
    }

    fn look_tail(&self) -> Option<char> {
        let tail = self.code.len() - 1;
        self.code.get(tail).map(|&c| c)
    }
}
