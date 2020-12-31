use super::token::literal::{
    DelimiterKind, OperatorKind, ParenKind, ReservedLiteral, TerminalSymbol,
};
use super::token::{Token, TokenKind};
use super::Inspector;

pub struct Lexer {
    code: Vec<char>,
    pub tokens: Vec<Token>,
    look: usize,
}

impl Lexer {
    pub fn new(code: &str) -> Self {
        let code = code.chars().collect::<Vec<char>>();
        Self {
            code,
            tokens: Vec::new(),
            look: 0,
        }
    }

    pub fn tokenize(&mut self) {
        self.rec_tokenize();
    }

    fn rec_tokenize(&mut self) {
        if !self.at_end() {
            let looked = self.look_at();
            if looked.is_ascii_digit() {
                // Number
                self.tokenize_integer();
            } else if looked.is_ascii_alphabetic() {
                // Reserved
                self.tokenize_reserved();
            } else if DelimiterKind::contains(looked) {
                // Delimiter
                self.tokenize_delimiter();
            } else if ParenKind::contains(looked) {
                // Paren
                self.tokenize_paren();
            } else if OperatorKind::contains(looked) {
                // Operator
                self.tokenize_operator();
            } else {
                self.forward();
            }
            self.rec_tokenize()
        }
    }

    fn tokenize_integer(&mut self) {
        let start = self.look;
        let mut nums = vec![];
        while !self.at_end() {
            let c = self.look_at();
            if c.is_ascii_digit() {
                nums.push(c);
                self.forward();
            } else {
                break;
            }
        }
        let s = nums.iter().collect::<String>();
        let n = s.parse::<u32>().unwrap();
        let kind = TokenKind::Integer(n);
        let token = Token::new(kind, nums, (start, self.look - start));
        self.tokens.push(token);
    }

    fn tokenize_reserved(&mut self) {
        let start = self.look;
        let mut words = vec![];
        while !self.at_end() {
            let c = self.look_at();
            if c.is_ascii_alphanumeric() {
                words.push(c);
                self.forward();
            } else {
                break;
            }
        }
        let s = words.iter().collect::<String>();
        let kind = if ReservedLiteral::contains(s.as_str()) {
            TokenKind::Reserved(ReservedLiteral::from_literal(&s))
        } else {
            unimplemented!()
        };
        let token = Token::new(kind, words, (start, self.look - start));
        self.tokens.push(token);
    }

    fn tokenize_delimiter(&mut self) {
        let start = self.look;
        let looked = self.look_and_forward();
        let kind = TokenKind::Delimiter(DelimiterKind::from_literal(looked));
        let token = Token::new(kind, vec![looked], (start, 1));
        self.tokens.push(token);
    }

    fn tokenize_paren(&mut self) {
        let start = self.look;
        let looked = self.look_and_forward();
        let kind = TokenKind::Paren(ParenKind::from_literal(looked));
        let token = Token::new(kind, vec![looked], (start, 1));
        self.tokens.push(token);
    }

    fn tokenize_operator(&mut self) {
        let start = self.look;
        let looked = self.look_and_forward();
        let kind = TokenKind::Operator(OperatorKind::from_literal(looked));
        let token = Token::new(kind, vec![looked], (start, 1));
        self.tokens.push(token);
    }
}

impl Inspector for Lexer {
    type Yielded = char;

    fn forward(&mut self) {
        self.look += 1;
    }

    fn backward(&mut self) {
        self.look -= 1;
    }

    fn at_end(&self) -> bool {
        self.look >= self.code.len()
    }

    fn look_at(&self) -> char {
        self.code[self.look]
    }

    fn look_prev(&self) -> char {
        self.code[self.look - 1]
    }

    fn look_next(&self) -> char {
        self.code[self.look + 1]
    }

    fn look_and_forward(&mut self) -> char {
        self.forward();
        self.look_prev()
    }
}
