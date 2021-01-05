use super::ast::{ASTKind, AST};
use super::error::{ParserError, ParserErrorKind, ParserResult};
use super::token::literal::{OperatorKind, TerminalSymbol};
use super::token::{Token, TokenKind};
use super::Inspector;

pub struct Parser {
    tokens: Vec<Token>,
    look: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, look: 0 }
    }

    pub fn look_or_error(&self) -> ParserResult<Token> {
        match self.look_at() {
            Some(token) => Ok(token),
            None => {
                let kind = ParserErrorKind::StatementEndsInTheMiddle;
                let loc = self.look_prev().unwrap().location;
                let error = ParserError::new(kind, loc);
                Err(error)
            }
        }
    }

    pub fn look_and_forward_or_error(&mut self) -> ParserResult<Token> {
        let token = self.look_or_error()?;
        self.forward();
        Ok(token)
    }

    pub fn parse(&mut self) -> ParserResult<AST> {
        self.parse_block()
    }

    fn parse_block(&mut self) -> ParserResult<AST> {
        // block  -> stmt* return

        if !self.at_end() {
            let mut token = self.look_at().unwrap();
            let start = token.location;
            let mut lines = vec![];
            while !self.at_end() {
                match token.kind {
                    TokenKind::Reserved(reserved)
                        if reserved.is_literal(String::from("return")) =>
                    {
                        let ast = self.parse_return()?;
                        lines.push(ast);
                    }
                    _ => {
                        let ast = self.parse_stmt()?;
                        lines.push(ast);
                    }
                }
                if self.at_end() {
                    break;
                } else {
                    token = self.look_at().unwrap();
                }
            }
            let kind = ASTKind::Block(lines);
            let loc = start.extend_to(token.location);
            Ok(AST::new(kind, loc))
        } else {
            let kind = ParserErrorKind::StatementEndsInTheMiddle;
            let error = ParserError::new(kind, self.look_prev().unwrap().location);
            Err(error)
        }
    }

    fn parse_return(&mut self) -> ParserResult<AST> {
        // return -> `return` expr `;`

        let ret = self.look_and_forward().unwrap();

        // expr の parse
        let expr = self.parse_expr()?;

        // semicolon の parse
        let semicolon = self.look_and_forward_or_error()?;
        match semicolon.kind {
            TokenKind::Delimiter(del) if del.is_literal(';') => {
                let kind = ASTKind::Return(Box::new(expr));
                let loc = ret.location.extend_to(semicolon.location);
                Ok(AST::new(kind, loc))
            }
            _ => {
                let kind = ParserErrorKind::NotEndWithSemicolon;
                let error = ParserError::new(kind, semicolon.location);
                return Err(error);
            }
        }
    }

    fn parse_stmt(&mut self) -> ParserResult<AST> {
        // stmt -> assigin

        // assign の parse
        self.parse_assign()
    }

    fn parse_assign(&mut self) -> ParserResult<AST> {
        // assign -> identifier `=` expr `;`

        // identifier の parse
        let id = self.look_and_forward().unwrap();
        let name = match id.kind {
            TokenKind::Identifier(name) => name,
            _ => {
                let kind = ParserErrorKind::AssignStartsWithIdentifier;
                let error = ParserError::new(kind, id.location);
                return Err(error);
            }
        };

        // '=' の parse
        let equal = self.look_and_forward_or_error()?;
        match equal.kind {
            TokenKind::Operator(ope) if ope.is_literal(String::from("=")) => {}
            _ => {
                let kind = ParserErrorKind::AssignHasEqualOnSecondToken(name);
                let error = ParserError::new(kind, equal.location);
                return Err(error);
            }
        }

        let expr = self.parse_expr()?;

        // semicolon の parse
        let semicolon = self.look_and_forward_or_error()?;
        match semicolon.kind {
            TokenKind::Delimiter(delimiter) if delimiter.is_literal(';') => {}
            _ => {
                let kind = ParserErrorKind::NotEndWithSemicolon;
                let error = ParserError::new(kind, semicolon.location);
                return Err(error);
            }
        }

        let kind = ASTKind::Assign(name, Box::new(expr));
        let loc = id.location.extend_to(semicolon.location);
        Ok(AST::new(kind, loc))
    }

    fn parse_expr(&mut self) -> ParserResult<AST> {
        // expr -> term expr'
        let term_left = self.parse_term()?;
        match self.parse_expr_prime()? {
            Some((ope, term_right)) => {
                let loc = term_left.location.extend_to(term_right.location);
                let kind = ASTKind::Binary(Box::new(term_left), Box::new(term_right), ope);
                Ok(AST::new(kind, loc))
            }
            None => Ok(term_left),
        }
    }

    fn parse_expr_prime(&mut self) -> ParserResult<Option<(OperatorKind, AST)>> {
        // expr'  -> (`+`|`-`) term expr' | epsilon
        let token = self.look_or_error()?;
        match token.kind {
            TokenKind::Operator(ope_kind) if ope_kind.priority().is_addition() => {
                self.forward();
                let term_left = self.parse_term()?;
                let ast = match self.parse_expr_prime()? {
                    Some((ope, term_right)) => {
                        let loc = term_left.location.extend_to(term_right.location);
                        let kind = ASTKind::Binary(Box::new(term_left), Box::new(term_right), ope);
                        AST::new(kind, loc)
                    }
                    None => term_left,
                };
                Ok(Some((ope_kind, ast)))
            }
            _ => Ok(None),
        }
    }

    fn parse_term(&mut self) -> ParserResult<AST> {
        // term   -> unary term'
        let unary_left = self.parse_unary()?;
        match self.parse_term_prime()? {
            Some((ope, unary_right)) => {
                let loc = unary_left.location.extend_to(unary_right.location);
                let kind = ASTKind::Binary(Box::new(unary_left), Box::new(unary_right), ope);
                Ok(AST::new(kind, loc))
            }
            None => Ok(unary_left),
        }
    }

    fn parse_term_prime(&mut self) -> ParserResult<Option<(OperatorKind, AST)>> {
        // term'  -> (`*`|`/`) unary term' | epsilon
        let token = self.look_or_error()?;
        match token.kind {
            TokenKind::Operator(ope_kind) if ope_kind.priority().is_multiplication() => {
                self.forward();
                let unary_left = self.parse_unary()?;
                let ast = match self.parse_term_prime()? {
                    Some((ope, unary_right)) => {
                        let loc = unary_left.location.extend_to(unary_right.location);
                        let kind =
                            ASTKind::Binary(Box::new(unary_left), Box::new(unary_right), ope);
                        AST::new(kind, loc)
                    }
                    None => unary_left,
                };
                Ok(Some((ope_kind, ast)))
            }
            _ => Ok(None),
        }
    }

    fn parse_unary(&mut self) -> ParserResult<AST> {
        // unary  -> (`+`|`-`) factor | factor
        let token = self.look_or_error()?;
        let ope = match token.kind {
            TokenKind::Operator(ope_kind) if ope_kind.priority().is_addition() => {
                self.forward();
                Some(ope_kind)
            }
            _ => None,
        };
        let factor = self.parse_factor()?;
        match ope {
            Some(OperatorKind::Minus) => {
                let loc = token.location.extend_to(factor.location);
                let kind = ASTKind::Unary(Box::new(factor), OperatorKind::Minus);
                Ok(AST::new(kind, loc))
            }
            _ => Ok(factor),
        }
    }

    fn parse_factor(&mut self) -> ParserResult<AST> {
        // factor -> `(` expr `)` | value
        let token = self.look_or_error()?;
        match token.kind {
            TokenKind::Paren(paren_kind) if paren_kind.is_literal('(') => {
                self._parse_expr_enclosed_paren()
            }
            _ => self.parse_value(),
            // _ => {
            //     let kind = ParserErrorKind::ExpectedToken(
            //         token.to_string(),
            //         String::from("<number> / <paren-expr>"),
            //     );
            //     let error = ParserError::new(kind, token.location);
            //     Err(error)
            // }
        }
    }

    fn _parse_expr_enclosed_paren(&mut self) -> ParserResult<AST> {
        self.forward();
        let expr = self.parse_expr()?;

        let close_token = self.look_and_forward_or_error()?;
        match close_token.kind {
            TokenKind::Paren(paren_kind) if paren_kind.is_literal(')') => Ok(expr),
            _ => {
                let kind =
                    ParserErrorKind::ExpectedToken(close_token.to_string(), String::from(")"));
                let error = ParserError::new(kind, close_token.location);
                Err(error)
            }
        }
    }

    fn parse_value(&mut self) -> ParserResult<AST> {
        // value -> integer | identifier
        let token = self.look_and_forward_or_error().unwrap();
        match token.kind {
            TokenKind::Integer(n) => {
                let kind = ASTKind::Integer(n);
                Ok(AST::new(kind, token.location))
            }
            TokenKind::Identifier(name) => {
                let kind = ASTKind::Identifier(name);
                Ok(AST::new(kind, token.location))
            }
            _ => {
                let kind = ParserErrorKind::ExpectedToken(
                    token.to_string(),
                    String::from("<number> / <identifier> / <paren-expr>"),
                );
                let error = ParserError::new(kind, token.location);
                Err(error)
            }
        }
    }
}

impl Inspector for Parser {
    type Yielded = Token;

    fn forward(&mut self) {
        self.look += 1;
    }

    fn at_end(&self) -> bool {
        self.look >= self.tokens.len()
    }

    fn look_at(&self) -> Option<Token> {
        self.tokens.get(self.look).map(|t| t.clone())
    }

    fn look_prev(&self) -> Option<Token> {
        self.tokens.get(self.look - 1).map(|t| t.clone())
    }

    fn look_next(&self) -> Option<Token> {
        self.tokens.get(self.look + 1).map(|t| t.clone())
    }

    fn look_and_forward(&mut self) -> Option<Token> {
        self.forward();
        self.look_prev()
    }

    fn look_head(&self) -> Option<Token> {
        self.tokens.get(0).map(|t| t.clone())
    }

    fn look_tail(&self) -> Option<Token> {
        let tail = self.tokens.len() - 1;
        self.tokens.get(tail).map(|t| t.clone())
    }
}
