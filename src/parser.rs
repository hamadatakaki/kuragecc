use super::ast::{ASTKind, AST};
use super::error::{ParserError, ParserErrorKind, ParserResult};
use super::token::literal::{OperatorKind, TerminalSymbol};
use super::token::{Token, TokenKind};
use super::Inspector;

pub struct Parser {
    tokens: Vec<Token>,
    scope: i32,
    look: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            scope: 0,
            look: 0,
        }
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
        self.parse_program()
    }

    fn parse_program(&mut self) -> ParserResult<AST> {
        // program -> func*
        let mut blocks = vec![];
        let token = self.look_or_error()?;
        let start = token.location;
        while !self.at_end() {
            let ast = self.parse_func()?;
            blocks.push(ast);
        }
        let end = blocks.last().unwrap().location;
        let loc = start.extend_to(end);
        let kind = ASTKind::Program(blocks);
        Ok(AST::new(kind, self.scope, loc))
    }

    fn parse_func(&mut self) -> ParserResult<AST> {
        // func   -> identifier `(` `)` block
        let token = self.look_and_forward_or_error()?;
        let name = match token.kind {
            TokenKind::Identifier(name) => name,
            _ => {
                let error = ParserError::expected_token(
                    token.to_string(),
                    String::from("<identifier>"),
                    token.location,
                );
                return Err(error);
            }
        };

        let normal_open = self.look_and_forward_or_error()?;
        let normal_close = self.look_and_forward_or_error()?;

        match normal_open.kind {
            TokenKind::Paren(paren) if paren.is_literal('(') => {}
            _ => {
                let error = ParserError::expected_token(
                    normal_open.to_string(),
                    String::from("("),
                    normal_open.location,
                );
                return Err(error);
            }
        }

        match normal_close.kind {
            TokenKind::Paren(paren) if paren.is_literal(')') => {}
            _ => {
                let error = ParserError::expected_token(
                    normal_close.to_string(),
                    String::from(")"),
                    normal_close.location,
                );
                return Err(error);
            }
        }

        let block = self.parse_block()?;
        let loc = token.location.extend_to(block.location);
        let kind = ASTKind::Func(name, Box::new(block));
        Ok(AST::new(kind, self.scope, loc))
    }

    fn parse_block(&mut self) -> ParserResult<AST> {
        // block  -> `{` stmt* `}`
        self.scope += 1;

        // { の parse
        let curly_open = self.look_and_forward_or_error()?;
        match curly_open.kind {
            TokenKind::Paren(paren) if paren.is_literal('{') => {}
            _ => {
                let error = ParserError::expected_token(
                    curly_open.to_string(),
                    String::from("{"),
                    curly_open.location,
                );
                return Err(error);
            }
        }

        let mut token = self.look_or_error()?;
        let mut lines = vec![];
        loop {
            match token.kind {
                TokenKind::Paren(paren) if paren.is_literal('}') => break,
                _ => {
                    let ast = self.parse_stmt()?;
                    lines.push(ast);
                }
            }
            token = self.look_or_error()?;
        }

        // } の parse
        let curly_close = self.look_and_forward_or_error()?;
        match curly_close.kind {
            TokenKind::Paren(paren) if paren.is_literal('}') => {}
            _ => {
                let error = ParserError::expected_token(
                    curly_close.to_string(),
                    String::from("}"),
                    curly_open.location,
                );
                return Err(error);
            }
        }

        self.scope -= 1;

        let kind = ASTKind::Block(lines);
        let loc = curly_open.location.extend_to(curly_close.location);
        let ast = AST::new(kind, self.scope, loc);

        Ok(ast)
    }

    fn parse_stmt(&mut self) -> ParserResult<AST> {
        // stmt -> assign | return
        let token = self.look_or_error()?;
        match token.kind {
            TokenKind::Reserved(reserved) if reserved.is_literal(String::from("return")) => {
                self.parse_return()
            }
            TokenKind::Identifier(_) => self.parse_assign(),
            _ => {
                let error = ParserError::expected_token(
                    token.to_string(),
                    String::from("return / <identifier>"),
                    token.location,
                );
                Err(error)
            }
        }
    }

    fn parse_return(&mut self) -> ParserResult<AST> {
        // return -> `return` expr `;`

        // return の parse
        let ret = self.look_and_forward().unwrap();

        // expr の parse
        let expr = self.parse_expr()?;

        // semicolon の parse
        let semicolon = self.look_and_forward_or_error()?;
        match semicolon.kind {
            TokenKind::Delimiter(del) if del.is_literal(';') => {
                let kind = ASTKind::Return(Box::new(expr));
                let loc = ret.location.extend_to(semicolon.location);
                Ok(AST::new(kind, self.scope, loc))
            }
            _ => {
                let kind = ParserErrorKind::NotEndWithSemicolon;
                let error = ParserError::new(kind, semicolon.location);
                return Err(error);
            }
        }
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
        Ok(AST::new(kind, self.scope, loc))
    }

    fn parse_expr(&mut self) -> ParserResult<AST> {
        // expr -> term expr'
        let term = self.parse_term()?;
        self.parse_expr_prime(term)
    }

    fn parse_expr_prime(&mut self, expr_left: AST) -> ParserResult<AST> {
        // expr'  -> (`+`|`-`) term expr' | epsilon
        let token = self.look_or_error()?;
        match token.kind {
            TokenKind::Operator(ope_kind) if ope_kind.priority().is_addition() => {
                self.forward();
                let term = self.parse_term()?;
                let loc = expr_left.location.extend_to(term.location);
                let kind = ASTKind::Binary(Box::new(expr_left), Box::new(term), ope_kind);
                let expr = AST::new(kind, self.scope, loc);
                self.parse_expr_prime(expr)
            }
            _ => Ok(expr_left),
        }
    }

    fn parse_term(&mut self) -> ParserResult<AST> {
        // term   -> unary term'
        let unary = self.parse_unary()?;
        self.parse_term_prime(unary)
    }

    fn parse_term_prime(&mut self, term_left: AST) -> ParserResult<AST> {
        // term'  -> (`*`|`/`) unary term' | epsilon
        let token = self.look_or_error()?;
        match token.kind {
            TokenKind::Operator(ope_kind) if ope_kind.priority().is_multiplication() => {
                self.forward();
                let unary = self.parse_unary()?;
                let loc = term_left.location.extend_to(unary.location);
                let kind = ASTKind::Binary(Box::new(term_left), Box::new(unary), ope_kind);
                let term = AST::new(kind, self.scope, loc);
                self.parse_term_prime(term)
            }
            _ => Ok(term_left),
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
                Ok(AST::new(kind, self.scope, loc))
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
        }
    }

    fn _parse_expr_enclosed_paren(&mut self) -> ParserResult<AST> {
        self.forward();
        let expr = self.parse_expr()?;

        let close_token = self.look_and_forward_or_error()?;
        match close_token.kind {
            TokenKind::Paren(paren_kind) if paren_kind.is_literal(')') => Ok(expr),
            _ => {
                let error = ParserError::expected_token(
                    close_token.to_string(),
                    String::from(")"),
                    close_token.location,
                );
                Err(error)
            }
        }
    }

    fn parse_value(&mut self) -> ParserResult<AST> {
        // value -> integer | identifier
        let token = self.look_or_error()?;
        match token.kind {
            TokenKind::Integer(_) => self.parse_integer(),
            TokenKind::Identifier(_) => self.parse_identifier_or_call(),
            _ => {
                let error = ParserError::expected_token(
                    token.to_string(),
                    String::from("<number> / <identifier>"),
                    token.location,
                );
                Err(error)
            }
        }
    }

    fn parse_integer(&mut self) -> ParserResult<AST> {
        let token = self.look_and_forward_or_error()?;
        match token.kind {
            TokenKind::Integer(n) => {
                let kind = ASTKind::Integer(n);
                Ok(AST::new(kind, self.scope, token.location))
            }
            _ => {
                let error = ParserError::expected_token(
                    token.to_string(),
                    String::from("<integer>"),
                    token.location,
                );
                Err(error)
            }
        }
    }

    fn parse_identifier(&mut self) -> ParserResult<AST> {
        let token = self.look_and_forward_or_error()?;
        match token.kind {
            TokenKind::Identifier(name) => {
                let kind = ASTKind::Identifier(name);
                Ok(AST::new(kind, self.scope, token.location))
            }
            _ => {
                let error = ParserError::expected_token(
                    token.to_string(),
                    String::from("<identifier>"),
                    token.location,
                );
                Err(error)
            }
        }
    }

    fn parse_identifier_or_call(&mut self) -> ParserResult<AST> {
        let token = self.look_and_forward_or_error()?;
        let name = match token.kind {
            TokenKind::Identifier(name) => name,
            _ => {
                let error = ParserError::expected_token(
                    token.to_string(),
                    String::from("<identifier>"),
                    token.location,
                );
                return Err(error);
            }
        };

        let normal_open = self.look_or_error()?;

        match normal_open.kind {
            TokenKind::Paren(paren) if paren.is_literal('(') => {}
            _ => {
                let kind = ASTKind::Identifier(name);
                return Ok(AST::new(kind, self.scope, token.location));
            }
        }

        self.forward();
        let normal_close = self.look_and_forward_or_error()?;

        match normal_close.kind {
            TokenKind::Paren(paren) if paren.is_literal(')') => {}
            _ => {
                let error = ParserError::expected_token(
                    normal_close.to_string(),
                    String::from(")"),
                    normal_close.location,
                );
                return Err(error);
            }
        };

        let kind = ASTKind::FuncCall(name);
        let loc = token.location.extend_to(normal_close.location);
        Ok(AST::new(kind, self.scope, loc))
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
