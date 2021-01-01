use super::ast::{ASTKind, AST};
use super::token::literal::{OperatorKind, TerminalSymbol};
use super::token::{Token, TokenKind};
use super::{Inspector, Place};

/*
    stmt   -> return
    return -> `return` expr `;`
    expr   -> term expr'
    expr'  -> (`+`|`-`) term expr' | epsilon
    term   -> unary term'
    term'  -> (`*`|`/`) unary term' | epsilon
    unary  -> (`+`|`-`) factor | factor
    factor -> `(` expr `)` | number
    number -> integer
*/

pub struct Parser {
    tokens: Vec<Token>,
    pub ast: Option<AST>,
    look: usize,
}

impl Inspector for Parser {
    type Yielded = Token;

    fn forward(&mut self) {
        self.look += 1;
    }

    fn backward(&mut self) {
        self.look -= 1;
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
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            ast: None,
            look: 0,
        }
    }

    pub fn parse(&mut self) {
        self.ast = Some(self.parse_stmt());
    }

    fn parse_stmt(&mut self) -> AST {
        // stmt   -> return
        self.parse_return()
    }

    fn parse_return(&mut self) -> AST {
        // return -> `return` expr `;`
        let ret = self.look_and_forward().unwrap();
        let expr = self.parse_expr();
        let semicolon = self.look_and_forward().unwrap();
        let pos = ret.pos.extend_to(semicolon.pos);
        let kind = ASTKind::Return(Box::new(expr));
        AST::new(kind, pos)
    }

    fn parse_expr(&mut self) -> AST {
        // expr   -> term expr'
        let term_left = self.parse_term();
        match self.parse_expr_prime() {
            Some((ope, term_right)) => {
                let pos = term_left.pos.extend_to(term_right.pos);
                let kind = ASTKind::Binary(Box::new(term_left), Box::new(term_right), ope);
                AST::new(kind, pos)
            }
            None => term_left,
        }
    }

    fn parse_expr_prime(&mut self) -> Option<(OperatorKind, AST)> {
        // expr'  -> (`+`|`-`) term expr' | epsilon
        let looked = self.look_at().unwrap();
        match looked.kind {
            TokenKind::Operator(ope_kind) => {
                self.forward();
                let term_left = self.parse_term();
                let ast = match self.parse_expr_prime() {
                    Some((ope, term_right)) => {
                        let pos = term_left.pos.extend_to(term_right.pos);
                        let kind = ASTKind::Binary(Box::new(term_left), Box::new(term_right), ope);
                        AST::new(kind, pos)
                    }
                    None => term_left,
                };
                Some((ope_kind, ast))
            }
            _ => None,
        }
    }

    fn parse_term(&mut self) -> AST {
        // term   -> unary term'
        let unary_left = self.parse_unary();
        match self.parse_term_prime() {
            Some((ope, unary_right)) => {
                let pos = unary_left.pos.extend_to(unary_right.pos);
                let kind = ASTKind::Binary(Box::new(unary_left), Box::new(unary_right), ope);
                AST::new(kind, pos)
            }
            None => unary_left,
        }
    }

    fn parse_term_prime(&mut self) -> Option<(OperatorKind, AST)> {
        // term'  -> (`*`|`/`) unary term' | epsilon
        let looked = self.look_at().unwrap();
        match looked.kind {
            TokenKind::Operator(ope_kind) if ope_kind.priority().is_multiplication() => {
                self.forward();
                let unary_left = self.parse_unary();
                let ast = match self.parse_term_prime() {
                    Some((ope, unary_right)) => {
                        let pos = unary_left.pos.extend_to(unary_right.pos);
                        let kind = ASTKind::Binary(
                            Box::new(unary_left.clone()),
                            Box::new(unary_right.clone()),
                            ope,
                        );
                        AST::new(kind, pos)
                    }
                    None => unary_left,
                };
                Some((ope_kind, ast))
            }
            _ => None,
        }
    }

    fn parse_unary(&mut self) -> AST {
        // unary  -> (`+`|`-`) factor | factor
        let looked = self.look_at().unwrap();
        let ope = match looked.kind {
            TokenKind::Operator(ope_kind) if ope_kind.priority().is_addition() => {
                self.forward();
                Some(ope_kind)
            }
            _ => None,
        };
        let factor = self.parse_factor();
        match ope {
            Some(OperatorKind::Minus) => {
                let pos = looked.pos.extend_to(factor.pos);
                let kind = ASTKind::Unary(Box::new(factor), OperatorKind::Minus);
                AST::new(kind, pos)
            }
            _ => factor,
        }
    }

    fn parse_factor(&mut self) -> AST {
        // factor -> `(` expr `)` | number
        let looked = self.look_at().unwrap();
        match looked.kind {
            TokenKind::Paren(paren_kind) if paren_kind.is_literal('(') => {
                self.forward();
                let expr = self.parse_expr();
                let close_token = self.look_and_forward().unwrap();
                match close_token.kind {
                    TokenKind::Paren(paren_kind) if paren_kind.is_literal(')') => expr,
                    _ => unreachable!(),
                }
            }
            TokenKind::Integer(_) => self.parse_number(),
            _ => unreachable!(),
        }
    }

    fn parse_number(&mut self) -> AST {
        // number -> integer
        let looked = self.look_and_forward().unwrap();
        let kind = match looked.kind {
            TokenKind::Integer(n) => ASTKind::Integer(n),
            _ => unreachable!(),
        };
        let pos = looked.pos;
        AST::new(kind, pos)
    }
}
