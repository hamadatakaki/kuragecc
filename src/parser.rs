use super::ast::{
    ASTBlock, ASTBlockKind, ASTExpr, ASTExprKind, ASTIdentifier, ASTStmt, ASTStmtKind,
    AsSyntaxExpression, AsSyntaxStatement, AST,
};
use super::error::parser::{ParserError, ParserErrorKind, ParserResult};
use super::operators::Operator;
use super::token::literal::{OperatorKind, TerminalSymbol};
use super::token::{Token, TokenKind};
use super::types::Type;
use super::Inspector;
use super::Location;

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
        // program -> block*
        let mut program = vec![];
        let token = self.look_or_error()?;
        let start = token.location;
        while !self.at_end() {
            let ast = self.parse_block()?;
            program.push(ast);
        }
        let end = program.last().unwrap().get_loc();
        let loc = start.extend_to(end);
        Ok(AST::new(program, self.scope, loc))
    }

    fn parse_block(&mut self) -> ParserResult<ASTBlock> {
        // block -> func
        self.parse_func()
    }

    fn parse_func(&mut self) -> ParserResult<ASTBlock> {
        // func -> type-id `(` param-seq `)` comp-stmt
        let identifier = self.parse_type_id()?;
        let start = identifier.get_loc();

        let normal_open = self.look_and_forward_or_error()?;

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

        let params = self.parse_func_params()?;

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
        }

        let stmts = self.parse_comp_stmt()?;

        let end = stmts.last().unwrap().get_loc();
        let loc = start.extend_to(end);
        let kind = ASTBlockKind::Func(identifier, params, stmts);
        Ok(ASTBlock::new(kind, self.scope, loc))
    }

    fn parse_func_params(&mut self) -> ParserResult<Vec<ASTIdentifier>> {
        // param-seq -> type-id (`,` type-id)* | epsilon

        let mut params = vec![];
        let mut token = self.look_or_error()?;
        match token.kind {
            TokenKind::Primitive(_) => {
                self.scope += 1;
                let identifier = self.parse_type_id()?;
                params.push(identifier);

                loop {
                    token = self.look_or_error()?;
                    match token.kind {
                        TokenKind::Delimiter(delimiter) if delimiter.is_literal(',') => {
                            self.forward()
                        }
                        _ => break,
                    };
                    let identifier = self.parse_type_id()?;
                    params.push(identifier);
                }
                self.scope -= 1;
            }
            _ => {}
        }

        Ok(params)
    }

    fn parse_comp_stmt(&mut self) -> ParserResult<Vec<ASTStmt>> {
        // comp-stmt -> `{` stmt* `}
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

        // stmt* の parse
        let mut token = self.look_or_error()?;
        let mut stmts = vec![];
        loop {
            match token.kind {
                TokenKind::Paren(paren) if paren.is_literal('}') => break,
                _ => {
                    let stmt = self.parse_stmt()?;
                    stmts.push(stmt);
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

        Ok(stmts)
    }

    fn parse_type_id(&mut self) -> ParserResult<ASTIdentifier> {
        // type-id -> type identifier
        let (ty, loc) = self.parse_type()?;
        let identifier = self.parse_identifier()?;

        let loc = loc.extend_to(identifier.get_loc());
        let id = ASTIdentifier::new(identifier.get_name(), ty, loc);
        Ok(id)
    }

    fn parse_type(&mut self) -> ParserResult<(Type, Location)> {
        // type -> primitive
        let token = self.look_and_forward_or_error()?;
        let loc = token.location;
        match token.kind {
            TokenKind::Primitive(primitive) => {
                let ty = Type::Primitive(primitive);
                Ok((ty, loc))
            }
            _ => {
                let error = ParserError::expected_token(
                    token.to_string(),
                    String::from("<primitive-type>"),
                    loc,
                );
                Err(error)
            }
        }
    }

    fn parse_stmt(&mut self) -> ParserResult<ASTStmt> {
        // stmt -> assign | declare | dec-ass | return | if
        let token = self.look_or_error()?;

        use TokenKind::*;

        match token.kind {
            Identifier(_) => self.parse_assign(),
            Primitive(_) => self._parse_declare_or_dec_ass(),
            Reserved(res) if res.is_literal(format!("return")) => self.parse_return(),
            Reserved(res) if res.is_literal(format!("if")) => self.parse_if(),
            Reserved(res) if res.is_literal(format!("while")) => self.parse_while(),
            _ => {
                let error = ParserError::expected_token(
                    token.to_string(),
                    format!("return / <identifier> / <primitive>"),
                    token.location,
                );
                Err(error)
            }
        }
    }

    fn parse_assign(&mut self) -> ParserResult<ASTStmt> {
        // assign -> identifier `=` cond `;`

        // identifier の parse
        let id = self.parse_identifier()?;

        // '=' の parse
        let equal = self.look_and_forward_or_error()?;
        match equal.kind {
            TokenKind::Operator(ope) if ope.is_literal(String::from("=")) => {}
            _ => {
                let kind = ParserErrorKind::AssignHasEqualOnSecondToken(id);
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

        let loc = id.get_loc().extend_to(semicolon.location);
        let kind = ASTStmtKind::Assign(id, expr);
        Ok(ASTStmt::new(kind, self.scope, loc))
    }

    fn _parse_declare_or_dec_ass(&mut self) -> ParserResult<ASTStmt> {
        // type-id の parse
        let id = self.parse_type_id()?;

        // `;` か `=` の parse
        let token = self.look_and_forward_or_error()?;
        match token.kind {
            TokenKind::Delimiter(del) if del.is_literal(';') => {
                // `;` なら Declare として parse
                let loc = id.get_loc().extend_to(token.location);
                let kind = ASTStmtKind::Declare(id);
                return Ok(ASTStmt::new(kind, self.scope, loc));
            }
            // `=` なら DeclareAssign として parse
            TokenKind::Operator(ope) if ope.is_literal(String::from("=")) => {}
            _ => {
                // それ以外なら ParserError
                let error = ParserError::expected_token(
                    token.to_string(),
                    String::from("`;` / `=`"),
                    token.location,
                );
                return Err(error);
            }
        }

        // expr の parse
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

        let loc = id.get_loc().extend_to(semicolon.location);
        let kind = ASTStmtKind::DeclareAssign(id, expr);
        Ok(ASTStmt::new(kind, self.scope, loc))
    }

    fn parse_return(&mut self) -> ParserResult<ASTStmt> {
        // return -> `return` expr `;`

        // return の parse
        let ret = self.look_and_forward().unwrap();

        // expr の parse
        let expr = self.parse_expr()?;

        // semicolon の parse
        let semicolon = self.look_and_forward_or_error()?;
        match semicolon.kind {
            TokenKind::Delimiter(del) if del.is_literal(';') => {}
            _ => {
                let kind = ParserErrorKind::NotEndWithSemicolon;
                let error = ParserError::new(kind, semicolon.location);
                return Err(error);
            }
        }

        let kind = ASTStmtKind::Return(expr);
        let loc = ret.location.extend_to(semicolon.location);
        Ok(ASTStmt::new(kind, self.scope, loc))
    }

    fn parse_if(&mut self) -> ParserResult<ASTStmt> {
        // if -> `if` `(` expr `)` (stmt | comp-stmt) (`else` stmt)?

        // if の parse
        let res_if = self.look_and_forward().unwrap();

        let normal_open = self.look_and_forward_or_error()?;

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

        let cond = self.parse_expr()?;

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
        }

        let token = self.look_or_error()?;
        let t_stmts = match token.clone().kind {
            TokenKind::Paren(paren) if paren.is_literal('{') => self.parse_comp_stmt()?,
            _ => vec![self.parse_stmt()?],
        };

        let res_else = self.look_and_forward_or_error()?;

        match res_else.kind {
            // `else` なら forward だけして pass
            TokenKind::Reserved(res) if res.is_literal(format!("else")) => {}
            _ => {
                // `else` じゃなければ f_stmts は empty
                let kind = ASTStmtKind::If(cond, t_stmts, vec![]);
                let end_loc = self.look_prev().unwrap().location;
                let loc = res_if.location.extend_to(end_loc);
                return Ok(ASTStmt::new(kind, self.scope, loc));
            }
        }

        let token = self.look_or_error()?;
        let f_stmts = match token.clone().kind {
            TokenKind::Paren(paren) if paren.is_literal('{') => self.parse_comp_stmt()?,
            _ => vec![self.parse_stmt()?],
        };

        let kind = ASTStmtKind::If(cond, t_stmts, f_stmts);
        let end_loc = self.look_prev().unwrap().location;
        let loc = res_if.location.extend_to(end_loc);
        Ok(ASTStmt::new(kind, self.scope, loc))
    }

    fn parse_while(&mut self) -> ParserResult<ASTStmt> {
        // while -> `while` `(` expr7 `)` (stmt | comp-stmt)

        // if の parse
        let res_while = self.look_and_forward().unwrap();

        let normal_open = self.look_and_forward_or_error()?;

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

        let cond = self.parse_expr()?;

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
        }

        let token = self.look_or_error()?;
        let stmts = match token.clone().kind {
            TokenKind::Paren(paren) if paren.is_literal('{') => self.parse_comp_stmt()?,
            _ => vec![self.parse_stmt()?],
        };

        let kind = ASTStmtKind::While(cond, stmts);
        let end_loc = self.look_prev().unwrap().location;
        let loc = res_while.location.extend_to(end_loc);
        Ok(ASTStmt::new(kind, self.scope, loc))
    }

    // expression

    fn parse_expr(&mut self) -> ParserResult<ASTExpr> {
        self.parse_expr7()
    }

    fn parse_expr7(&mut self) -> ParserResult<ASTExpr> {
        // expr7 -> expr4 expr7'
        let expr = self.parse_expr4()?;
        self.parse_expr7_prime(expr)
    }

    fn parse_expr7_prime(&mut self, left: ASTExpr) -> ParserResult<ASTExpr> {
        // expr7' -> (`==`|`!=`) expr4 expr7' | epsilon
        let token = self.look_or_error()?;
        match token.kind {
            TokenKind::Operator(ope_kind) if ope_kind.priority().is_equivalence() => {
                self.forward();
                let right = self.parse_expr4()?;
                let loc = left.get_loc().extend_to(right.get_loc());
                let ope = Operator::from_ope_kind(ope_kind);
                let kind = ASTExprKind::Binary(Box::new(left), Box::new(right), ope);
                let expr = ASTExpr::new(kind, loc);

                self.parse_expr7_prime(expr)
            }
            _ => Ok(left),
        }
    }

    fn parse_expr4(&mut self) -> ParserResult<ASTExpr> {
        //  expr4 -> expr3 expr4'
        let expr = self.parse_expr3()?;
        self.parse_expr4_prime(expr)
    }

    fn parse_expr4_prime(&mut self, left: ASTExpr) -> ParserResult<ASTExpr> {
        // expr4' -> (`+`|`-`) expr3 expr4' | epsilon
        let token = self.look_or_error()?;
        match token.kind {
            TokenKind::Operator(ope_kind) if ope_kind.priority().is_addition() => {
                self.forward();
                let right = self.parse_expr3()?;
                let loc = left.get_loc().extend_to(right.get_loc());
                let ope = Operator::from_ope_kind(ope_kind);
                let kind = ASTExprKind::Binary(Box::new(left), Box::new(right), ope);
                let expr = ASTExpr::new(kind, loc);

                self.parse_expr4_prime(expr)
            }
            _ => Ok(left),
        }
    }

    fn parse_expr3(&mut self) -> ParserResult<ASTExpr> {
        // expr3 -> expr2 expr3'
        let expr = self.parse_expr2()?;
        self.parse_expr3_prime(expr)
    }

    fn parse_expr3_prime(&mut self, left: ASTExpr) -> ParserResult<ASTExpr> {
        // expr3' -> (`*`|`/`) expr2 expr3' | epsilon
        let token = self.look_or_error()?;
        match token.kind {
            TokenKind::Operator(ope_kind) if ope_kind.priority().is_multiplication() => {
                self.forward();
                let right = self.parse_expr2()?;
                let loc = left.get_loc().extend_to(right.get_loc());
                let ope = Operator::from_ope_kind(ope_kind);
                let kind = ASTExprKind::Binary(Box::new(left), Box::new(right), ope);
                let term = ASTExpr::new(kind, loc);

                self.parse_expr3_prime(term)
            }
            _ => Ok(left),
        }
    }

    fn parse_expr2(&mut self) -> ParserResult<ASTExpr> {
        // expr2 -> (`+`|`-`)? factor
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
                let loc = token.location.extend_to(factor.get_loc());
                let kind = ASTExprKind::Unary(Box::new(factor), Operator::Minus);
                Ok(ASTExpr::new(kind, loc))
            }
            _ => Ok(factor),
        }
    }

    fn parse_factor(&mut self) -> ParserResult<ASTExpr> {
        // factor  -> `(` expr7 `)` | integer | identifier | call-func

        let token = self.look_or_error()?;
        match token.kind {
            TokenKind::Paren(paren_kind) if paren_kind.is_literal('(') => {
                self._parse_expr_enclosed_paren()
            }
            TokenKind::Integer(_) => self.parse_integer(),
            TokenKind::Identifier(_) => self._parse_identifier_or_call(),
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

    fn _parse_expr_enclosed_paren(&mut self) -> ParserResult<ASTExpr> {
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

    fn parse_integer(&mut self) -> ParserResult<ASTExpr> {
        let token = self.look_and_forward_or_error()?;
        match token.kind {
            TokenKind::Integer(n) => {
                let kind = ASTExprKind::Integer(n);
                Ok(ASTExpr::new(kind, token.location))
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

    fn parse_identifier(&mut self) -> ParserResult<ASTIdentifier> {
        let token = self.look_and_forward_or_error()?;

        use TokenKind::Identifier;

        match token.kind {
            Identifier(name) => Ok(ASTIdentifier::new(name, Type::NoneType, token.location)),
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

    fn _parse_identifier_or_call(&mut self) -> ParserResult<ASTExpr> {
        let id = self.parse_identifier()?;

        let normal_open = self.look_or_error()?;

        match normal_open.kind {
            TokenKind::Paren(paren) if paren.is_literal('(') => {}
            _ => {
                let kind = ASTExprKind::Identifier(id.clone());
                return Ok(ASTExpr::new(kind, id.get_loc()));
            }
        }

        self.forward();

        let args = self.parse_func_call_args()?;

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

        let loc = id.get_loc().extend_to(normal_close.location);
        let kind = ASTExprKind::FuncCall(id, args);
        Ok(ASTExpr::new(kind, loc))
    }

    fn parse_func_call_args(&mut self) -> ParserResult<Vec<ASTExpr>> {
        // arg-seq -> expr (`,` expr)* | epsilon

        let mut v = vec![];
        if let Ok(value) = self.parse_expr() {
            v.push(value);
            let mut token: Token;
            loop {
                token = self.look_or_error()?;
                match token.kind {
                    TokenKind::Delimiter(delimiter) if delimiter.is_literal(',') => self.forward(),
                    _ => break,
                };
                let value = self.parse_expr()?;
                v.push(value);
            }
        }

        Ok(v)
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
