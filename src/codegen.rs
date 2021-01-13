pub mod code;
pub mod symbol;

use super::ast::{ASTKind, AST};
use super::token::literal::OperatorKind;
use code::{Code, CodeType, Expression, ExpressionKind};
use symbol::{Symbol, SymbolTable};

pub struct CodeGenerator {
    ast: AST,
    codes: Vec<Code>,
    table: SymbolTable,
}

impl CodeGenerator {
    pub fn new(ast: AST) -> Self {
        Self {
            ast,
            codes: Vec::new(),
            table: SymbolTable::new(),
        }
    }

    pub fn code(&self) -> String {
        self.codes
            .iter()
            .map(|code| code.to_assembly())
            .collect::<String>()
    }

    pub fn gen_code(&mut self) -> String {
        self.gen_program(self.ast.clone());
        self.code()
    }

    fn gen_program(&mut self, ast: AST) {
        match ast.kind {
            ASTKind::Program(asts) => {
                for ast in asts {
                    self.gen_func(ast);
                }
            }
            _ => unreachable!(),
        }
    }

    fn gen_func(&mut self, ast: AST) {
        match ast.kind {
            ASTKind::Func {
                name,
                params,
                block,
            } => {
                let sym = Symbol::new(format!("{}", name));
                let expr = sym.to_expr();
                let mut syms = Vec::new();
                for param in params.clone() {
                    let sym = match param.kind {
                        ASTKind::Identifier(name) => Symbol::new(name),
                        _ => unreachable!(),
                    };
                    syms.push(sym);
                }
                self.codes.push(Code::FuncDefineOpen(expr, syms.clone()));

                for (index, sym) in syms.iter().enumerate() {
                    let arg = Symbol::new(format!("%{}", index)).to_expr();
                    let ano = self.table.anonymous_symbol().to_expr();
                    let symbol = self.table.register_name(sym.reveal()).to_expr();
                    self.codes.push(Code::Alloca(ano.clone()));
                    self.codes.push(Code::Store(ano.clone(), arg));
                    self.codes.push(Code::Load(ano, symbol));
                }

                self.gen_block(*block);
                self.codes.push(Code::FuncDefineClose);
            }
            _ => unreachable!(),
        }
    }

    fn gen_block(&mut self, ast: AST) {
        let stmts = match ast.kind {
            ASTKind::Block(stmts) => stmts,
            _ => unreachable!(),
        };
        for stmt in stmts {
            match stmt.clone().kind {
                ASTKind::Assign(_, _) => self.gen_assign(stmt),
                ASTKind::Return(_) => self.gen_return(stmt),
                _ => unreachable!(),
            }
        }
    }

    fn gen_return(&mut self, ast: AST) {
        let ret = match ast.kind {
            ASTKind::Return(expr) => match expr.kind {
                ASTKind::Identifier(name) => self.gen_identifier(name),
                _ => self.gen_expr(*expr),
            },
            _ => unreachable!(),
        };
        self.codes.push(Code::Return(ret))
    }

    fn gen_assign(&mut self, ast: AST) {
        // Alloca(<name1>)
        // Store(<name1>, <integer>)
        // Load(<name1>, <name2>)

        match ast.kind {
            ASTKind::Assign(name, expr) => {
                let expr = self.gen_expr(*expr);

                match expr.kind {
                    ExpressionKind::Value(n) => {
                        let ano = self.table.anonymous_symbol().to_expr();
                        let symbol = self.table.register_name(name.clone()).to_expr();
                        self.codes.push(Code::Alloca(ano.clone()));
                        let kind = ExpressionKind::Value(n);
                        let expr = Expression::new(kind, CodeType::Int);
                        self.codes.push(Code::Store(ano.clone(), expr));
                        self.codes.push(Code::Load(ano, symbol));
                    }
                    ExpressionKind::Symbol(symbol) => {
                        self.table.overwrite_name_and_symbol(name, symbol)
                    }
                }
            }
            _ => unreachable!(),
        }
    }

    fn gen_identifier(&mut self, name: String) -> Expression {
        match self.table.get_symbol(&name) {
            Some(sym) => sym.to_expr(),
            None => unreachable!(),
        }
    }

    fn gen_expr(&mut self, ast: AST) -> Expression {
        match ast.kind {
            ASTKind::Integer(n) => {
                let kind = ExpressionKind::Value(n as i32);
                Expression::new(kind, CodeType::Int)
            }
            ASTKind::Identifier(name) => match self.table.get_symbol(&name) {
                Some(sym) => sym.to_expr(),
                None => unreachable!(),
            },
            ASTKind::FuncCall { name, args } => {
                let mut syms = Vec::new();

                for arg in args {
                    let expr = self.gen_expr(arg);
                    syms.push(expr);
                }

                let assigned = self.table.anonymous_symbol().to_expr();
                let code = Code::FuncCall {
                    name,
                    assigned: assigned.clone(),
                    args: syms,
                };
                self.codes.push(code);
                assigned
            }
            ASTKind::Binary(left, right, ope) => {
                let l = self.gen_expr(*left);
                let r = self.gen_expr(*right);
                let ans = self.table.anonymous_symbol().to_expr();
                let code = match ope {
                    OperatorKind::Plus => Code::Add(l, r, ans.clone()),
                    OperatorKind::Minus => Code::Sub(l, r, ans.clone()),
                    OperatorKind::Times => Code::Multi(l, r, ans.clone()),
                    OperatorKind::Devide => Code::Divide(l, r, ans.clone()),
                    _ => unreachable!(),
                };
                self.codes.push(code);
                ans
            }
            ASTKind::Unary(factor, ope) => match ope {
                OperatorKind::Minus => {
                    let expr = self.gen_expr(*factor);
                    match expr.kind {
                        ExpressionKind::Value(n) => {
                            let kind = ExpressionKind::Value(-n);
                            Expression::new(kind, CodeType::Int)
                        }
                        ExpressionKind::Symbol(sym) => {
                            let ans = self.table.anonymous_symbol().to_expr();
                            let l = sym.to_expr();
                            let r = {
                                let kind = ExpressionKind::Value(-1);
                                Expression::new(kind, CodeType::Int)
                            };
                            let code = Code::Multi(l, r, ans.clone());
                            self.codes.push(code);
                            ans
                        }
                    }
                }
                _ => unreachable!(),
            },
            _ => unreachable!(),
        }
    }
}
