pub mod code;
pub mod symbol;

use super::ast::{ASTKind, AST};
use super::token::literal::OperatorKind;
use code::{Code, Expression};
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
            .map(|code| code.to_string())
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
                let expr = Expression::Symbol(Symbol(format!("{}", name)));
                self.codes.push(Code::DefineOpen(expr));
                self.gen_block(*block);
                self.codes.push(Code::DefineClose);
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

                match expr {
                    Expression::Value(n) => {
                        let ano = self.table.anonymous_symbol();
                        let ano = Expression::Symbol(ano);
                        let symbol = self.table.register_name(name.clone());
                        let symbol = Expression::Symbol(symbol);
                        self.codes.push(Code::Alloca(ano.clone()));
                        self.codes.push(Code::Store(ano.clone(), n));
                        self.codes.push(Code::Load(ano, symbol));
                    }
                    Expression::Symbol(symbol) => {
                        self.table.overwrite_name_and_symbol(name, symbol)
                    }
                }
            }
            _ => unreachable!(),
        }
    }

    fn gen_identifier(&mut self, name: String) -> Expression {
        match self.table.get_symbol(&name) {
            Some(sym) => Expression::Symbol(sym),
            None => unreachable!(),
        }
    }

    fn gen_expr(&mut self, ast: AST) -> Expression {
        match ast.kind {
            ASTKind::Integer(n) => Expression::Value(n as i32),
            ASTKind::Identifier(name) => match self.table.get_symbol(&name) {
                Some(sym) => Expression::Symbol(sym.clone()),
                None => unreachable!(),
            },
            ASTKind::FuncCall { name, args } => {
                let assigned = self.table.anonymous_symbol();
                let assigned = Expression::Symbol(assigned);
                let code = Code::FuncCall(name, assigned.clone());
                self.codes.push(code);
                assigned
            }
            ASTKind::Binary(left, right, ope) => {
                let l = self.gen_expr(*left);
                let r = self.gen_expr(*right);
                let ans = self.table.anonymous_symbol();
                let ans = Expression::Symbol(ans);
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
                OperatorKind::Minus => match self.gen_expr(*factor) {
                    Expression::Value(n) => Expression::Value(-n),
                    Expression::Symbol(sym) => {
                        let ans = self.table.anonymous_symbol();
                        let ans = Expression::Symbol(ans);
                        let l = Expression::Symbol(sym);
                        let r = Expression::Value(-1);
                        let code = Code::Multi(l, r, ans.clone());
                        self.codes.push(code);
                        ans
                    }
                },
                _ => unreachable!(),
            },
            _ => unreachable!(),
        }
    }
}
