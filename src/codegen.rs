pub mod expression;

use super::ast::{ASTKind, AST};
use super::token::literal::OperatorKind;

use std::collections::HashMap;

/*
    define i32 @main() {
        <name> = alloca i32
        store i32 <expr>, i32* <name>
        %r = load i32, i32* <name>
        ret i32 %r
    }
*/

type Symbol = String;

#[derive(Debug, Clone)]
pub struct SymbolTable {
    count: usize,
    table: HashMap<String, Symbol>,
}

impl SymbolTable {
    fn new() -> Self {
        Self {
            count: 0,
            table: HashMap::new(),
        }
    }

    fn new_id(&mut self, name: &str) -> String {
        format!("%val_{}", name)
    }

    fn anonymous_symbol(&mut self) -> Symbol {
        let name = format!("ano_{}", self.count);
        self.count += 1;
        let symbol = format!("%{}", name);
        self.table.insert(name, symbol.clone());
        symbol
    }

    fn get_symbol(&mut self, name: &String) -> Option<Symbol> {
        self.table.get(name).map(|sym| sym.clone())
    }

    fn register_name(&mut self, name: String) -> Symbol {
        let symbol = self.new_id(name.as_str());
        self.table.insert(name, symbol.clone());
        symbol
    }

    fn get_or_register_name(&mut self, name: String) -> Symbol {
        match self.table.get(&name) {
            Some(sym) => sym.clone(),
            None => self.register_name(name),
        }
    }

    fn put_name_and_symbol(&mut self, name: String, sym: Symbol) {
        self.table.insert(name, sym);
    }
}

#[derive(Debug, Clone)]
pub enum Expression {
    Value(i32),
    Symbol(Symbol),
}

impl Expression {
    fn to_symbol(&self) -> Option<Symbol> {
        match self {
            Expression::Symbol(sym) => Some(sym.clone()),
            _ => None,
        }
    }

    fn to_string(&self) -> String {
        match self {
            Expression::Value(n) => format!("{}", n),
            Expression::Symbol(sym) => sym.clone(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Code {
    DefineOpen,
    DefineClose,
    Alloca(Expression),
    Store(Expression, i32),
    Load(Expression, Expression), // (from, to)
    Return(Expression),
    // binary operations: (left, right, ans)
    Add(Expression, Expression, Expression),
    Sub(Expression, Expression, Expression),
    Multi(Expression, Expression, Expression),
    Divide(Expression, Expression, Expression),
}

impl Code {
    fn to_string(&self) -> String {
        match self {
            Code::DefineOpen => format!("define i32 @main() {{\n"),
            Code::DefineClose => format!("}}\n"),
            Code::Alloca(expr) => match expr.to_symbol() {
                Some(sym) => {
                    format!("  {} = alloca i32\n", sym)
                }
                None => unreachable!(),
            },
            Code::Store(expr, n) => match expr.to_symbol() {
                Some(sym) => {
                    format!("  store i32 {}, i32* {}\n", n, sym)
                }
                None => unreachable!(),
            },
            Code::Load(from, to) => match to.to_symbol() {
                Some(sym) => {
                    format!("  {} = load i32, i32* {}\n", sym, from.to_symbol().unwrap())
                }
                None => unreachable!(),
            },
            Code::Return(expr) => format!("  ret i32 {}\n", expr.to_string()),
            Code::Add(left, right, ans) => {
                let ans = ans.to_symbol().unwrap();
                format!(
                    "  {} = add i32 {}, {}\n",
                    ans,
                    left.to_string(),
                    right.to_string()
                )
            }
            Code::Sub(left, right, ans) => {
                let ans = ans.to_symbol().unwrap();
                format!(
                    "  {} = sub i32 {}, {}\n",
                    ans,
                    left.to_string(),
                    right.to_string()
                )
            }
            Code::Multi(left, right, ans) => {
                let ans = ans.to_symbol().unwrap();
                format!(
                    "  {} = mul i32 {}, {}\n",
                    ans,
                    left.to_string(),
                    right.to_string()
                )
            }
            Code::Divide(left, right, ans) => {
                let ans = ans.to_symbol().unwrap();
                format!(
                    "  {} = sdiv i32 {}, {}\n",
                    ans,
                    left.to_string(),
                    right.to_string()
                )
            }
        }
    }
}

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

    pub fn gen_code(&mut self) {
        self.gen_block(self.ast.clone());
    }

    fn gen_block(&mut self, ast: AST) {
        self.codes.push(Code::DefineOpen);
        let stmts = match ast.kind {
            ASTKind::Block(stmts) => stmts,
            _ => unreachable!(),
        };
        for stmt in stmts {
            match stmt.clone().kind {
                ASTKind::Assign(_, _) => self.gen_assign(stmt),
                ASTKind::Return(_) => {
                    self.gen_return(stmt);
                }
                _ => unreachable!(),
            }
        }
        self.codes.push(Code::DefineClose);
    }

    // fn gen_stmt(&mut self, ast: AST) {}

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
        // <name> = alloca i32
        // store i32 <expr>, i32* <name>

        match ast.kind {
            ASTKind::Assign(name, expr) => {
                let expr = self.gen_expr(*expr);

                match expr {
                    Expression::Value(n) => {
                        let ano = self.table.anonymous_symbol();
                        let id = self.table.get_or_register_name(name.clone());
                        self.table.put_name_and_symbol(name.clone(), ano.clone());
                        let ano = Expression::Symbol(ano);
                        let name = Expression::Symbol(id);
                        self.codes.push(Code::Alloca(name.clone()));
                        self.codes.push(Code::Store(name.clone(), n));
                        self.codes.push(Code::Load(name, ano));
                    }
                    Expression::Symbol(symbol) => self.table.put_name_and_symbol(name, symbol),
                }
            }
            _ => unreachable!(),
        }
    }

    fn gen_identifier(&mut self, name: String) -> Expression {
        match self.table.get_symbol(&name) {
            Some(sym) => Expression::Symbol(sym),
            None => unimplemented!(),
        }
    }

    fn gen_expr(&mut self, ast: AST) -> Expression {
        match ast.kind {
            ASTKind::Integer(n) => Expression::Value(n as i32),
            ASTKind::Identifier(name) => match self.table.get_symbol(&name) {
                Some(sym) => Expression::Symbol(sym.clone()),
                None => unreachable!(),
            },
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
            _ => unimplemented!(),
        }
    }
}
