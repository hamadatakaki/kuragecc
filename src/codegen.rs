pub mod code;
pub mod expression;
pub mod symbol_table;

use super::ast::{
    ASTBlock, ASTBlockKind, ASTExpr, ASTExprKind, ASTIdentifier, ASTStmt, ASTStmtKind, AST,
};
use super::token::literal::OperatorKind;
use code::{Code, CodeType};
use expression::{CodeExpression, Expression, ExpressionKind, Symbol, Value, ValueKind};
use symbol_table::SymbolTable;

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
        for block in ast.program {
            self.gen_block(block);
        }
    }

    fn gen_block(&mut self, block: ASTBlock) {
        match block.kind {
            ASTBlockKind::Func(id, params, stmts) => self.gen_func(id, params, stmts),
        }
    }

    fn gen_func(&mut self, id: ASTIdentifier, params: Vec<ASTIdentifier>, stmts: Vec<ASTStmt>) {
        let sym = Symbol::from_identifier(id);

        let mut syms = Vec::new();
        for param in params.clone() {
            let sym = Symbol::new(param.get_name());
            syms.push(sym);
        }
        self.codes.push(Code::FuncDefineOpen(sym, syms.clone()));

        for (index, sym) in syms.iter().enumerate() {
            let arg = Symbol::new(format!("%{}", index)).to_expr();
            let ano = self.table.anonymous_symbol().to_expr();
            let symbol = self.table.register_name(sym.to_string()).to_expr();
            self.codes.push(Code::Alloca(ano.clone()));
            self.codes.push(Code::Store(ano.clone(), arg));
            self.codes.push(Code::Load(ano, symbol));
        }

        self.gen_comp_stmts(stmts);
        self.codes.push(Code::FuncDefineClose);
    }

    fn gen_comp_stmts(&mut self, stmts: Vec<ASTStmt>) {
        for stmt in stmts {
            match stmt.kind {
                ASTStmtKind::Assign(id, expr) => self.gen_assign(id, expr),
                ASTStmtKind::Return(expr) => self.gen_return(expr),
                _ => unimplemented!(),
            }
        }
    }

    fn gen_assign(&mut self, id: ASTIdentifier, expr: ASTExpr) {
        // Alloca(<name1>)
        // Store(<name1>, <integer>)
        // Load(<name1>, <name2>)

        let expr = self.gen_expr(expr);

        match expr.kind {
            ExpressionKind::Value(n) => {
                let ano = self.table.anonymous_symbol().to_expr();
                let symbol = self.table.register_name(id.get_name()).to_expr();
                self.codes.push(Code::Alloca(ano.clone()));
                let kind = ExpressionKind::Value(n);
                let expr = Expression::new(kind, CodeType::Int);
                self.codes.push(Code::Store(ano.clone(), expr));
                self.codes.push(Code::Load(ano, symbol));
            }
            ExpressionKind::Symbol(symbol) => {
                self.table.overwrite_name_and_symbol(id.get_name(), symbol)
            }
        }
    }

    fn gen_return(&mut self, expr: ASTExpr) {
        let ret = match expr.kind {
            ASTExprKind::Identifier(id) => self.gen_identifier(id),
            _ => self.gen_expr(expr),
        };
        self.codes.push(Code::Return(ret))
    }

    fn gen_identifier(&mut self, id: ASTIdentifier) -> Expression {
        match self.table.get_symbol(&id.get_name()) {
            Some(sym) => sym.to_expr(),
            None => unreachable!(),
        }
    }

    fn gen_expr(&mut self, expr: ASTExpr) -> Expression {
        match expr.kind {
            ASTExprKind::Integer(n) => {
                let kind = ValueKind::Int(n as i32);
                let value = Value::new(kind);
                value.to_expr()
            }
            ASTExprKind::Identifier(id) => match self.table.get_symbol(&id.get_name()) {
                Some(sym) => sym.to_expr(),
                None => unreachable!(),
            },
            ASTExprKind::FuncCall(id, args) => {
                let sym = Symbol::from_identifier(id);

                let mut syms = Vec::new();
                for arg in args {
                    let expr = self.gen_expr(arg);
                    syms.push(expr);
                }

                let assigned = self.table.anonymous_symbol().to_expr();
                let code = Code::FuncCall(sym, syms, assigned.clone());
                self.codes.push(code);
                assigned
            }
            ASTExprKind::Binary(left, right, ope) => {
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
            ASTExprKind::Unary(factor, ope) => match ope {
                OperatorKind::Minus => {
                    let expr = self.gen_expr(*factor);
                    match expr.kind {
                        ExpressionKind::Value(val) => {
                            let kind = match val.kind {
                                ValueKind::Int(n) => ValueKind::Int(-n),
                            };
                            let value = Value::new(kind);
                            value.to_expr()
                        }
                        ExpressionKind::Symbol(sym) => {
                            let ans = self.table.anonymous_symbol().to_expr();
                            let l = sym.to_expr();
                            let r = {
                                let kind = ValueKind::Int(-1);
                                let value = Value::new(kind);
                                value.to_expr()
                            };
                            let code = Code::Multi(l, r, ans.clone());
                            self.codes.push(code);
                            ans
                        }
                    }
                }
                _ => unreachable!(),
            },
        }
    }
}
