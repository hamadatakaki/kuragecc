pub mod code;
pub mod expression;
pub mod symbol_table;

use super::ast::types::Type;
use super::ast::{
    ASTBlock, ASTBlockKind, ASTExpr, ASTExprKind, ASTIdentifier, ASTStmt, ASTStmtKind, AST,
};
use super::token::literal::OperatorKind;
use code::Code;
use expression::{AsCode, Expression, ExpressionKind, Symbol, Value, ValueKind};
use symbol_table::{FunctionTable, SymbolicTable, VariableTable};

pub struct CodeGenerator {
    ast: AST,
    codes: Vec<Code>,
    symbol_table: VariableTable,
    func_table: FunctionTable,
}

impl CodeGenerator {
    pub fn new(ast: AST) -> Self {
        Self {
            ast,
            codes: Vec::new(),
            symbol_table: VariableTable::new(),
            func_table: FunctionTable::new(),
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
        let func = self.func_table.register(id);

        let mut types = Vec::new();
        for param in params.clone() {
            types.push(param.get_type());
        }
        self.codes.push(Code::FuncDefineOpen(func, types.clone()));

        for (index, param) in params.iter().enumerate() {
            let arg = Symbol::new(format!("%{}", index), param.get_type()).to_expr();
            let ano = self.symbol_table.anonymous_symbol(param.get_type());
            let symbol = self.symbol_table.register(param.clone());
            self.codes.push(Code::Alloca(ano.clone()));
            self.codes.push(Code::Store(ano.clone(), arg));
            self.codes.push(Code::Load(ano, symbol));
        }

        self.gen_comp_stmts(stmts);
        self.codes.push(Code::FuncDefineClose);
    }

    fn gen_comp_stmts(&mut self, stmts: Vec<ASTStmt>) {
        for stmt in stmts {
            use ASTStmtKind::*;

            match stmt.kind {
                Assign(id, expr) => self.gen_assign(id, expr),
                Declare(id) => self.gen_declare(id),
                DeclareAssign(id, expr) => self.gen_declare_and_assign(id, expr),
                Return(expr) => self.gen_return(expr),
            }
        }
    }

    fn gen_declare_and_assign(&mut self, id: ASTIdentifier, expr: ASTExpr) {
        // Alloca(<name1>)
        // Store(<name1>, <integer>)
        // Load(<name1>, <name2>)

        let expr = self.gen_expr(expr);

        match expr.clone().kind {
            ExpressionKind::Value(val) => {
                let ano = self.symbol_table.anonymous_symbol(val.get_type());
                let symbol = self.symbol_table.register(id);
                self.codes.push(Code::Alloca(ano.clone()));
                self.codes.push(Code::Store(ano.clone(), expr));
                self.codes.push(Code::Load(ano, symbol));
            }
            ExpressionKind::Symbol(symbol) => self
                .symbol_table
                .overwrite_name_and_symbol(id.get_name(), symbol),
        }
    }

    fn gen_declare(&mut self, id: ASTIdentifier) {
        self.symbol_table.register(id);
    }

    fn gen_assign(&mut self, id: ASTIdentifier, expr: ASTExpr) {
        // Alloca(<name1>)
        // Store(<name1>, <integer>)
        // Load(<name1>, <name2>)

        let expr = self.gen_expr(expr);

        match expr.clone().kind {
            ExpressionKind::Value(val) => {
                let ano = self.symbol_table.anonymous_symbol(val.get_type());
                let symbol = self.symbol_table.search_symbol(&id.get_name()).unwrap();
                self.codes.push(Code::Alloca(ano.clone()));
                self.codes.push(Code::Store(ano.clone(), expr));
                self.codes.push(Code::Load(ano, symbol));
            }
            ExpressionKind::Symbol(symbol) => self
                .symbol_table
                .overwrite_name_and_symbol(id.get_name(), symbol),
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
        match self.symbol_table.search_symbol(&id.get_name()) {
            Some(sym) => sym.to_expr(),
            None => unreachable!(),
        }
    }

    fn gen_expr(&mut self, expr: ASTExpr) -> Expression {
        use ASTExprKind::*;

        match expr.kind {
            Integer(n) => {
                let kind = ValueKind::Int(n as i32);
                let value = Value::new(kind, Type::int());
                value.to_expr()
            }
            Identifier(id) => self.gen_identifier(id),
            FuncCall(id, args) => {
                let sym = self.func_table.search_symbol(&id.get_name()).unwrap();

                let mut syms = Vec::new();
                for arg in args {
                    let expr = self.gen_expr(arg);
                    syms.push(expr);
                }

                let assigned = self.symbol_table.anonymous_symbol(sym.get_type());
                let code = Code::FuncCall(sym, syms, assigned.clone());
                self.codes.push(code);
                assigned.to_expr()
            }
            Binary(left, right, ope) => {
                let l = self.gen_expr(*left);
                let r = self.gen_expr(*right);
                let ty = l.get_type();
                let ans = self.symbol_table.anonymous_symbol(ty);
                let code = match ope {
                    OperatorKind::Plus => Code::Add(l, r, ans.clone()),
                    OperatorKind::Minus => Code::Sub(l, r, ans.clone()),
                    OperatorKind::Times => Code::Multi(l, r, ans.clone()),
                    OperatorKind::Devide => Code::Divide(l, r, ans.clone()),
                    _ => unreachable!(),
                };
                self.codes.push(code);
                ans.to_expr()
            }
            Unary(factor, ope) => match ope {
                OperatorKind::Minus => {
                    let expr = self.gen_expr(*factor);
                    match expr.kind {
                        ExpressionKind::Value(val) => {
                            let kind = match val.kind {
                                ValueKind::Int(n) => ValueKind::Int(-n),
                            };
                            let value = Value::new(kind, val.get_type());
                            value.to_expr()
                        }
                        ExpressionKind::Symbol(sym) => {
                            let ans = self.symbol_table.anonymous_symbol(sym.get_type());
                            let l = sym.to_expr();
                            let r = {
                                let kind = ValueKind::Int(-1);
                                let value = Value::new(kind, Type::int());
                                value.to_expr()
                            };
                            let code = Code::Multi(l, r, ans.clone());
                            self.codes.push(code);
                            ans.to_expr()
                        }
                    }
                }
                _ => unreachable!(),
            },
        }
    }
}
