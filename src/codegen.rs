pub mod code;
pub mod expression;
pub mod symbol_table;

use super::ast::{
    ASTBlock, ASTBlockKind, ASTExpr, ASTExprKind, ASTIdentifier, ASTStmt, ASTStmtKind,
    HasSyntaxKind, AST,
};
use super::operators::Operator;
use super::types::Type;
use code::{Assembly, Code};
use expression::{AsCode, Expression, ExpressionKind, Symbol, Value, ValueKind};
use symbol_table::{FunctionTable, SymbolicTable, VariableTable};

pub struct CodeGenerator {
    ast: AST,
    code_stack: Vec<Code>,
    symbol_table: VariableTable,
    func_table: FunctionTable,
    label_count: usize,
}

impl CodeGenerator {
    pub fn new(ast: AST) -> Self {
        Self {
            ast,
            code_stack: Vec::new(),
            symbol_table: VariableTable::new(),
            func_table: FunctionTable::new(),
            label_count: 0,
        }
    }

    pub fn gen_assembly(&mut self) -> Assembly {
        self.gen_program(self.ast.clone());
        Assembly(self.code_stack.clone())
    }

    fn gen_program(&mut self, ast: AST) {
        // program -> block*

        for block in ast.program() {
            self.gen_block(block);
        }
    }

    fn gen_block(&mut self, block: ASTBlock) {
        // block -> func

        match block.kind {
            ASTBlockKind::Func(id, params, stmts) => self.gen_func(id, params, stmts),
        }
    }

    fn gen_func(&mut self, id: ASTIdentifier, params: Vec<ASTIdentifier>, stmts: Vec<ASTStmt>) {
        // func -> def-open, stmts, def-close

        let func = self.func_table.register(id);

        let mut types = Vec::new();
        for param in params.clone() {
            types.push(param.get_type());
        }
        self.code_stack
            .push(Code::FuncDefineOpen(func, types.clone()));

        for (index, param) in params.iter().enumerate() {
            let arg = Symbol::new(format!("%{}", index), param.get_type()).to_expr();
            let symbol = self.symbol_table.register(param.clone());
            self.code_stack.push(Code::Alloca(symbol.clone()));
            self.code_stack.push(Code::Store(arg, symbol));
        }

        self.gen_stmts(stmts);
        self.code_stack.push(Code::FuncDefineClose);
    }

    fn gen_stmts(&mut self, stmts: Vec<ASTStmt>) {
        // stmts -> stmt*
        // stmt -> assign | declare | dec-ass | return

        let l = stmts.len() - 1;

        for (k, stmt) in stmts.iter().enumerate() {
            use ASTStmtKind::*;

            match stmt.get_kind() {
                Assign(id, expr) => self.gen_assign(id, expr),
                Declare(id) => self.gen_declare(id),
                DeclareAssign(id, expr) => self.gen_declare_and_assign(id, expr),
                Return(expr) => self.gen_return(expr),
                If(expr, t_stmts, f_stmts) => self.gen_if(expr, t_stmts, f_stmts, k < l),
            }
        }
    }

    fn gen_declare_and_assign(&mut self, id: ASTIdentifier, expr: ASTExpr) {
        self.gen_declare(id.clone());
        self.gen_assign(id, expr);
    }

    fn gen_declare(&mut self, id: ASTIdentifier) {
        let symbol = self.symbol_table.register(id);
        self.code_stack.push(Code::Alloca(symbol));
    }

    fn gen_assign(&mut self, id: ASTIdentifier, expr: ASTExpr) {
        let expr = self.gen_expr(expr);
        let symbol = self.symbol_table.search_symbol(&id.get_name()).unwrap();

        self.code_stack.push(Code::Store(expr, symbol));
    }

    fn gen_return(&mut self, expr: ASTExpr) {
        let ret = match expr.kind {
            ASTExprKind::Identifier(id) => self.gen_identifier(id),
            _ => self.gen_expr(expr),
        };
        self.code_stack.push(Code::Return(ret))
    }

    fn gen_if(
        &mut self,
        cond: ASTExpr,
        t_stmts: Vec<ASTStmt>,
        f_stmts: Vec<ASTStmt>,
        has_continue: bool,
    ) {
        let t_label = format!("true_label_{}", self.label_count);
        let f_label = format!("false_label_{}", self.label_count);
        let continue_label = format!("continue_label_{}", self.label_count);
        let jump_code = Code::Jump(continue_label.clone());
        self.label_count += 1;

        // Compareを生成
        use ASTExprKind::*;
        let ans = self.symbol_table.anonymous_symbol(Type::int()); // TODO: LLVM IRとC言語の間で型にギャップが起こる.
        let cmp_code = match cond.get_kind() {
            Binary(l, r, ope) => {
                let l_expr = self.gen_expr(*l);
                let r_expr = self.gen_expr(*r);
                Code::Condition(l_expr, r_expr, ope, ans.clone())
            }
            _ => {
                let cond_expr = self.gen_expr(cond);
                let zero_expr = Value::new(ValueKind::Int(0), Type::int()).to_expr();
                Code::Condition(cond_expr, zero_expr, Operator::NotEqual, ans.clone())
            }
        };

        self.code_stack.push(cmp_code);

        // 分岐を生成
        let branch_code = Code::Branch(ans.to_expr(), t_label.clone(), f_label.clone());
        self.code_stack.push(branch_code);

        // true-labelを生成
        self.code_stack.push(Code::Label(t_label));
        self.gen_stmts(t_stmts);
        let jump_cond = !self
            .code_stack
            .last()
            .map_or(false, |last| last.is_return());
        if jump_cond {
            self.code_stack.push(jump_code.clone());
        }

        // false-labelを生成
        self.code_stack.push(Code::Label(f_label));
        self.gen_stmts(f_stmts);
        let jump_cond = !self
            .code_stack
            .last()
            .map_or(false, |last| last.is_return());
        if jump_cond {
            self.code_stack.push(jump_code);
        }

        // 後続部分を生成
        if has_continue {
            self.code_stack.push(Code::Label(continue_label));
        }
    }

    fn gen_expr(&mut self, expr: ASTExpr) -> Expression {
        use ASTExprKind::*;

        match expr.kind {
            Integer(n) => self.gen_integer(n),
            Identifier(id) => self.gen_identifier(id),
            FuncCall(id, args) => self.gen_func_call(id, args),
            Binary(l, r, ope) => self.gen_binary(*l, *r, ope),
            Unary(factor, ope) => self.gen_unary(*factor, ope),
        }
    }

    fn gen_integer(&mut self, n: u32) -> Expression {
        let kind = ValueKind::Int(n as i32);
        let value = Value::new(kind, Type::int());
        value.to_expr()
    }

    fn gen_identifier(&mut self, id: ASTIdentifier) -> Expression {
        match self.symbol_table.search_symbol(&id.get_name()) {
            Some(sym) => {
                let ano = self.symbol_table.anonymous_symbol(sym.get_type());
                self.code_stack.push(Code::Load(sym, ano.clone()));
                ano.to_expr()
            }
            None => unreachable!(),
        }
    }

    fn gen_func_call(&mut self, id: ASTIdentifier, args: Vec<ASTExpr>) -> Expression {
        let sym = self.func_table.search_symbol(&id.get_name()).unwrap();

        let mut syms = Vec::new();
        for arg in args {
            let expr = self.gen_expr(arg);
            syms.push(expr);
        }

        let assigned = self.symbol_table.anonymous_symbol(sym.get_type());
        let code = Code::FuncCall(sym, syms, assigned.clone());
        self.code_stack.push(code);
        assigned.to_expr()
    }

    fn gen_binary(&mut self, l: ASTExpr, r: ASTExpr, ope: Operator) -> Expression {
        use Operator::*;

        let l = self.gen_expr(l);
        let r = self.gen_expr(r);
        let ty = l.get_type();
        let ans = self.symbol_table.anonymous_symbol(ty);
        let code = match ope {
            Plus | Minus | Times | Devide => Code::Arithmetic(l, r, ope, ans.clone()),
            Equal | NotEqual => Code::Condition(l, r, ope, ans.clone()),
        };
        self.code_stack.push(code);
        ans.to_expr()
    }

    fn gen_unary(&mut self, factor: ASTExpr, ope: Operator) -> Expression {
        match ope {
            Operator::Minus => {
                let expr = self.gen_expr(factor);
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
                        let code = Code::Arithmetic(l, r, Operator::Times, ans.clone());
                        self.code_stack.push(code);
                        ans.to_expr()
                    }
                }
            }
            _ => unreachable!(),
        }
    }
}
