pub mod identifier;

use super::ast::types::Type;
use super::ast::{
    ASTBlock, ASTBlockKind, ASTExpr, ASTExprKind, ASTIdentifier, ASTStmt, ASTStmtKind, PartialAST,
    AST,
};
use super::error::semantics::{SemanticError, SemanticErrorKind, SemanticResult};
use identifier::{IdentifierInformation, IdentifierManager};

pub struct SemanticAnalyzer {
    var_manager: IdentifierManager,
    func_manager: IdentifierManager,
    errors: Vec<SemanticError>,
}

impl SemanticAnalyzer {
    pub fn new() -> Self {
        Self {
            var_manager: IdentifierManager::new(),
            func_manager: IdentifierManager::new(),
            errors: Vec::new(),
        }
    }

    pub fn semantic_analyze(&mut self, ast: AST) -> SemanticResult<()> {
        self.semantic_analyze_program(ast);
        if self.errors.is_empty() {
            Ok(())
        } else {
            Err(self.errors.clone())
        }
    }

    fn semantic_analyze_program(&mut self, ast: AST) {
        for block in ast.program {
            self.semantic_analyze_block(block);
        }
    }

    fn semantic_analyze_block(&mut self, block: ASTBlock) {
        match block.kind {
            ASTBlockKind::Func(id, params, stmts) => self.semantic_analyze_func(id, params, stmts),
        }
    }

    fn semantic_analyze_func(
        &mut self,
        id: ASTIdentifier,
        params: Vec<ASTIdentifier>,
        stmts: Vec<ASTStmt>,
    ) {
        // 関数名のスコープを記憶
        let info = IdentifierInformation::new(id, params.len());
        self.func_manager.push_info(info);

        // スコープを一段階深くし、関数引数のスコープを記憶
        self.var_manager.deepen_scope();
        for param in params {
            let info = IdentifierInformation::new(param, 0);
            self.var_manager.push_info(info);
        }

        // 関数定義内の解析
        self.semantic_analyze_comp_stmts(stmts.clone());

        // 関数のスコープから抜ける処理
        self.var_manager.shallow_scope();
    }

    fn semantic_analyze_comp_stmts(&mut self, stmts: Vec<ASTStmt>) {
        for stmt in stmts {
            self.semantic_analyze_stmt(stmt);
        }
    }

    fn semantic_analyze_stmt(&mut self, stmt: ASTStmt) {
        use ASTStmtKind::*;

        match stmt.kind {
            Assign(id, expr) => self.semantic_analyze_assign(id, expr),
            Declare(id) => self.semantic_analyze_declare(id),
            DeclareAssign(id, expr) => self.semantic_analyze_declare_and_assign(id, expr),
            Return(expr) => self.semantic_analyze_return(expr),
            If(expr, t_stmts, f_stmts) => self.semantic_analyze_if(expr, t_stmts, f_stmts),
        }
    }

    fn semantic_analyze_assign(&mut self, id: ASTIdentifier, expr: ASTExpr) {
        // exprを解析
        self.semantic_analyze_expr(expr);

        // 該当の変数がなければIdentifierIsNotDeclaredを吐く
        if self.var_manager.search_name(&id).is_none() {
            let kind = SemanticErrorKind::IdentifierIsNotDeclared(id.get_name());
            let error = SemanticError::new(kind, id.get_loc());
            self.errors.push(error)
        }
    }

    fn semantic_analyze_declare(&mut self, id: ASTIdentifier) {
        // 新たな変数名を追加
        let info = IdentifierInformation::new(id, 0);
        self.var_manager.push_info(info);
    }

    fn semantic_analyze_declare_and_assign(&mut self, id: ASTIdentifier, expr: ASTExpr) {
        // exprを解析
        self.semantic_analyze_expr(expr.clone());

        // type-check
        let id_type = id.get_type();
        let expr_type = expr.get_type();
        if id_type != expr_type {
            let kind = SemanticErrorKind::TypesAreDifferent(id_type, expr_type);
            let error = SemanticError::new(kind, id.get_loc());
            self.errors.push(error)
        }

        // 新たな変数名を追加
        let info = IdentifierInformation::new(id, 0);
        self.var_manager.push_info(info);
    }

    fn semantic_analyze_return(&mut self, expr: ASTExpr) {
        self.semantic_analyze_expr(expr)
    }

    fn semantic_analyze_if(&mut self, cond: ASTExpr, t_stmts: Vec<ASTStmt>, f_stmts: Vec<ASTStmt>) {
        let cond_type = cond.get_type();
        // cond の型は int であるべき.
        if cond_type != Type::int() {
            let kind = SemanticErrorKind::TypesAreDifferent(cond_type, Type::int());
            let error = SemanticError::new(kind, cond.get_loc());
            self.errors.push(error)
        }

        self.semantic_analyze_comp_stmts(t_stmts);
        self.semantic_analyze_comp_stmts(f_stmts);
    }

    fn semantic_analyze_expr(&mut self, expr: ASTExpr) {
        use ASTExprKind::*;

        match expr.clone().kind {
            Binary(left, right, _) => {
                self.semantic_analyze_expr(*left);
                self.semantic_analyze_expr(*right);
            }
            Unary(factor, _) => {
                self.semantic_analyze_expr(*factor);
            }
            Integer(_) => {}
            Identifier(id) => {
                // 該当の変数がなければIdentifierIsNotDeclaredを吐く
                if self.var_manager.search_name(&id).is_none() {
                    let kind = SemanticErrorKind::IdentifierIsNotDeclared(id.get_name());
                    let error = SemanticError::new(kind, expr.get_loc());
                    self.errors.push(error)
                }
            }
            FuncCall(id, args) => {
                match self.func_manager.search_name(&id) {
                    Some(info) => {
                        // 引数の数をチェック
                        if info.param_size != args.len() {
                            let kind = SemanticErrorKind::DifferentNumbersArgsTaken(
                                id.get_name(),
                                args.len(),
                                info.param_size,
                            );
                            let error = SemanticError::new(kind, expr.get_loc());
                            self.errors.push(error)
                        }
                    }
                    None => {
                        let kind = SemanticErrorKind::FunctionIsNotDefined(id.get_name());
                        let error = SemanticError::new(kind, expr.get_loc());
                        self.errors.push(error)
                    }
                }
            }
        }
    }
}
