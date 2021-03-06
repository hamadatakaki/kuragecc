pub mod function;
pub mod identifier;

use super::ast::{
    ASTBlock, ASTBlockKind, ASTExpr, ASTExprKind, ASTIdentifier, ASTStmt, ASTStmtKind,
    AsSyntaxExpression, AsSyntaxStatement, HasSyntaxKind, AST,
};
use super::error::semantics::{SemanticError, SemanticErrorKind, SemanticResult};
use super::operators::Operator;
use super::types::Type;
use function::{FunctionInformation, FunctionManager};
use identifier::IdentifierManager;

macro_rules! type_check {
    ($actual_type: expr, $expected_type: expr) => {
        if $actual_type != $expected_type {
            let kind = SemanticErrorKind::TypeIsDifferent($actual_type, $expected_type);
            Some(kind)
        } else {
            None
        }
    };
}

pub struct SemanticAnalyzer {
    var_manager: IdentifierManager,
    func_manager: FunctionManager,
    errors: Vec<SemanticError>,
}

impl SemanticAnalyzer {
    pub fn new() -> Self {
        Self {
            var_manager: IdentifierManager::new(),
            func_manager: FunctionManager::new(),
            errors: Vec::new(),
        }
    }

    fn declare_new_identifier(&mut self, id: ASTIdentifier) -> ASTIdentifier {
        let info = self.var_manager.new_info(id.clone());
        self.var_manager.push_info(info.clone());
        ASTIdentifier::new(info.verbose_name(), id.get_type(), id.get_loc())
    }

    pub fn semantic_analyze(&mut self, ast: AST) -> SemanticResult<AST> {
        let ast = self.semantic_analyze_program(ast);
        if self.errors.is_empty() {
            Ok(ast)
        } else {
            Err(self.errors.clone())
        }
    }

    fn semantic_analyze_program(&mut self, ast: AST) -> AST {
        let mut new_blocks = vec![];

        for block in ast.program() {
            let new_block = self.semantic_analyze_block(block);
            new_blocks.push(new_block);
        }

        AST::new(new_blocks, ast.get_scope(), ast.get_loc())
    }

    fn semantic_analyze_block(&mut self, block: ASTBlock) -> ASTBlock {
        let kind = match block.get_kind() {
            ASTBlockKind::Func(id, params, stmts) => self.semantic_analyze_func(id, params, stmts),
        };
        ASTBlock::new(kind, block.get_scope(), block.get_loc())
    }

    fn semantic_analyze_func(
        &mut self,
        id: ASTIdentifier,
        params: Vec<ASTIdentifier>,
        stmts: Vec<ASTStmt>,
    ) -> ASTBlockKind {
        // スコープを一段階深くし、関数引数のスコープを記憶
        self.var_manager.deepen_scope();

        let mut new_params = vec![];

        for param in params {
            let new_id = self.declare_new_identifier(param);
            new_params.push(new_id);
        }

        // 関数名のスコープを記憶
        let param_def = new_params
            .iter()
            .map(|param| param.get_type())
            .collect::<Vec<Type>>();
        let info = FunctionInformation::new(id.clone(), param_def);
        self.func_manager.push_info(info);

        // 関数定義内の解析
        let new_stmts = self.semantic_analyze_comp_stmts(stmts, id.get_type());

        // 関数のスコープから抜ける処理
        self.var_manager.shallow_scope();

        ASTBlockKind::Func(id, new_params, new_stmts)
    }

    fn semantic_analyze_comp_stmts(&mut self, stmts: Vec<ASTStmt>, ret_ty: Type) -> Vec<ASTStmt> {
        let mut new_stmts = vec![];

        for stmt in stmts {
            let analyzed_stmt = self.semantic_analyze_stmt(stmt, ret_ty.clone());
            new_stmts.push(analyzed_stmt);
        }

        new_stmts
    }

    fn semantic_analyze_stmt(&mut self, stmt: ASTStmt, ret_ty: Type) -> ASTStmt {
        use ASTStmtKind::*;

        let kind = match stmt.get_kind() {
            Assign(id, expr) => self.semantic_analyze_assign(id, expr),
            Declare(id) => self.semantic_analyze_declare(id),
            DeclareAssign(id, expr) => self.semantic_analyze_declare_and_assign(id, expr),
            Return(expr) => self.semantic_analyze_return(expr, ret_ty),
            If(cond, t_stmts, f_stmts) => self.semantic_analyze_if(cond, t_stmts, f_stmts, ret_ty),
            While(cond, stmts) => self.semantic_analyze_while(cond, stmts, ret_ty),
        };

        ASTStmt::new(kind, stmt.get_scope(), stmt.get_loc())
    }

    fn semantic_analyze_assign(&mut self, id: ASTIdentifier, expr: ASTExpr) -> ASTStmtKind {
        // exprを解析
        let (new_expr, expr_type) = self.semantic_analyze_expr(expr.clone());

        // idに該当する変数が宣言されているかを探索
        match self.var_manager.search_name(&id) {
            Some(info) => {
                // 該当の変数があればtype-check
                type_check!(info.get_type(), expr_type).map(|kind| {
                    let e = SemanticError::new(kind, id.get_loc());
                    self.errors.push(e);
                });

                let new_id = ASTIdentifier::new(info.verbose_name(), id.get_type(), id.get_loc());
                ASTStmtKind::Assign(new_id, new_expr)
            }
            None => {
                // 該当の変数がなければIdentifierIsNotDeclaredを吐く
                let kind = SemanticErrorKind::IdentifierIsNotDeclared(id.get_name());
                let error = SemanticError::new(kind, id.get_loc());
                self.errors.push(error);
                ASTStmtKind::Assign(id, new_expr)
            }
        }
    }

    fn semantic_analyze_declare(&mut self, id: ASTIdentifier) -> ASTStmtKind {
        ASTStmtKind::Declare(self.declare_new_identifier(id))
    }

    fn semantic_analyze_declare_and_assign(
        &mut self,
        id: ASTIdentifier,
        expr: ASTExpr,
    ) -> ASTStmtKind {
        // exprを解析
        let (new_expr, expr_type) = self.semantic_analyze_expr(expr.clone());

        // type-check
        type_check!(id.get_type(), expr_type).map(|kind| {
            let e = SemanticError::new(kind, id.get_loc());
            self.errors.push(e);
        });

        // idを解析
        let new_id = self.declare_new_identifier(id);
        ASTStmtKind::DeclareAssign(new_id, new_expr)
    }

    fn semantic_analyze_return(&mut self, expr: ASTExpr, ret_ty: Type) -> ASTStmtKind {
        let (new_expr, expr_type) = self.semantic_analyze_expr(expr.clone());

        // type-check
        type_check!(ret_ty, expr_type).map(|kind| {
            let e = SemanticError::new(kind, new_expr.get_loc());
            self.errors.push(e);
        });

        ASTStmtKind::Return(new_expr)
    }

    fn semantic_analyze_if(
        &mut self,
        cond: ASTExpr,
        t_stmts: Vec<ASTStmt>,
        f_stmts: Vec<ASTStmt>,
        ret_ty: Type,
    ) -> ASTStmtKind {
        let (new_cond, cond_type) = self.semantic_analyze_expr(cond.clone());

        let new_cond = match new_cond.get_kind() {
            ASTExprKind::Binary(_, _, ope) if ope.is_equivalence() => new_cond,
            _ => {
                let zero = ASTExpr::new(ASTExprKind::Integer(0), new_cond.get_loc());
                let kind = ASTExprKind::Binary(
                    Box::new(new_cond.clone()),
                    Box::new(zero),
                    Operator::NotEqual,
                );
                ASTExpr::new(kind, new_cond.get_loc())
            }
        };

        // cond の型は int であるべき.
        type_check!(cond_type, Type::int()).map(|kind| {
            let e = SemanticError::new(kind, cond.get_loc());
            self.errors.push(e);
        });

        // t_stmtsの解析
        self.var_manager.deepen_scope();
        let new_t_stmts = self.semantic_analyze_comp_stmts(t_stmts, ret_ty.clone());
        self.var_manager.shallow_scope();

        // f_stmtsの解析
        self.var_manager.deepen_scope();
        let new_f_stmts = self.semantic_analyze_comp_stmts(f_stmts, ret_ty);
        self.var_manager.shallow_scope();

        ASTStmtKind::If(new_cond, new_t_stmts, new_f_stmts)
    }

    fn semantic_analyze_while(
        &mut self,
        cond: ASTExpr,
        stmts: Vec<ASTStmt>,
        ret_ty: Type,
    ) -> ASTStmtKind {
        let (new_cond, cond_type) = self.semantic_analyze_expr(cond.clone());

        let new_cond = match new_cond.get_kind() {
            ASTExprKind::Binary(_, _, ope) if ope.is_equivalence() => new_cond,
            _ => {
                let zero = ASTExpr::new(ASTExprKind::Integer(0), new_cond.get_loc());
                let kind = ASTExprKind::Binary(
                    Box::new(new_cond.clone()),
                    Box::new(zero),
                    Operator::NotEqual,
                );
                ASTExpr::new(kind, new_cond.get_loc())
            }
        };

        // cond の型は int であるべき.
        type_check!(cond_type, Type::int()).map(|kind| {
            let e = SemanticError::new(kind, cond.get_loc());
            self.errors.push(e);
        });

        // stmtsの解析
        self.var_manager.deepen_scope();
        let new_stmts = self.semantic_analyze_comp_stmts(stmts, ret_ty.clone());
        self.var_manager.shallow_scope();

        ASTStmtKind::While(new_cond, new_stmts)
    }

    // expression

    fn semantic_analyze_expr(&mut self, expr: ASTExpr) -> (ASTExpr, Type) {
        use ASTExprKind::*;

        let (kind, ty) = match expr.get_kind() {
            Binary(left, right, ope) => self.semantic_analyze_binary(*left, *right, ope),
            Unary(factor, ope) => self.semantic_analyze_unary(*factor, ope),
            Integer(_) => (expr.get_kind(), Type::int()),
            Identifier(id) => self.semantic_analyze_identifier(id),
            FuncCall(id, args) => self.semantic_analyze_func_call(id, args),
        };

        let new_expr = ASTExpr::new(kind, expr.get_loc());
        (new_expr, ty)
    }

    fn semantic_analyze_binary(
        &mut self,
        left: ASTExpr,
        right: ASTExpr,
        ope: Operator,
    ) -> (ASTExprKind, Type) {
        let (new_left, left_type) = self.semantic_analyze_expr(left.clone());
        let (new_right, right_type) = self.semantic_analyze_expr(right.clone());

        let ty = if ope.type_check_binary(left_type.clone(), right_type.clone()) {
            left_type
        } else {
            let err_kind = SemanticErrorKind::TypeIsDifferent(left_type, right_type);
            let loc = new_left.get_loc().extend_to(new_right.get_loc());
            let err = SemanticError::new(err_kind, loc);
            self.errors.push(err);

            Type::IgnoreType
        };

        let kind = ASTExprKind::Binary(Box::new(new_left), Box::new(new_right), ope);
        (kind, ty)
    }

    fn semantic_analyze_unary(&mut self, factor: ASTExpr, ope: Operator) -> (ASTExprKind, Type) {
        let (new_factor, factor_type) = self.semantic_analyze_expr(factor);

        let ty = if ope.type_check_unary(factor_type.clone()) {
            factor_type
        } else {
            let kind = SemanticErrorKind::TypeIsDifferent(factor_type, Type::int());
            let err = SemanticError::new(kind, new_factor.get_loc());
            self.errors.push(err);

            Type::IgnoreType
        };

        let kind = ASTExprKind::Unary(Box::new(new_factor), ope);
        (kind, ty)
    }

    fn semantic_analyze_identifier(&mut self, id: ASTIdentifier) -> (ASTExprKind, Type) {
        // 該当の変数がなければIdentifierIsNotDeclaredを吐く
        let search_result = self.var_manager.search_name(&id);
        let (id, ty) = match search_result {
            None => {
                let kind = SemanticErrorKind::IdentifierIsNotDeclared(id.get_name());
                let error = SemanticError::new(kind, id.get_loc());
                self.errors.push(error);
                (id, Type::IgnoreType)
            }
            Some(info) => {
                let new_name = info.verbose_name();
                let new_id = ASTIdentifier::new(new_name, info.get_type(), id.get_loc());
                (new_id, info.get_type())
            }
        };

        (ASTExprKind::Identifier(id), ty)
    }

    fn semantic_analyze_func_call(
        &mut self,
        id: ASTIdentifier,
        args: Vec<ASTExpr>,
    ) -> (ASTExprKind, Type) {
        match self.func_manager.search_name(&id) {
            Some(info) => {
                // 引数の数をチェック
                let size = info.param_size();
                let mut new_args: Vec<ASTExpr>;
                if size == args.len() {
                    new_args = vec![];

                    // 引数型のチェック
                    for (param_type, arg) in info.param_def.iter().zip(args) {
                        let param_type = param_type.clone();
                        let (new_arg, arg_type) = self.semantic_analyze_expr(arg);

                        type_check!(arg_type, param_type).map(|kind| {
                            let e = SemanticError::new(kind, id.get_loc());
                            self.errors.push(e);
                        });

                        new_args.push(new_arg);
                    }
                } else {
                    let kind = SemanticErrorKind::DifferentNumbersArgsTaken(
                        id.get_name(),
                        args.len(),
                        size,
                    );
                    let error = SemanticError::new(kind, id.get_loc());
                    self.errors.push(error);

                    new_args = args;
                };

                let kind = ASTExprKind::FuncCall(id, new_args);
                (kind, info.get_type())
            }
            None => {
                let kind = SemanticErrorKind::FunctionIsNotDefined(id.get_name());
                let error = SemanticError::new(kind, id.get_loc());
                self.errors.push(error);
                (ASTExprKind::FuncCall(id, args), Type::IgnoreType)
            }
        }
    }
}
