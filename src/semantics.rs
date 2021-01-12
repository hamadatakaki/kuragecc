pub mod identifier;

use super::ast::{ASTKind, AST};
use super::error::{SemanticError, SemanticErrorKind, SemanticResult};
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
        match ast.kind {
            ASTKind::Program(asts) => {
                for ast in asts {
                    self.semantic_analyze_func(ast);
                }
            }
            _ => unreachable!(),
        }
    }

    fn semantic_analyze_func(&mut self, ast: AST) {
        match ast.kind {
            ASTKind::Func {
                name,
                params,
                block,
            } => {
                // 関数名のスコープを記憶
                let info = IdentifierInformation::new(name, params.len(), ast.scope);
                self.func_manager.push_info(info);

                // スコープを一段階深くし、関数引数のスコープを記憶
                self.var_manager.memory_scope();
                for param in params {
                    match param.kind {
                        ASTKind::Identifier(name) => {
                            let info = IdentifierInformation::new(name, 0, param.scope);
                            self.var_manager.push_info(info);
                        }
                        _ => unreachable!(),
                    }
                }

                // 関数定義内の解析
                self.semantic_analyze_block(*block);

                // 関数のスコープから抜ける処理
                self.var_manager.down_scope();
            }
            _ => unreachable!(),
        }
    }

    fn semantic_analyze_block(&mut self, ast: AST) {
        match ast.kind {
            ASTKind::Block(lines) => {
                for line in lines {
                    self.semantic_analyze_stmt(line);
                }
            }
            _ => unreachable!(),
        }
    }

    fn semantic_analyze_stmt(&mut self, ast: AST) {
        match ast.clone().kind {
            ASTKind::Assign(_, __) => self.semantic_analyze_assign(ast),
            ASTKind::Return(_) => self.semantic_analyze_return(ast),
            _ => unreachable!(),
        }
    }

    fn semantic_analyze_assign(&mut self, ast: AST) {
        match ast.kind {
            ASTKind::Assign(name, expr) => {
                // exprを解析
                self.semantic_analyze_expr(*expr);

                // 新たな変数名を追加
                let info = IdentifierInformation::new(name, 0, ast.scope);
                self.var_manager.push_info(info);
            }
            _ => unreachable!(),
        }
    }

    fn semantic_analyze_return(&mut self, ast: AST) {
        match ast.kind {
            ASTKind::Return(expr) => self.semantic_analyze_expr(*expr),
            _ => unreachable!(),
        }
    }

    fn semantic_analyze_expr(&mut self, ast: AST) {
        match ast.kind {
            ASTKind::Binary(left, right, _) => {
                self.semantic_analyze_expr(*left);
                self.semantic_analyze_expr(*right);
            }
            ASTKind::Unary(factor, _) => {
                self.semantic_analyze_expr(*factor);
            }
            ASTKind::Integer(_) => {}
            ASTKind::Identifier(name) => {
                // 該当の変数がなければIdentifierIsNotDeclaredを吐く
                if self.var_manager.search_name(&name).is_none() {
                    let kind = SemanticErrorKind::IdentifierIsNotDeclared(name);
                    let error = SemanticError::new(kind, ast.location);
                    self.errors.push(error)
                }
            }
            ASTKind::FuncCall { name, args } => match self.func_manager.search_name(&name) {
                Some(info) => {
                    // 引数の数をチェック
                    if !info.equal_param_size(args.len()) {
                        let kind = SemanticErrorKind::DifferentNumbersArgsTaken(
                            name,
                            args.len(),
                            info.param_size,
                        );
                        let error = SemanticError::new(kind, ast.location);
                        self.errors.push(error)
                    }
                }
                None => {
                    let kind = SemanticErrorKind::FunctionIsNotDefined(name);
                    let error = SemanticError::new(kind, ast.location);
                    self.errors.push(error)
                }
            },
            _ => unreachable!(),
        }
    }
}
