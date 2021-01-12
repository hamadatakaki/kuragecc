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
                self.var_manager.memory_scope();
                for param in params.clone() {
                    match param.kind {
                        ASTKind::Identifier(name) => {
                            let info = IdentifierInformation::new(name, 0, ast.scope + 1);
                            self.var_manager.push_info(info);
                        }
                        _ => unreachable!(),
                    }
                }

                let info = IdentifierInformation::new(name, params.len(), ast.scope);
                self.func_manager.push_info(info);
                self.semantic_analyze_block(*block);
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
                self.var_manager.down_scope();
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
                let info = IdentifierInformation::new(name, 0, ast.scope);
                self.var_manager.push_info(info);
                self.semantic_analyze_expr(*expr)
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
                if self.var_manager.search_name(&name).is_none() {
                    let kind = SemanticErrorKind::IdentifierIsNotDeclared(name);
                    let error = SemanticError::new(kind, ast.location);
                    self.errors.push(error)
                }
            }
            ASTKind::FuncCall { name, args } => match self.func_manager.search_name(&name) {
                Some(info) => {
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
