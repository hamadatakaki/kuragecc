use super::ast::{ASTKind, AST};
use super::error::{SemanticError, SemanticErrorKind, SemanticResult};
use super::identifier::{IdentifierInformation, IdentifierKind, IdentifierManager};

pub struct SemanticAnalyzer {
    id_manager: IdentifierManager,
    errors: Vec<SemanticError>,
}

impl SemanticAnalyzer {
    pub fn new() -> Self {
        Self {
            id_manager: IdentifierManager::new(),
            errors: Vec::new(),
        }
    }

    pub fn semantic_analyze(&mut self, ast: AST) -> SemanticResult<()> {
        self.semantic_analyze_func(ast);
        if self.errors.is_empty() {
            Ok(())
        } else {
            Err(self.errors.clone())
        }
    }

    fn semantic_analyze_func(&mut self, ast: AST) {
        match ast.kind {
            ASTKind::Func(_, block) => self.semantic_analyze_block(*block),
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
                let info = IdentifierInformation::new(IdentifierKind::Variable, ast.scope);
                self.id_manager.set_name(name, info);
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
                if self.id_manager.get_name(&name).is_none() {
                    let kind = SemanticErrorKind::IdentifierIsNotDeclared(name);
                    let error = SemanticError::new(kind, ast.location);
                    self.errors.push(error)
                }
            }
            _ => unreachable!(),
        }
    }
}
