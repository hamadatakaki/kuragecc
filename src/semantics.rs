use super::ast::{ASTKind, AST};
use super::error::{SemanticError, SemanticErrorKind, SemanticResult};
use super::identifier::IdentifierManager;

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
        self.semantic_analyze_block(ast);
        if self.errors.is_empty() {
            Ok(())
        } else {
            Err(self.errors.clone())
        }
    }

    fn semantic_analyze_block(&mut self, ast: AST) {
        match ast.kind {
            ASTKind::Block(lines) => {
                for (i, line) in lines.iter().enumerate() {
                    match line.kind {
                        ASTKind::Return(_) => {
                            self.semantic_analyze_return(line.clone());
                            if i < lines.len() - 1 {
                                let start = line.location;
                                let end = lines.last().unwrap().location;
                                let loc = start.extend_to(end);
                                let kind = SemanticErrorKind::BlockMustEndAtFirstReturn;
                                let error = SemanticError::new(kind, loc);
                                self.errors.push(error);
                            }
                            break;
                        }
                        _ => self.semantic_analyze_stmt(line.clone()),
                    }
                }
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

    fn semantic_analyze_stmt(&mut self, ast: AST) {
        self.semantic_analyze_assign(ast);
    }

    fn semantic_analyze_assign(&mut self, ast: AST) {
        match ast.kind {
            ASTKind::Assign(name, expr) => {
                self.id_manager.set_name(name);
                self.semantic_analyze_expr(*expr)
            }
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
