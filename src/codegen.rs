use super::ast::{ASTKind, AST};

pub struct CodeGenerator {
    ast: AST,
    pub code: String,
}

impl CodeGenerator {
    fn new(ast: AST) -> Self {
        Self {
            ast,
            code: String::new(),
        }
    }

    fn gen_code(&mut self) {
        self.code = self.gen_stmt(self.ast.clone());
    }

    fn gen_stmt(&self, ast: AST) -> String {
        format!("define i32 @main() {{\n{}\n}}", self.gen_return(ast))
    }

    fn gen_return(&self, ast: AST) -> String {
        let expr_code = match ast.kind {
            ASTKind::Return(integer) => self.gen_expr(*integer),
            _ => unimplemented!(),
        };
        format!("  return i32 {}", expr_code)
    }

    fn gen_expr(&self, ast: AST) -> String {
        let n = match ast.kind {
            ASTKind::Integer(n) => n,
            _ => unimplemented!(),
        };
        format!("{}", n)
    }
}
