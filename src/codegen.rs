use super::ast::{ASTKind, AST};
use super::token::literal::OperatorKind;

pub struct CodeGenerator {
    ast: AST,
    lines: Vec<String>,
}

impl CodeGenerator {
    pub fn new(ast: AST) -> Self {
        Self { ast, lines: vec![] }
    }

    pub fn code(&self) -> String {
        self.lines.concat()
    }

    pub fn gen_code(&mut self) {
        self.gen_stmt(self.ast.clone());
    }

    fn gen_stmt(&mut self, ast: AST) {
        self.lines.push(String::from("define i32 @main() {\n"));
        self.gen_return(ast);
        self.lines.push(String::from("}\n"));
    }

    fn gen_return(&mut self, ast: AST) {
        let ret_code = match ast.kind {
            ASTKind::Return(expr) => self.gen_expr(*expr),
            _ => unimplemented!(),
        };
        self.lines.push(format!("  ret i32 {}\n", ret_code));
    }

    fn gen_expr(&self, ast: AST) -> i32 {
        match ast.kind {
            ASTKind::Integer(n) => n as i32,
            ASTKind::Binary(left, right, ope) => {
                let l = self.gen_expr(*left);
                let r = self.gen_expr(*right);
                match ope {
                    OperatorKind::Plus => l + r,
                    OperatorKind::Minus => l - r,
                    OperatorKind::Times => l * r,
                    OperatorKind::Devide => l / r,
                    _ => unimplemented!(),
                }
            }
            ASTKind::Unary(factor, ope) => {
                let sign = match ope {
                    OperatorKind::Minus => -1,
                    _ => 1,
                };
                sign * self.gen_expr(*factor)
            }
            _ => unimplemented!(),
        }
    }
}
