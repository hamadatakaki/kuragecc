use super::ast::{ASTKind, AST};
use super::token::literal::OperatorKind;
use std::collections::HashMap;

// TODO: unimplementedとunreachableの使い分けをする.

/*
    define i32 @main() {
        <name> = alloca i32
        store i32 <expr>, i32* <name>
        %r = load i32, i32* <name>
        ret i32 %r
    }
*/

// TODO: 変数管理の機構を追加する.
pub struct VariableManager {
    count: usize,
    book: HashMap<String, String>,
}

impl VariableManager {
    fn new() -> Self {
        Self {
            count: 1,
            book: HashMap::new(),
        }
    }

    fn new_id(&mut self, name: &str) -> String {
        let c = self.count;
        self.count += 1;
        format!("%val_{}_{}", c, name)
    }

    fn get_name(&self, name: String) -> Option<&String> {
        self.book.get(&name)
    }

    fn set_name(&mut self, name: String) -> String {
        let id = self.new_id(name.as_str());
        self.book.insert(name, id.clone());
        id
    }
}

pub struct CodeGenerator {
    ast: AST,
    lines: Vec<String>,
    manager: VariableManager,
}

impl CodeGenerator {
    pub fn new(ast: AST) -> Self {
        Self {
            ast,
            lines: vec![],
            manager: VariableManager::new(),
        }
    }

    pub fn code(&self) -> String {
        self.lines.concat()
    }

    pub fn gen_code(&mut self) {
        self.gen_block(self.ast.clone());
    }

    fn gen_block(&mut self, ast: AST) {
        self.lines.push(String::from("define i32 @main() {\n"));
        let stmts = match ast.kind {
            ASTKind::Block(stmts) => stmts,
            _ => unimplemented!(),
        };
        for stmt in stmts {
            match stmt.clone().kind {
                ASTKind::Assign(_, _) => self.gen_assign(stmt),
                ASTKind::Return(_) => {
                    self.gen_return(stmt);
                    break;
                }
                _ => unreachable!(),
            }
        }
        self.lines.push(String::from("}\n"));
    }

    // fn gen_stmt(&mut self, ast: AST) {}

    fn gen_return(&mut self, ast: AST) {
        let ret = match ast.kind {
            ASTKind::Return(expr) => match expr.kind {
                ASTKind::Identifier(name) => self.gen_identifier(name),
                _ => format!("{}", self.gen_expr(*expr)),
            },
            _ => unreachable!(),
        };
        self.lines.push(format!("  %r = load i32, i32* {}\n", ret));
        self.lines.push(format!("  ret i32 %r\n"));
    }

    fn gen_assign(&mut self, ast: AST) {
        // <name> = alloca i32
        // store i32 <expr>, i32* <name>

        match ast.kind {
            ASTKind::Assign(name, expr) => {
                let id = self.manager.set_name(name);

                let expr_code = self.gen_expr(*expr);

                self.lines.push(format!("  {} = alloca i32\n", id));
                self.lines
                    .push(format!("  store i32 {}, i32* {}\n", expr_code, id));
            }
            _ => unreachable!(),
        }
    }

    fn gen_identifier(&mut self, name: String) -> String {
        match self.manager.get_name(name) {
            Some(id) => format!("{}", id),
            None => unimplemented!(),
        }
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
