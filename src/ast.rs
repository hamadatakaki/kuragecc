use super::token::literal::{OperatorKind, TerminalSymbol};
use super::Position;

#[derive(Debug, Clone)]
pub enum ASTKind {
    Return(Box<AST>),
    Binary(Box<AST>, Box<AST>, OperatorKind),
    Unary(Box<AST>, OperatorKind),
    Integer(u32),
}

#[derive(Debug, Clone)]
pub struct AST {
    pub kind: ASTKind,
    pub pos: Position,
}

impl AST {
    pub fn new(kind: ASTKind, pos: Position) -> Self {
        Self { kind, pos }
    }
}

impl std::fmt::Display for AST {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self.kind.clone() {
            ASTKind::Return(ast) => write!(f, "return [{}]", *ast),
            ASTKind::Binary(l, r, ope) => write!(f, "{} {} {}", *l, *r, ope.to_literal()),
            ASTKind::Unary(factor, ope) => write!(f, "0 {} {}", *factor, ope.to_literal()),
            ASTKind::Integer(n) => write!(f, "{}", n),
        }
    }
}

fn visualize_ast(ast: AST) {
    rec_visualize_ast(ast, 0);
}

fn rec_visualize_ast(ast: AST, i: usize) {
    print!("{}", "  ".repeat(i));
    match ast.kind.clone() {
        ASTKind::Return(ast) => {
            println!("return:");
            rec_visualize_ast(*ast, i + 1)
        }
        ASTKind::Binary(l, r, ope) => {
            println!("{}:", ope.to_literal());
            rec_visualize_ast(*l, i + 1);
            rec_visualize_ast(*r, i + 1);
        }
        ASTKind::Unary(factor, ope) => {
            println!("{}:", ope.to_literal());
            rec_visualize_ast(*factor, i + 1);
        }
        ASTKind::Integer(n) => println!("{}:", n),
    }
}
