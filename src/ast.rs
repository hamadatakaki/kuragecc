use super::token::literal::{OperatorKind, TerminalSymbol};
use super::Location;

/*
    block  -> stmt* return
    return -> `return` (expr | identifier) `;`
    stmt   -> assign
    assign -> identifier `=` expr `;`
    expr   -> term expr'
    expr'  -> (`+`|`-`) term expr' | epsilon
    term   -> unary term'
    term'  -> (`*`|`/`) unary term' | epsilon
    unary  -> (`+`|`-`) factor | factor
    factor -> `(` expr `)` | number
    number -> integer
*/

#[derive(Debug, Clone)]
pub enum ASTKind {
    Block(Vec<AST>),
    Return(Box<AST>),
    Assign(String, Box<AST>),
    Binary(Box<AST>, Box<AST>, OperatorKind),
    Unary(Box<AST>, OperatorKind),
    Identifier(String),
    Integer(u32),
}

#[derive(Debug, Clone)]
pub struct AST {
    pub kind: ASTKind,
    pub location: Location,
}

impl AST {
    pub fn new(kind: ASTKind, location: Location) -> Self {
        Self { kind, location }
    }
}

impl std::fmt::Display for AST {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self.kind.clone() {
            ASTKind::Block(asts) => {
                for ast in asts {
                    println!("{}", ast);
                }
                write!(f, "")
            }
            ASTKind::Return(ast) => write!(f, "return [{}]", *ast),
            ASTKind::Assign(id, expr) => write!(f, "{} {} =", id, *expr),
            ASTKind::Binary(l, r, ope) => write!(f, "{} {} {}", *l, *r, ope.to_literal()),
            ASTKind::Unary(factor, ope) => write!(f, "0 {} {}", *factor, ope.to_literal()),
            ASTKind::Identifier(name) => write!(f, "{}", name),
            ASTKind::Integer(n) => write!(f, "{}", n),
        }
    }
}

pub fn visualize_ast(ast: AST) {
    rec_visualize_ast(ast, 0);
}

fn rec_visualize_ast(ast: AST, i: usize) {
    print!("{}", "  ".repeat(i));
    match ast.kind.clone() {
        ASTKind::Block(asts) => {
            println!("Block:");
            for ast in asts {
                rec_visualize_ast(ast, i + 1);
            }
        }
        ASTKind::Return(ast) => {
            println!("Return:");
            rec_visualize_ast(*ast, i + 1);
        }
        ASTKind::Assign(name, expr) => {
            println!("Assign {}:", name);
            rec_visualize_ast(*expr, i + 1)
        }
        ASTKind::Binary(l, r, ope) => {
            println!("Binary {}:", ope.to_literal());
            rec_visualize_ast(*l, i + 1);
            rec_visualize_ast(*r, i + 1);
        }
        ASTKind::Unary(factor, ope) => {
            println!("Unary {}:", ope.to_literal());
            rec_visualize_ast(*factor, i + 1);
        }
        ASTKind::Identifier(name) => println!("Identifier {},", name),
        ASTKind::Integer(n) => println!("Integer {},", n),
    }
}
