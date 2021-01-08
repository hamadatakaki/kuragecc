use super::token::literal::{OperatorKind, TerminalSymbol};
use super::Location;

/*
    program   -> func*
    func      -> identifier `(` args-seq `)` block
    arg-seq   -> [(identifier `,`)* identifier]?
    block     -> `{` stmt* `}`
    stmt      -> assign | return
    assign    -> identifier `=` expr `;`
    return    -> `return` expr `;`
    expr      -> term expr'
    expr'     -> (`+`|`-`) term expr' | epsilon
    term      -> unary term'
    term'     -> (`*`|`/`) unary term' | epsilon
    unary     -> (`+`|`-`) factor | factor
    factor    -> `(` expr `)` | value
    value     -> integer | identifier | call-func
    call-func -> identifier `(` value-seq `)`
    value-seq -> [(value `,`)* value]?
*/

#[derive(Debug, Clone)]
pub enum ASTKind {
    Program(Vec<AST>),
    Func(String, Box<AST>),
    Block(Vec<AST>),
    Assign(String, Box<AST>),
    Return(Box<AST>),
    Binary(Box<AST>, Box<AST>, OperatorKind),
    Unary(Box<AST>, OperatorKind),
    Identifier(String),
    Integer(u32),
    FuncCall(String),
}

#[derive(Debug, Clone)]
pub struct AST {
    pub kind: ASTKind,
    pub scope: i32,
    pub location: Location,
}

impl AST {
    pub fn new(kind: ASTKind, scope: i32, location: Location) -> Self {
        Self {
            kind,
            scope,
            location,
        }
    }
}

impl std::fmt::Display for AST {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self.kind.clone() {
            ASTKind::Program(asts) => {
                for ast in asts {
                    println!("{}", ast);
                }
                write!(f, "")
            }
            ASTKind::Func(name, block) => {
                write!(f, "{}:\n{}", name, block)
                // print!("{}", name);
                // let args = args
                //     .iter()
                //     .map(|arg| format!("{}", arg))
                //     .collect::<Vec<String>>();
                // let args = args.join(" ");
                // write!(f, "{} {}:\n{}", name, args, block)
            }
            ASTKind::Block(asts) => {
                for ast in asts {
                    print!("{}", "  ".repeat(ast.scope as usize));
                    println!("{}", ast);
                }
                write!(f, "")
            }
            ASTKind::Return(ast) => write!(f, "return {}", *ast),
            ASTKind::Assign(id, expr) => write!(f, "{} {} =", id, *expr),
            ASTKind::Binary(l, r, ope) => write!(f, "{} {} {}", *l, *r, ope.to_literal()),
            ASTKind::Unary(factor, ope) => write!(f, "0 {} {}", *factor, ope.to_literal()),
            ASTKind::Identifier(name) => write!(f, "{}", name),
            ASTKind::Integer(n) => write!(f, "{}", n),
            ASTKind::FuncCall(name) => write!(f, "{}()", name),
            // let args = args
            //     .iter()
            //     .map(|arg| format!("{}", arg))
            //     .collect::<Vec<String>>();
            // let args = args.join(", ");
            // write!(f, "{}({})", name, args)
        }
    }
}

pub fn visualize_ast(ast: AST) {
    rec_visualize_ast(ast, 0);
}

fn rec_visualize_ast(ast: AST, i: usize) {
    print!("{}", "  ".repeat(i));
    match ast.kind.clone() {
        ASTKind::Program(asts) => {
            println!("Program:");
            for ast in asts {
                rec_visualize_ast(ast, i + 1);
            }
        }
        ASTKind::Func(name, block) => {
            println!("Function <scope: {}> {}:", ast.scope, name);
            rec_visualize_ast(*block, i + 1);
        }
        ASTKind::Block(asts) => {
            println!("Block <scope: {}>:", ast.scope);
            for ast in asts {
                rec_visualize_ast(ast, i + 1);
            }
        }
        ASTKind::Return(ast) => {
            println!("Return <scope: {}>:", ast.scope);
            rec_visualize_ast(*ast, i + 1);
        }
        ASTKind::Assign(name, expr) => {
            println!("Assign <scope: {}> {}:", ast.scope, name);
            rec_visualize_ast(*expr, i + 1)
        }
        ASTKind::Binary(l, r, ope) => {
            println!("Binary <scope: {}> {}:", ast.scope, ope.to_literal());
            rec_visualize_ast(*l, i + 1);
            rec_visualize_ast(*r, i + 1);
        }
        ASTKind::Unary(factor, ope) => {
            println!("Unary <scope: {}> {}:", ast.scope, ope.to_literal());
            rec_visualize_ast(*factor, i + 1);
        }
        ASTKind::Identifier(name) => println!("Identifier <scope: {}> {},", ast.scope, name),
        ASTKind::Integer(n) => println!("Integer <scope: {}> {},", ast.scope, n),
        ASTKind::FuncCall(name) => {
            println!("FunctionCalled <scope: {}> {},", ast.scope, name)
        }
    }
}
