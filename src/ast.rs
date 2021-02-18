pub mod types;

use super::token::literal::{OperatorKind, TerminalSymbol};
use super::Location;
use types::Type;

/*
    program -> block*

    block     -> func
    func      -> type-id `(` param-seq `)` comp-stmt
    param-seq -> type-id (`,` type-id)* | epsilon
    comp-stmt -> `{` stmt* `}`
    type-id   -> type identifier
    type      -> primitive

    stmt  -> assign | declare | dec-ass | return | if
    assign  -> identifier `=` expr `;`
    declare -> type-id `;`
    dec-ass -> type-id `=` expr `;`
    return  -> `return` expr `;`
    if      -> `if` `(` expr `)` stmt (`else` stmt)?

    expr      -> term expr'
    expr'     -> (`+`|`-`) term expr' | epsilon
    term      -> unary term'
    term'     -> (`*`|`/`) unary term' | epsilon
    unary     -> (`+`|`-`) factor | factor
    factor    -> `(` expr `)` | value
    value     -> integer | identifier | call-func
    call-func -> identifier `(` arg-seq `)`
    arg-seq   -> expr (`,` expr)* | epsilon
*/

#[derive(Debug, Clone)]
pub struct ASTIdentifier {
    name: String,
    id_type: Type,
    location: Location,
}

impl ASTIdentifier {
    pub fn new(name: String, id_type: Type, loc: Location) -> Self {
        Self {
            name,
            id_type,
            location: loc,
        }
    }

    pub fn get_name(&self) -> String {
        self.name.clone()
    }

    pub fn get_type(&self) -> Type {
        self.id_type.clone()
    }
}

#[derive(Debug, Clone)]
pub enum ASTExprKind {
    Binary(Box<ASTExpr>, Box<ASTExpr>, OperatorKind),
    Unary(Box<ASTExpr>, OperatorKind),
    Identifier(ASTIdentifier),
    Integer(u32),
    FuncCall(ASTIdentifier, Vec<ASTExpr>),
}

#[derive(Debug, Clone)]
pub struct ASTExpr {
    pub kind: ASTExprKind,
    location: Location,
}

impl ASTExpr {
    pub fn new(kind: ASTExprKind, loc: Location) -> Self {
        Self {
            kind,
            location: loc,
        }
    }
}

#[derive(Debug, Clone)]
pub enum ASTStmtKind {
    Assign(ASTIdentifier, ASTExpr),
    Declare(ASTIdentifier),
    DeclareAssign(ASTIdentifier, ASTExpr),
    Return(ASTExpr),
    If(ASTExpr, Vec<ASTStmt>, Vec<ASTStmt>), // cond, true, false
}

#[derive(Debug, Clone)]
pub struct ASTStmt {
    pub kind: ASTStmtKind,
    scope: i32,
    location: Location,
}

impl ASTStmt {
    pub fn new(kind: ASTStmtKind, scope: i32, loc: Location) -> Self {
        Self {
            kind,
            scope,
            location: loc,
        }
    }
}

#[derive(Debug, Clone)]
pub enum ASTBlockKind {
    Func(ASTIdentifier, Vec<ASTIdentifier>, Vec<ASTStmt>),
}

#[derive(Debug, Clone)]
pub struct ASTBlock {
    pub kind: ASTBlockKind,
    scope: i32,
    location: Location,
}

impl ASTBlock {
    pub fn new(kind: ASTBlockKind, scope: i32, loc: Location) -> Self {
        Self {
            kind,
            scope,
            location: loc,
        }
    }
}

#[derive(Debug, Clone)]
pub struct AST {
    program: Vec<ASTBlock>,
    scope: i32,
    location: Location,
}

impl AST {
    pub fn new(program: Vec<ASTBlock>, scope: i32, location: Location) -> Self {
        Self {
            program,
            scope,
            location,
        }
    }

    pub fn program(&self) -> Vec<ASTBlock> {
        self.program.clone()
    }
}

pub trait HasSyntaxKind {
    type Kind;
    fn get_kind(&self) -> Self::Kind;
}

pub trait AsSyntaxExpression {
    fn get_loc(&self) -> Location;
}

pub trait AsSyntaxStatement {
    fn get_scope(&self) -> i32;
    fn get_loc(&self) -> Location;
}

impl AsSyntaxExpression for ASTIdentifier {
    fn get_loc(&self) -> Location {
        self.location
    }
}

impl AsSyntaxExpression for ASTExpr {
    fn get_loc(&self) -> Location {
        self.location
    }
}

impl HasSyntaxKind for ASTExpr {
    type Kind = ASTExprKind;
    fn get_kind(&self) -> Self::Kind {
        self.kind.clone()
    }
}

impl AsSyntaxStatement for ASTStmt {
    fn get_scope(&self) -> i32 {
        self.scope
    }

    fn get_loc(&self) -> Location {
        self.location
    }
}

impl HasSyntaxKind for ASTStmt {
    type Kind = ASTStmtKind;
    fn get_kind(&self) -> Self::Kind {
        self.kind.clone()
    }
}

impl AsSyntaxStatement for ASTBlock {
    fn get_scope(&self) -> i32 {
        self.scope
    }

    fn get_loc(&self) -> Location {
        self.location
    }
}

impl HasSyntaxKind for ASTBlock {
    type Kind = ASTBlockKind;
    fn get_kind(&self) -> Self::Kind {
        self.kind.clone()
    }
}

impl AsSyntaxStatement for AST {
    fn get_scope(&self) -> i32 {
        self.scope
    }

    fn get_loc(&self) -> Location {
        self.location
    }
}

impl std::fmt::Display for ASTIdentifier {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl std::fmt::Display for ASTExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use ASTExprKind::*;

        match &self.kind {
            Binary(l, r, ope) => write!(f, "{} {} {}", *l, *r, ope.to_literal()),
            Unary(factor, ope) => write!(f, "0 {} {}", *factor, ope.to_literal()),
            Identifier(name) => write!(f, "{}", name),
            Integer(n) => write!(f, "{}", n),
            FuncCall(name, args) => {
                let arg_string = args
                    .iter()
                    .map(|arg| format!("{}", arg))
                    .collect::<Vec<String>>()
                    .join(", ");
                write!(f, "{}({})", name, arg_string)
            }
        }
    }
}

impl std::fmt::Display for ASTStmt {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use ASTStmtKind::*;

        match &self.kind {
            Assign(id, expr) | DeclareAssign(id, expr) => {
                write!(f, "{} {} =", id, expr)
            }
            Declare(id) => write!(f, "{}", id),
            Return(expr) => write!(f, "return {}", expr),
            If(expr, t_stmts, f_stmts) => {
                let mut lines = vec![format!("if({}){{", expr)];
                for line in t_stmts {
                    lines.push(format!("  {}\n", line));
                }

                lines.push(format!("}}"));

                if !f_stmts.is_empty() {
                    lines.push(format!("else{{"));
                    for line in f_stmts {
                        lines.push(format!("  {}\n", line));
                    }
                    lines.push(format!("}}"));
                }

                write!(f, "{}", lines.join(""))
            }
        }
    }
}

impl std::fmt::Display for ASTBlock {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match &self.kind {
            ASTBlockKind::Func(id, params, stmts) => {
                let param_string = params
                    .iter()
                    .map(|arg| format!("{}", arg))
                    .collect::<Vec<String>>()
                    .join(", ");
                let mut lines = vec![format!("{}({}):\n", id, param_string)];
                for line in stmts {
                    lines.push(format!("  {}\n", line));
                }
                let func = lines.join("");
                write!(f, "{}", func)
            }
        }
    }
}

impl std::fmt::Display for AST {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let program = self
            .program
            .iter()
            .map(|block| format!("{}", block))
            .collect::<Vec<String>>()
            .join("");
        write!(f, "{}", program)
    }
}

pub fn visualize_ast(ast: AST) {
    println!("Program:");
    for block in ast.program {
        visualize_ast_block(block, 1);
    }
}

fn visualize_ast_identifier(id: ASTIdentifier, i: usize) {
    print!("{}", "  ".repeat(i));
    println!("Identifier {}", id)
}

fn visualize_ast_expr(expr: ASTExpr, i: usize) {
    print!("{}", "  ".repeat(i));

    use ASTExprKind::*;

    match expr.kind {
        Binary(l, r, ope) => {
            println!("Binary {}:", ope.to_literal());
            visualize_ast_expr(*l, i + 1);
            visualize_ast_expr(*r, i + 1);
        }
        Unary(factor, ope) => {
            println!("Unary {}:", ope.to_literal());
            visualize_ast_expr(*factor, i + 1);
        }
        Identifier(id) => println!("Identifier {}", id),
        Integer(n) => println!("Integer {}", n),
        FuncCall(id, args) => {
            println!("FunctionCalled {}:", id);
            for arg in args {
                visualize_ast_expr(arg, i + 1);
            }
        }
    }
}

fn visualize_ast_stmt(stmt: ASTStmt, i: usize) {
    print!("{}", "  ".repeat(i));

    use ASTStmtKind::*;

    match stmt.kind {
        Assign(id, expr) => {
            println!("Assign <scope: {}>:", stmt.scope);
            visualize_ast_identifier(id, i + 1);
            visualize_ast_expr(expr, i + 1);
        }
        Declare(id) => {
            println!("Declare <scope: {}>:", stmt.scope);
            visualize_ast_identifier(id, i + 1);
        }
        DeclareAssign(id, expr) => {
            println!("DeclareAssign <scope: {}>:", stmt.scope);
            visualize_ast_identifier(id, i + 1);
            visualize_ast_expr(expr, i + 1);
        }
        Return(expr) => {
            println!("Return <scope: {}>:", stmt.scope);
            visualize_ast_expr(expr, i + 1);
        }
        If(expr, t_stmts, f_stmts) => {
            println!("If <scope: {}> ({}):", stmt.scope, expr);
            for stmt in t_stmts {
                visualize_ast_stmt(stmt, i + 1);
            }

            if !f_stmts.is_empty() {
                println!("{}Else <scope: {}>:", "  ".repeat(i), stmt.scope);
                for stmt in f_stmts {
                    visualize_ast_stmt(stmt, i + 1);
                }
            }
        }
    }
}

fn visualize_ast_block(block: ASTBlock, i: usize) {
    print!("{}", "  ".repeat(i));
    match block.kind {
        ASTBlockKind::Func(id, params, stmts) => {
            println!("Function <scope: {}> {}:", block.scope, id);
            for p in params {
                visualize_ast_identifier(p, i + 1);
            }
            println!("{}Function-Statement:", "  ".repeat(i));
            for stmt in stmts {
                visualize_ast_stmt(stmt, i + 1);
            }
        }
    }
}
