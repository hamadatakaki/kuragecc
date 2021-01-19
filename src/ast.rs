use super::token::literal::{OperatorKind, PrimitiveType, TerminalSymbol};
use super::Location;

/*
    program -> block*

    block     -> func
    func      -> type-id `(` param-seq `)` comp-stmt
    param-seq -> type-id (`,` type-id)* | epsilon
    comp-stmt -> `{` stmt* `}`
    type-id   -> type identifier
    type      -> primitive

    stmt    -> assign | declare | dec-ass | return
    assign  -> identifier `=` expr `;`
    declare -> type-id `;`
    dec-ass -> type-id `=` expr `;`
    return  -> `return` expr `;`

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

#[derive(Debug, Clone, PartialEq)]
pub struct CustomType {
    name: String,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ValueType {
    Primitive(PrimitiveType),
    Custom(CustomType),
    InvalidTypeError,
    None,
}

impl ValueType {
    pub fn int() -> Self {
        ValueType::Primitive(PrimitiveType::Int)
    }
}

impl std::fmt::Display for ValueType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            ValueType::Primitive(primitive) => {
                write!(f, "{}", primitive.to_literal())
            }
            ValueType::Custom(custom) => {
                write!(f, "{}", custom.name)
            }
            ValueType::InvalidTypeError => write!(f, "<invalid-type-error-occured>"),
            ValueType::None => write!(f, "<none>"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ASTIdentifier {
    name: String,
    id_type: ValueType,
    scope: i32,
    location: Location,
}

impl ASTIdentifier {
    pub fn new(name: String, id_type: ValueType, scope: i32, loc: Location) -> Self {
        Self {
            name,
            id_type,
            scope,
            location: loc,
        }
    }

    pub fn get_name(&self) -> String {
        self.name.clone()
    }

    pub fn get_type(&self) -> ValueType {
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
    expr_type: ValueType,
    scope: i32,
    location: Location,
}

impl ASTExpr {
    pub fn new(kind: ASTExprKind, expr_type: ValueType, scope: i32, loc: Location) -> Self {
        Self {
            kind,
            expr_type,
            scope,
            location: loc,
        }
    }

    pub fn get_type(&self) -> ValueType {
        self.expr_type.clone()
    }
}

#[derive(Debug, Clone)]
pub enum ASTStmtKind {
    Assign(ASTIdentifier, ASTExpr),
    Declare(ASTIdentifier),
    DeclareAssign(ASTIdentifier, ASTExpr),
    Return(ASTExpr),
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
    pub program: Vec<ASTBlock>,
    pub scope: i32,
    pub location: Location,
}

impl AST {
    pub fn new(program: Vec<ASTBlock>, scope: i32, location: Location) -> Self {
        Self {
            program,
            scope,
            location,
        }
    }
}

pub trait PartialAST {
    fn get_scope(&self) -> i32;
    fn get_loc(&self) -> Location;
}

impl PartialAST for ASTIdentifier {
    fn get_scope(&self) -> i32 {
        self.scope
    }

    fn get_loc(&self) -> Location {
        self.location
    }
}

impl PartialAST for ASTExpr {
    fn get_scope(&self) -> i32 {
        self.scope
    }

    fn get_loc(&self) -> Location {
        self.location
    }
}

impl PartialAST for ASTStmt {
    fn get_scope(&self) -> i32 {
        self.scope
    }

    fn get_loc(&self) -> Location {
        self.location
    }
}

impl PartialAST for ASTBlock {
    fn get_scope(&self) -> i32 {
        self.scope
    }

    fn get_loc(&self) -> Location {
        self.location
    }
}

impl PartialAST for AST {
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
        match &self.kind {
            ASTExprKind::Binary(l, r, ope) => write!(f, "{} {} {}", *l, *r, ope.to_literal()),
            ASTExprKind::Unary(factor, ope) => write!(f, "0 {} {}", *factor, ope.to_literal()),
            ASTExprKind::Identifier(name) => write!(f, "{}", name),
            ASTExprKind::Integer(n) => write!(f, "{}", n),
            ASTExprKind::FuncCall(name, args) => {
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
        match &self.kind {
            ASTStmtKind::Assign(id, expr) | ASTStmtKind::DeclareAssign(id, expr) => {
                write!(f, "{} {} =", id, expr)
            }
            ASTStmtKind::Declare(id) => write!(f, "{}", id),
            ASTStmtKind::Return(expr) => write!(f, "return {}", expr),
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
    println!(
        "Identifier <scope: {}, type: {}> {},",
        id.scope,
        id.get_type(),
        id
    )
}

fn visualize_ast_expr(expr: ASTExpr, i: usize) {
    print!("{}", "  ".repeat(i));
    match expr.kind {
        ASTExprKind::Binary(l, r, ope) => {
            println!("Binary <scope: {}> {}:", expr.scope, ope.to_literal());
            visualize_ast_expr(*l, i + 1);
            visualize_ast_expr(*r, i + 1);
        }
        ASTExprKind::Unary(factor, ope) => {
            println!("Unary <scope: {}> {}:", expr.scope, ope.to_literal());
            visualize_ast_expr(*factor, i + 1);
        }
        ASTExprKind::Identifier(id) => println!(
            "Identifier <scope: {}, type: {}> {},",
            id.scope,
            id.get_type(),
            id
        ),
        ASTExprKind::Integer(n) => println!("Integer <scope: {}> {},", expr.scope, n),
        ASTExprKind::FuncCall(id, args) => {
            println!("FunctionCalled <scope: {}> {}:", expr.scope, id);
            for arg in args {
                visualize_ast_expr(arg, i + 1);
            }
        }
    }
}

fn visualize_ast_stmt(stmt: ASTStmt, i: usize) {
    print!("{}", "  ".repeat(i));
    match stmt.kind {
        ASTStmtKind::Assign(id, expr) => {
            println!("Assign <scope: {}>:", stmt.scope);
            visualize_ast_identifier(id, i + 1);
            visualize_ast_expr(expr, i + 1);
        }
        ASTStmtKind::Declare(id) => {
            println!("Declare <scope: {}>:", stmt.scope);
            visualize_ast_identifier(id, i + 1);
        }
        ASTStmtKind::DeclareAssign(id, expr) => {
            println!("DeclareAssign <scope: {}>:", stmt.scope);
            visualize_ast_identifier(id, i + 1);
            visualize_ast_expr(expr, i + 1);
        }
        ASTStmtKind::Return(expr) => {
            println!("Return <scope: {}>:", stmt.scope);
            visualize_ast_expr(expr, i + 1);
        }
    }
}

fn visualize_ast_block(block: ASTBlock, i: usize) {
    print!("{}", "  ".repeat(i));
    match block.kind {
        ASTBlockKind::Func(id, params, stmts) => {
            let param_string = params
                .iter()
                .map(|arg| format!("{}", arg))
                .collect::<Vec<String>>()
                .join(", ");
            println!(
                "Function <scope: {}, type: {}> {}({}):",
                block.scope,
                id.get_type(),
                id,
                param_string
            );
            for stmt in stmts {
                visualize_ast_stmt(stmt, i + 1);
            }
        }
    }
}
