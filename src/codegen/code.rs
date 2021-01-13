use super::symbol::Symbol;

#[derive(Debug, Clone)]
pub enum ExpressionKind {
    Value(i32),
    Symbol(Symbol),
    // Call(String),
}

impl ExpressionKind {
    pub fn to_symbol(&self) -> Option<Symbol> {
        match self {
            ExpressionKind::Symbol(sym) => Some(sym.clone()),
            _ => None,
        }
    }

    pub fn to_string(&self) -> String {
        match self {
            ExpressionKind::Value(n) => format!("{}", n),
            ExpressionKind::Symbol(sym) => sym.reveal(),
            // ExpressionKind::Call(name) => name.clone(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Expression {
    pub kind: ExpressionKind,
    code_type: CodeType,
}

impl Expression {
    pub fn new(kind: ExpressionKind, code_type: CodeType) -> Self {
        Self { kind, code_type }
    }
    pub fn to_symbol(&self) -> Option<Symbol> {
        self.kind.to_symbol()
    }

    pub fn to_string(&self) -> String {
        self.kind.to_string()
    }

    pub fn as_func_arg(&self) -> String {
        format!("{} {}", self.code_type.to_string(), self.to_string())
    }
}

#[derive(Debug, Clone)]
pub enum CodeType {
    Int,
}

impl CodeType {
    pub fn to_string(&self) -> String {
        match self {
            CodeType::Int => format!("i32"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Code {
    FuncDefineOpen(Expression, Vec<Symbol>),
    FuncDefineClose,
    Alloca(Expression),
    // (to, stored-value)
    Store(Expression, Expression),
    // (from, to)
    Load(Expression, Expression),
    Return(Expression),
    // binary operations: (left, right, ans)
    Add(Expression, Expression, Expression),
    Sub(Expression, Expression, Expression),
    Multi(Expression, Expression, Expression),
    Divide(Expression, Expression, Expression),
    // function calling: assigned = name(args)
    FuncCall {
        name: String,
        assigned: Expression,
        args: Vec<Expression>,
    },
}

impl Code {
    pub fn to_assembly(&self) -> String {
        // TODO: アセンブリの関数定義・呼び出しにargを適応する実装
        match self {
            Code::FuncDefineOpen(define_name, params) => {
                let param_seq = params
                    .iter()
                    .map(|param| param.as_func_param())
                    .collect::<Vec<String>>()
                    .join(", ");

                format!(
                    "define i32 @{}({}) {{\n",
                    define_name.to_string(),
                    param_seq
                )
            }
            Code::FuncDefineClose => format!("}}\n\n"),
            Code::Alloca(expr) => match expr.to_symbol() {
                Some(sym) => {
                    format!("  {} = alloca i32\n", sym.reveal())
                }
                None => unreachable!(),
            },
            Code::Store(expr, value) => match expr.to_symbol() {
                Some(sym) => {
                    format!("  store i32 {}, i32* {}\n", value.to_string(), sym.reveal())
                }
                None => unreachable!(),
            },
            Code::Load(from, to) => {
                let from = from.to_symbol().unwrap().reveal();
                let to = to.to_symbol().unwrap().reveal();
                format!("  {} = load i32, i32* {}\n", to, from)
            }
            Code::Return(expr) => format!("  ret i32 {}\n", expr.to_string()),
            Code::Add(left, right, ans) => {
                let ans = ans.to_symbol().unwrap().reveal();
                format!(
                    "  {} = add i32 {}, {}\n",
                    ans,
                    left.to_string(),
                    right.to_string()
                )
            }
            Code::Sub(left, right, ans) => {
                let ans = ans.to_symbol().unwrap().reveal();
                format!(
                    "  {} = sub i32 {}, {}\n",
                    ans,
                    left.to_string(),
                    right.to_string()
                )
            }
            Code::Multi(left, right, ans) => {
                let ans = ans.to_symbol().unwrap().reveal();
                format!(
                    "  {} = mul i32 {}, {}\n",
                    ans,
                    left.to_string(),
                    right.to_string()
                )
            }
            Code::Divide(left, right, ans) => {
                let ans = ans.to_symbol().unwrap().reveal();
                format!(
                    "  {} = sdiv i32 {}, {}\n",
                    ans,
                    left.to_string(),
                    right.to_string()
                )
            }
            Code::FuncCall {
                name,
                assigned,
                args,
            } => {
                let assigned = assigned.to_symbol().unwrap().reveal();
                let arg_seq = args
                    .iter()
                    .map(|param| param.as_func_arg())
                    .collect::<Vec<String>>()
                    .join(", ");
                format!("  {} = call i32 @{}({})\n", assigned, name, arg_seq)
            }
        }
    }
}
