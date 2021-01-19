use super::expression::{CodeExpression, Expression, Symbol};

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
    FuncDefineOpen(Symbol, Vec<Symbol>),
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
    // function calling: (sym, args, assigned)
    FuncCall(Symbol, Vec<Expression>, Expression),
}

impl Code {
    pub fn to_assembly(&self) -> String {
        match self {
            Code::FuncDefineOpen(sym, params) => {
                let param_seq = params
                    .iter()
                    .map(|param| param.as_func_param())
                    .collect::<Vec<String>>()
                    .join(", ");

                format!("define i32 @{}({}) {{\n", sym.to_string(), param_seq)
            }
            Code::FuncDefineClose => format!("}}\n\n"),
            Code::Alloca(expr) => match expr.to_symbol() {
                Some(sym) => {
                    format!("  {} = alloca i32\n", sym.to_string())
                }
                None => unreachable!(),
            },
            Code::Store(expr, value) => match expr.to_symbol() {
                Some(sym) => {
                    format!(
                        "  store i32 {}, i32* {}\n",
                        value.to_string(),
                        sym.to_string()
                    )
                }
                None => unreachable!(),
            },
            Code::Load(from, to) => {
                let from = from.to_string();
                let to = to.to_string();
                format!("  {} = load i32, i32* {}\n", to, from)
            }
            Code::Return(expr) => format!("  ret i32 {}\n", expr.to_string()),
            Code::Add(left, right, ans) => {
                let ans = ans.to_string();
                format!(
                    "  {} = add i32 {}, {}\n",
                    ans,
                    left.to_string(),
                    right.to_string()
                )
            }
            Code::Sub(left, right, ans) => {
                let ans = ans.to_string();
                format!(
                    "  {} = sub i32 {}, {}\n",
                    ans,
                    left.to_string(),
                    right.to_string()
                )
            }
            Code::Multi(left, right, ans) => {
                let ans = ans.to_string();
                format!(
                    "  {} = mul i32 {}, {}\n",
                    ans,
                    left.to_string(),
                    right.to_string()
                )
            }
            Code::Divide(left, right, ans) => {
                let ans = ans.to_string();
                format!(
                    "  {} = sdiv i32 {}, {}\n",
                    ans,
                    left.to_string(),
                    right.to_string()
                )
            }
            Code::FuncCall(sym, args, assigned) => {
                let assigned = assigned.to_symbol().unwrap().to_string();
                let arg_seq = args
                    .iter()
                    .map(|param| param.as_func_arg())
                    .collect::<Vec<String>>()
                    .join(", ");
                format!(
                    "  {} = call i32 @{}({})\n",
                    assigned,
                    sym.to_string(),
                    arg_seq
                )
            }
        }
    }
}
