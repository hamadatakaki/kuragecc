use super::expression::{Expression, Symbol};

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
    FuncDefineOpen(String, Vec<Symbol>),
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
    // function calling: (name, args, assigned)
    FuncCall(String, Vec<Expression>, Expression),
}

impl Code {
    pub fn to_assembly(&self) -> String {
        match self {
            Code::FuncDefineOpen(name, params) => {
                let param_seq = params
                    .iter()
                    .map(|param| param.as_func_param())
                    .collect::<Vec<String>>()
                    .join(", ");

                format!("define i32 @{}({}) {{\n", name, param_seq)
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
            Code::FuncCall(name, args, assigned) => {
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
