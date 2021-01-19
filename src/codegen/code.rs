use super::expression::{CodeExpression, Expression, Symbol};

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
                    .map(|param| param.get_type().as_code())
                    .collect::<Vec<String>>()
                    .join(", ");

                format!(
                    "define {} @{}({}) {{\n",
                    sym.get_type().as_code(),
                    sym.as_code(),
                    param_seq
                )
            }
            Code::FuncDefineClose => format!("}}\n\n"),
            Code::Alloca(expr) => match expr.to_symbol() {
                Some(sym) => {
                    format!(
                        "  {} = alloca {}\n",
                        sym.as_code(),
                        sym.get_type().as_code()
                    )
                }
                None => unreachable!(),
            },
            Code::Store(expr, value) => match expr.to_symbol() {
                Some(sym) => {
                    format!(
                        "  store {} {}, {}* {}\n",
                        value.get_type().as_code(),
                        value.as_code(),
                        sym.get_type().as_code(),
                        sym.as_code()
                    )
                }
                None => unreachable!(),
            },
            Code::Load(from, to) => {
                format!(
                    "  {} = load {}, {}* {}\n",
                    to.as_code(),
                    to.get_type().as_code(),
                    from.get_type().as_code(),
                    from.as_code()
                )
            }
            Code::Return(expr) => {
                format!("  ret {} {}\n", expr.get_type().as_code(), expr.as_code())
            }
            Code::Add(left, right, ans) => {
                let ans = ans.as_code();
                format!(
                    "  {} = add {} {}, {}\n",
                    ans,
                    left.get_type().as_code(),
                    left.as_code(),
                    right.as_code()
                )
            }
            Code::Sub(left, right, ans) => {
                let ans = ans.as_code();
                format!(
                    "  {} = sub {} {}, {}\n",
                    ans,
                    left.get_type().as_code(),
                    left.as_code(),
                    right.as_code()
                )
            }
            Code::Multi(left, right, ans) => {
                let ans = ans.as_code();
                format!(
                    "  {} = mul {} {}, {}\n",
                    ans,
                    left.get_type().as_code(),
                    left.as_code(),
                    right.as_code()
                )
            }
            Code::Divide(left, right, ans) => {
                let ans = ans.as_code();
                format!(
                    "  {} = sdiv {} {}, {}\n",
                    ans,
                    left.get_type().as_code(),
                    left.as_code(),
                    right.as_code()
                )
            }
            Code::FuncCall(sym, args, assigned) => {
                let assigned = assigned.to_symbol().unwrap().as_code();
                let arg_seq = args
                    .iter()
                    .map(|param| param.as_func_arg())
                    .collect::<Vec<String>>()
                    .join(", ");
                format!(
                    "  {} = call {} @{}({})\n",
                    assigned,
                    sym.get_type().as_code(),
                    sym.as_code(),
                    arg_seq
                )
            }
        }
    }
}
