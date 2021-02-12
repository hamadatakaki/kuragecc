use super::super::ast::types::Type;
use super::expression::{AsCode, Expression, Symbol};

#[derive(Debug, Clone)]
pub enum Code {
    FuncDefineOpen(Symbol, Vec<Type>),
    FuncDefineClose,
    Alloca(Symbol),
    // (to, stored-value)
    Store(Symbol, Expression),
    // (from, to)
    Load(Symbol, Symbol),
    Return(Expression),
    // binary operations: (left, right, ans)
    Add(Expression, Expression, Symbol),
    Sub(Expression, Expression, Symbol),
    Multi(Expression, Expression, Symbol),
    Divide(Expression, Expression, Symbol),
    // function calling: (sym, args, assigned)
    FuncCall(Symbol, Vec<Expression>, Symbol),
}

impl Code {
    pub fn to_assembly(&self) -> String {
        use Code::*;

        match self {
            FuncDefineOpen(sym, params) => {
                let param_seq = params
                    .iter()
                    .map(|ty| ty.as_code())
                    .collect::<Vec<String>>()
                    .join(", ");

                format!(
                    "define {} @{}({}) {{\n",
                    sym.get_type().as_code(),
                    sym.as_code(),
                    param_seq
                )
            }
            FuncDefineClose => format!("}}\n\n"),
            Alloca(sym) => format!(
                "  {} = alloca {}\n",
                sym.as_code(),
                sym.get_type().as_code()
            ),
            Store(sym, value) => format!(
                "  store {} {}, {}* {}\n",
                value.get_type().as_code(),
                value.as_code(),
                sym.get_type().as_code(),
                sym.as_code()
            ),
            Load(from, to) => {
                format!(
                    "  {} = load {}, {}* {}\n",
                    to.as_code(),
                    to.get_type().as_code(),
                    from.get_type().as_code(),
                    from.as_code()
                )
            }
            Return(expr) => {
                format!("  ret {} {}\n", expr.get_type().as_code(), expr.as_code())
            }
            Add(left, right, ans) => {
                let ans = ans.as_code();
                format!(
                    "  {} = add {} {}, {}\n",
                    ans,
                    left.get_type().as_code(),
                    left.as_code(),
                    right.as_code()
                )
            }
            Sub(left, right, ans) => {
                let ans = ans.as_code();
                format!(
                    "  {} = sub {} {}, {}\n",
                    ans,
                    left.get_type().as_code(),
                    left.as_code(),
                    right.as_code()
                )
            }
            Multi(left, right, ans) => {
                let ans = ans.as_code();
                format!(
                    "  {} = mul {} {}, {}\n",
                    ans,
                    left.get_type().as_code(),
                    left.as_code(),
                    right.as_code()
                )
            }
            Divide(left, right, ans) => {
                let ans = ans.as_code();
                format!(
                    "  {} = sdiv {} {}, {}\n",
                    ans,
                    left.get_type().as_code(),
                    left.as_code(),
                    right.as_code()
                )
            }
            FuncCall(sym, args, assigned) => {
                let assigned = assigned.as_code();
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
