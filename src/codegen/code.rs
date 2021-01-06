use super::symbol::Symbol;

#[derive(Debug, Clone)]
pub enum Expression {
    Value(i32),
    Symbol(Symbol),
}

impl Expression {
    pub fn to_symbol(&self) -> Option<Symbol> {
        match self {
            Expression::Symbol(sym) => Some(sym.clone()),
            _ => None,
        }
    }

    pub fn to_string(&self) -> String {
        match self {
            Expression::Value(n) => format!("{}", n),
            Expression::Symbol(sym) => sym.reveal(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Code {
    DefineOpen,
    DefineClose,
    Alloca(Expression),
    Store(Expression, i32),
    Load(Expression, Expression), // (from, to)
    Return(Expression),
    // binary operations: (left, right, ans)
    Add(Expression, Expression, Expression),
    Sub(Expression, Expression, Expression),
    Multi(Expression, Expression, Expression),
    Divide(Expression, Expression, Expression),
}

impl Code {
    pub fn to_string(&self) -> String {
        match self {
            Code::DefineOpen => format!("define i32 @main() {{\n"),
            Code::DefineClose => format!("}}\n"),
            Code::Alloca(expr) => match expr.to_symbol() {
                Some(sym) => {
                    format!("  {} = alloca i32\n", sym.reveal())
                }
                None => unreachable!(),
            },
            Code::Store(expr, n) => match expr.to_symbol() {
                Some(sym) => {
                    format!("  store i32 {}, i32* {}\n", n, sym.reveal())
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
        }
    }
}
