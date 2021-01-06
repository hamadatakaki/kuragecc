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

/*
main() {
    x = 40;
    y = 50;
    z = 60;
    return (x + z) * y ;
}

define i32 @main() #0 {
    %1 = alloca i32, align 4
    store i32 0, i32* %1, align 4

    // x
    %2 = alloca i32, align 4
    store i32 40, i32* %2, align 4
    %5 = load i32, i32* %2, align 4

    // y
    %4 = alloca i32, align 4
    store i32 60, i32* %4, align 4
    %6 = load i32, i32* %4, align 4

    // z
    %3 = alloca i32, align 4
    store i32 50, i32* %3, align 4
    %8 = load i32, i32* %3, align 4

    // x + z -> %7
    %7 = add nsw i32 %5, %6

    // %7 * y -> %9
    %9 = mul nsw i32 %7, %8

    ret i32 %9
}
*/

#[derive(Debug, Clone)]
pub enum Code {
    DefineOpen(Expression),
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
            Code::DefineOpen(define_name) => {
                format!("define i32 @{}() {{\n", define_name.to_string())
            }
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
