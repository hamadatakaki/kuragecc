use super::super::types::Type;
// use super::super::token::literal::OperatorKind;
use super::expression::{AsCode, Expression, Symbol};

#[derive(Debug, Clone)]
pub enum Code {
    // function
    FuncDefineOpen(Symbol, Vec<Type>),
    FuncDefineClose,
    // declare, assign
    Alloca(Symbol),
    Store(Expression, Symbol), // (value, store-to)
    Load(Symbol, Symbol),      // (from, to)
    // return
    Return(Expression),
    // binary operations: (l, r, ans)
    Add(Expression, Expression, Symbol),
    Sub(Expression, Expression, Symbol),
    Multi(Expression, Expression, Symbol),
    Divide(Expression, Expression, Symbol),
    Compare(Expression, Expression, Symbol),
    // function calling: (name, args, to)
    FuncCall(Symbol, Vec<Expression>, Symbol),
    // label
    Label(String),
    // jump, branch
    Jump(String),
    Branch(Expression, String, String),
}

impl Code {
    pub fn is_return(&self) -> bool {
        match self {
            Code::Return(_) => true,
            _ => false,
        }
    }

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
                    sym.type_as_code(),
                    sym.as_code(),
                    param_seq
                )
            }
            FuncDefineClose => format!("}}\n\n"),
            Alloca(sym) => format!("  {} = alloca {}\n", sym.as_code(), sym.type_as_code()),
            Store(val, sym) => format!(
                "  store {} {}, {}* {}\n",
                val.type_as_code(),
                val.as_code(),
                sym.type_as_code(),
                sym.as_code()
            ),
            Load(from, to) => format!(
                "  {} = load {}, {}* {}\n",
                to.as_code(),
                to.type_as_code(),
                from.type_as_code(),
                from.as_code()
            ),
            Return(expr) => format!("  ret {} {}\n", expr.type_as_code(), expr.as_code()),
            Add(l, r, ans) => format!(
                "  {} = add {} {}, {}\n",
                ans.as_code(),
                l.type_as_code(),
                l.as_code(),
                r.as_code()
            ),
            Sub(l, r, ans) => format!(
                "  {} = sub {} {}, {}\n",
                ans.as_code(),
                l.type_as_code(),
                l.as_code(),
                r.as_code()
            ),
            Multi(l, r, ans) => format!(
                "  {} = mul {} {}, {}\n",
                ans.as_code(),
                l.type_as_code(),
                l.as_code(),
                r.as_code()
            ),
            Divide(l, r, ans) => format!(
                "  {} = sdiv {} {}, {}\n",
                ans.as_code(),
                l.type_as_code(),
                l.as_code(),
                r.as_code()
            ),
            Compare(l, r, ans) => format!(
                "  {} = icmp ne {} {}, {}\n",
                ans.as_code(),
                l.type_as_code(),
                l.as_code(),
                r.as_code()
            ),
            FuncCall(name, args, to) => {
                let arg_seq = args
                    .iter()
                    .map(|param| param.as_func_arg())
                    .collect::<Vec<String>>()
                    .join(", ");
                format!(
                    "  {} = call {} @{}({})\n",
                    to.as_code(),
                    name.type_as_code(),
                    name.as_code(),
                    arg_seq
                )
            }
            Label(label) => format!("\n{}:\n", label),
            Jump(label) => format!("  br label %{}\n", label),
            Branch(cond, t_label, f_label) => format!(
                "  br i1 {}, label %{}, label %{}\n",
                cond.as_code(),
                t_label,
                f_label
            ),
        }
    }
}

impl std::fmt::Display for Code {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use Code::*;

        match self {
            FuncDefineOpen(sym, _) => write!(f, "Func {} {{", sym.as_code()),
            FuncDefineClose => write!(f, "}}"),
            Alloca(sym) => write!(f, "Alloca({})", sym.as_code()),
            Store(to, val) => write!(f, "Store({}: {})", to.as_code(), val.as_code()),
            Load(from, to) => write!(f, "Load({}: {})", to.as_code(), from.as_code()),
            Return(val) => write!(f, "Returen({})", val.as_code()),
            Add(l, r, ans) => write!(
                f,
                "Add({}: {} {} +)",
                ans.as_code(),
                l.as_code(),
                r.as_code()
            ),
            Sub(l, r, ans) => write!(
                f,
                "Sub({}: {} {} +)",
                ans.as_code(),
                l.as_code(),
                r.as_code()
            ),
            Multi(l, r, ans) => write!(
                f,
                "Multi({}: {} {} +)",
                ans.as_code(),
                l.as_code(),
                r.as_code()
            ),
            Divide(l, r, ans) => write!(
                f,
                "Devide({}: {} {} +)",
                ans.as_code(),
                l.as_code(),
                r.as_code()
            ),
            Compare(l, r, ans) => write!(
                f,
                "Compare({}: {} {} !=",
                ans.as_code(),
                l.as_code(),
                r.as_code()
            ),
            FuncCall(name, args, to) => {
                let arg_seq = args
                    .iter()
                    .map(|param| param.as_code())
                    .collect::<Vec<String>>()
                    .join(", ");
                write!(f, "Call({}: {}({})", to.as_code(), name.as_code(), arg_seq)
            }
            Label(label) => write!(f, "Label(`{}`)", label),
            Jump(label) => write!(f, "Jump(`{}`)", label),
            Branch(cond, t_label, f_label) => write!(
                f,
                "Branch({} ? `{}` : `{}`)",
                cond.as_code(),
                t_label,
                f_label
            ),
        }
    }
}

pub struct Assembly(pub Vec<Code>);

impl Assembly {
    pub fn to_assembly(&self) -> String {
        self.0
            .iter()
            .map(|code| code.to_assembly())
            .collect::<String>()
    }
}

impl std::fmt::Display for Assembly {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let s = self
            .0
            .iter()
            .map(|code| format!("{}\n", code))
            .collect::<String>();
        write!(f, "{}", s)
    }
}
