use super::token::literal::OperatorKind;
use super::types::Type;

#[derive(Debug, Clone)]
pub enum Operator {
    Plus,
    Minus,
    Times,
    Devide,
    Equal,
    NotEqual,
}

impl Operator {
    pub fn from_ope_kind(kind: OperatorKind) -> Self {
        use OperatorKind::*;

        match kind {
            Plus => Operator::Plus,
            Minus => Operator::Minus,
            Times => Operator::Times,
            Devide => Operator::Devide,
            Equal => Operator::Equal,
            NotEqual => Operator::NotEqual,
            _ => unreachable!(),
        }
    }

    pub fn is_arithmetic(&self) -> bool {
        use Operator::*;

        match self {
            Plus | Minus | Times | Devide => true,
            _ => false,
        }
    }

    pub fn is_equivalence(&self) -> bool {
        use Operator::*;

        match self {
            Equal | NotEqual => true,
            _ => false,
        }
    }

    pub fn as_code(&self) -> String {
        use Operator::*;

        let s = match self {
            Plus => "add",
            Minus => "sub",
            Times => "mul",
            Devide => "sdiv",
            Equal => "eq",
            NotEqual => "ne",
        };
        format!("{}", s)
    }

    pub fn type_check_binary(&self, l: Type, r: Type) -> bool {
        // TODO: 型を増やして処理を詳しくする.

        if self.is_arithmetic() {
            l == r
        } else if self.is_equivalence() {
            l == r
        } else {
            unreachable!()
        }
    }

    pub fn type_check_unary(&self, t: Type) -> bool {
        // TODO: 型を増やして処理を詳しくする.

        if self.is_arithmetic() {
            t == Type::int()
        } else {
            unreachable!()
        }
    }
}

impl std::fmt::Display for Operator {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use Operator::*;

        match self {
            Plus => write!(f, "+"),
            Minus => write!(f, "-"),
            Times => write!(f, "*"),
            Devide => write!(f, "/"),
            Equal => write!(f, "=="),
            NotEqual => write!(f, "!="),
        }
    }
}
