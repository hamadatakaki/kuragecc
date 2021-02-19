pub trait TerminalSymbol {
    type SymbolLiteral: PartialEq;

    fn contains(literal: Self::SymbolLiteral) -> bool;
    fn from_literal(literal: Self::SymbolLiteral) -> Self;
    fn to_literal(&self) -> Self::SymbolLiteral;
    fn is_literal(&self, literal: Self::SymbolLiteral) -> bool {
        self.to_literal() == literal
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum PrimitiveType {
    Int,
    Void,
}

impl PrimitiveType {
    pub fn as_code(&self) -> String {
        use PrimitiveType::*;

        let s = match self {
            Int => "i32",
            Void => unimplemented!(),
        };
        String::from(s)
    }
}

impl TerminalSymbol for PrimitiveType {
    type SymbolLiteral = String;

    fn contains(literal: String) -> bool {
        match literal.as_str() {
            "int" | "void" => true,
            _ => false,
        }
    }

    fn from_literal(literal: String) -> PrimitiveType {
        use PrimitiveType::*;

        match literal.as_str() {
            "int" => Int,
            "void" => Void,
            _ => unreachable!(),
        }
    }

    fn to_literal(&self) -> String {
        use PrimitiveType::*;

        let s = match self {
            Int => "int",
            Void => "void",
        };
        String::from(s)
    }
}

#[derive(Debug, Clone)]
pub enum ReservedLiteral {
    Return,
    If,
    Else,
}

impl TerminalSymbol for ReservedLiteral {
    type SymbolLiteral = String;

    fn contains(literal: String) -> bool {
        match literal.as_str() {
            "return" | "if" | "else" => true,
            _ => false,
        }
    }

    fn from_literal(literal: String) -> ReservedLiteral {
        match literal.as_str() {
            "return" => ReservedLiteral::Return,
            "if" => ReservedLiteral::If,
            "else" => ReservedLiteral::Else,
            _ => unreachable!(),
        }
    }

    fn to_literal(&self) -> String {
        let s = match self {
            ReservedLiteral::Return => "return",
            ReservedLiteral::If => "if",
            ReservedLiteral::Else => "else",
        };
        String::from(s)
    }
}

#[derive(Debug, Clone)]
pub enum DelimiterKind {
    Semicolon,
    Comma,
}

impl TerminalSymbol for DelimiterKind {
    type SymbolLiteral = char;

    fn contains(literal: char) -> bool {
        match literal {
            ';' | ',' => true,
            _ => false,
        }
    }

    fn from_literal(literal: char) -> DelimiterKind {
        use DelimiterKind::*;

        match literal {
            ';' => Semicolon,
            ',' => Comma,
            _ => unreachable!(),
        }
    }

    fn to_literal(&self) -> char {
        use DelimiterKind::*;

        match self {
            Semicolon => ';',
            Comma => ',',
        }
    }
}

#[derive(Debug, Clone)]
pub enum ParenKind {
    NormalOpen,
    NormalClose,
    SquareOpen,
    SquareClose,
    CurlyOpen,
    CurlyClose,
}

impl TerminalSymbol for ParenKind {
    type SymbolLiteral = char;

    fn contains(literal: char) -> bool {
        match literal {
            '(' | ')' | '[' | ']' | '{' | '}' => true,
            _ => false,
        }
    }

    fn from_literal(literal: char) -> ParenKind {
        use ParenKind::*;

        match literal {
            '(' => NormalOpen,
            ')' => NormalClose,
            '[' => SquareOpen,
            ']' => SquareClose,
            '{' => CurlyOpen,
            '}' => CurlyClose,
            _ => unreachable!(),
        }
    }

    fn to_literal(&self) -> char {
        use ParenKind::*;

        match self {
            NormalOpen => '(',
            NormalClose => ')',
            SquareOpen => '[',
            SquareClose => ']',
            CurlyOpen => '{',
            CurlyClose => '}',
        }
    }
}
pub enum OperatorPriority {
    Addition,
    Multiplication,
    Equivalence,
    Assignment,
}

impl OperatorPriority {
    pub fn is_addition(&self) -> bool {
        match self {
            OperatorPriority::Addition => true,
            _ => false,
        }
    }

    pub fn is_multiplication(&self) -> bool {
        match self {
            OperatorPriority::Multiplication => true,
            _ => false,
        }
    }

    pub fn is_equivalence(&self) -> bool {
        match self {
            OperatorPriority::Equivalence => true,
            _ => false,
        }
    }
}

#[derive(Debug, Clone)]
pub enum OperatorKind {
    Plus,
    Minus,
    Times,
    Devide,
    Assign,
    Equal,
    NotEqual,
}

impl OperatorKind {
    pub fn priority(&self) -> OperatorPriority {
        use OperatorKind::*;
        use OperatorPriority::*;

        match self {
            Plus | Minus => Addition,
            Times | Devide => Multiplication,
            Assign => Assignment,
            Equal | NotEqual => Equivalence,
        }
    }
}

impl TerminalSymbol for OperatorKind {
    type SymbolLiteral = String;

    fn contains(literal: String) -> bool {
        match literal.as_str() {
            "+" | "-" | "*" | "/" | "=" | "!" | "==" | "!=" => true,
            _ => false,
        }
    }

    fn from_literal(literal: String) -> OperatorKind {
        use OperatorKind::*;

        match literal.as_str() {
            "+" => Plus,
            "-" => Minus,
            "*" => Times,
            "/" => Devide,
            "=" => Assign,
            "==" => Equal,
            "!=" => NotEqual,
            _ => unreachable!(),
        }
    }

    fn to_literal(&self) -> String {
        use OperatorKind::*;

        let s = match self {
            Plus => "+",
            Minus => "-",
            Times => "*",
            Devide => "/",
            Assign => "=",
            Equal => "==",
            NotEqual => "!=",
        };
        String::from(s)
    }
}
