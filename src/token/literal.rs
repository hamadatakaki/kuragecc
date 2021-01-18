pub trait TerminalSymbol {
    type SymbolLiteral: PartialEq;

    fn contains(literal: Self::SymbolLiteral) -> bool;
    fn from_literal(literal: Self::SymbolLiteral) -> Self;
    fn to_literal(&self) -> Self::SymbolLiteral;
    fn is_literal(&self, literal: Self::SymbolLiteral) -> bool {
        self.to_literal() == literal
    }
}

#[derive(Debug, Clone)]
pub enum PrimitiveType {
    Int,
    Void,
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
        match literal.as_str() {
            "int" => PrimitiveType::Int,
            "void" => PrimitiveType::Void,
            _ => unreachable!(),
        }
    }

    fn to_literal(&self) -> String {
        let s = match self {
            PrimitiveType::Int => "int",
            PrimitiveType::Void => "void",
        };
        String::from(s)
    }
}

#[derive(Debug, Clone)]
pub enum ReservedLiteral {
    Return,
}

impl TerminalSymbol for ReservedLiteral {
    type SymbolLiteral = String;

    fn contains(literal: String) -> bool {
        match literal.as_str() {
            "return" => true,
            _ => false,
        }
    }

    fn from_literal(literal: String) -> ReservedLiteral {
        match literal.as_str() {
            "return" => ReservedLiteral::Return,
            _ => unreachable!(),
        }
    }

    fn to_literal(&self) -> String {
        let s = match self {
            ReservedLiteral::Return => "return",
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
        match literal {
            ';' => DelimiterKind::Semicolon,
            ',' => DelimiterKind::Comma,
            _ => unreachable!(),
        }
    }

    fn to_literal(&self) -> char {
        match self {
            DelimiterKind::Semicolon => ';',
            DelimiterKind::Comma => ',',
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
        match literal {
            '(' => ParenKind::NormalOpen,
            ')' => ParenKind::NormalClose,
            '[' => ParenKind::SquareOpen,
            ']' => ParenKind::SquareClose,
            '{' => ParenKind::CurlyOpen,
            '}' => ParenKind::CurlyClose,
            _ => unreachable!(),
        }
    }

    fn to_literal(&self) -> char {
        match self {
            ParenKind::NormalOpen => '(',
            ParenKind::NormalClose => ')',
            ParenKind::SquareOpen => '[',
            ParenKind::SquareClose => ']',
            ParenKind::CurlyOpen => '{',
            ParenKind::CurlyClose => '}',
        }
    }
}

pub enum OperatorPriority {
    Addition,
    Multiplication,
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
}

#[derive(Debug, Clone)]
pub enum OperatorKind {
    Plus,
    Minus,
    Times,
    Devide,
    Assign,
}

impl OperatorKind {
    pub fn priority(&self) -> OperatorPriority {
        match self {
            OperatorKind::Plus | OperatorKind::Minus => OperatorPriority::Addition,
            OperatorKind::Times | OperatorKind::Devide => OperatorPriority::Multiplication,
            OperatorKind::Assign => OperatorPriority::Assignment,
        }
    }
}

impl TerminalSymbol for OperatorKind {
    type SymbolLiteral = String;

    fn contains(literal: String) -> bool {
        match literal.as_str() {
            "+" | "-" | "*" | "/" | "=" => true,
            _ => false,
        }
    }

    fn from_literal(literal: String) -> OperatorKind {
        match literal.as_str() {
            "+" => OperatorKind::Plus,
            "-" => OperatorKind::Minus,
            "*" => OperatorKind::Times,
            "/" => OperatorKind::Devide,
            "=" => OperatorKind::Assign,
            _ => unreachable!(),
        }
    }

    fn to_literal(&self) -> String {
        let s = match self {
            OperatorKind::Plus => "+",
            OperatorKind::Minus => "-",
            OperatorKind::Times => "*",
            OperatorKind::Devide => "/",
            OperatorKind::Assign => "=",
        };
        String::from(s)
    }
}
