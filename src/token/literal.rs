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
pub enum ReservedLiteral {
    Return,
}

impl ReservedLiteral {
    pub fn contains(word: &str) -> bool {
        match word {
            "return" => true,
            _ => false,
        }
    }

    pub fn from_literal(word: &str) -> Self {
        match word {
            "return" => ReservedLiteral::Return,
            _ => unreachable!(),
        }
    }

    pub fn to_literal(&self) -> &str {
        match self {
            ReservedLiteral::Return => "return",
        }
    }

    pub fn is_literal(&self, literal: &str) -> bool {
        self.to_literal() == literal
    }
}

#[derive(Debug, Clone)]
pub enum DelimiterKind {
    Semicolon,
}

impl TerminalSymbol for DelimiterKind {
    type SymbolLiteral = char;

    fn contains(literal: char) -> bool {
        match literal {
            ';' => true,
            _ => false,
        }
    }

    fn from_literal(literal: char) -> DelimiterKind {
        match literal {
            ';' => DelimiterKind::Semicolon,
            _ => unreachable!(),
        }
    }

    fn to_literal(&self) -> char {
        match self {
            DelimiterKind::Semicolon => ';',
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
}

impl OperatorKind {
    pub fn priority(&self) -> OperatorPriority {
        match self {
            OperatorKind::Plus | OperatorKind::Minus => OperatorPriority::Addition,
            OperatorKind::Times | OperatorKind::Devide => OperatorPriority::Multiplication,
        }
    }
}

impl TerminalSymbol for OperatorKind {
    type SymbolLiteral = char;

    fn contains(word: char) -> bool {
        match word {
            '+' | '-' | '*' | '/' => true,
            _ => false,
        }
    }

    fn from_literal(word: char) -> OperatorKind {
        match word {
            '+' => OperatorKind::Plus,
            '-' => OperatorKind::Minus,
            '*' => OperatorKind::Times,
            '/' => OperatorKind::Devide,
            _ => unreachable!(),
        }
    }

    fn to_literal(&self) -> char {
        match self {
            OperatorKind::Plus => '+',
            OperatorKind::Minus => '-',
            OperatorKind::Times => '*',
            OperatorKind::Devide => '/',
        }
    }
}
