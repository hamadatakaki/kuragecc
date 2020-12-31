fn main() {
    let code = "return -(1+2)*5+(23-12)/6;";
    let mut lexer = Lexer::new(code);
    lexer.tokenize();
    let tokens = lexer.tokens;
    for token in tokens.clone() {
        println!("{}", token);
    }

    let mut parser = Parser::new(tokens);
    parser.parse();
    let ast = parser.ast.clone().unwrap();
    println!("{}", ast);

    // let mut generator = CodeGenerator::new(parser.ast.unwrap());
    // generator.gen_code();
    // println!("{}", generator.code);
}

trait Place {
    fn extend_to(&self, right: Self) -> Self;
}

type Position = (usize, usize);

impl Place for Position {
    fn extend_to(&self, right: Position) -> Position {
        (self.0, right.0 - self.0 + right.1)
    }
}

// TODO: 一度動くものを書いた後に、look_at系をOptionalに書き直す.

trait Inspector {
    type Yielded;

    fn forward(&mut self);
    fn backward(&mut self);
    fn at_end(&self) -> bool;
    fn look_at(&self) -> Self::Yielded;
    fn look_next(&self) -> Self::Yielded;
    fn look_prev(&self) -> Self::Yielded;
    fn look_and_forward(&mut self) -> Self::Yielded;
}

trait TerminalSymbol {
    type SymbolLiteral: PartialEq;

    fn contains(literal: Self::SymbolLiteral) -> bool;
    fn from_literal(literal: Self::SymbolLiteral) -> Self;
    fn to_literal(&self) -> Self::SymbolLiteral;
    fn is_literal(&self, literal: Self::SymbolLiteral) -> bool {
        self.to_literal() == literal
    }
}

#[derive(Debug, Clone)]
pub enum TokenKind {
    Reserved(ReservedLiteral),
    Integer(u32),
    Delimiter(DelimiterKind),
    Paren(ParenKind),
    Operator(OperatorKind),
}

#[derive(Debug, Clone)]
pub enum ReservedLiteral {
    Return,
}

impl ReservedLiteral {
    fn contains(word: &str) -> bool {
        match word {
            "return" => true,
            _ => false,
        }
    }

    fn from_literal(word: &str) -> Self {
        match word {
            "return" => ReservedLiteral::Return,
            _ => unreachable!(),
        }
    }

    fn to_literal(&self) -> &str {
        match self {
            ReservedLiteral::Return => "return",
            _ => unreachable!(),
        }
    }

    fn is_literal(&self, literal: &str) -> bool {
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
            _ => unreachable!(),
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

#[derive(Debug, Clone)]
pub enum OperatorKind {
    Plus,
    Minus,
    Times,
    Devide,
}

impl OperatorKind {
    fn priority(&self) -> OperatorPriority {
        match self {
            OperatorKind::Plus | OperatorKind::Minus => OperatorPriority::Addition,
            OperatorKind::Times | OperatorKind::Devide => OperatorPriority::Multiplication,
            _ => unreachable!(),
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

pub enum OperatorPriority {
    Addition,
    Multiplication,
}

impl OperatorPriority {
    fn is_addition(&self) -> bool {
        match self {
            OperatorPriority::Addition => true,
            _ => false,
        }
    }

    fn is_multiplication(&self) -> bool {
        match self {
            OperatorPriority::Multiplication => true,
            _ => false,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Token {
    kind: TokenKind,
    expression: Vec<char>,
    pos: Position,
}

impl Token {
    fn new(kind: TokenKind, expression: Vec<char>, pos: Position) -> Self {
        Self {
            kind,
            expression,
            pos,
        }
    }

    fn to_string(&self) -> String {
        self.expression.iter().collect::<String>()
    }
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        if std::env::var("RUST_BACKTRACE").is_ok() {
            write!(f, "Token<{}, {:?}>", self.to_string(), self.pos)
        } else {
            write!(f, "Token<{}>", self.to_string())
        }
    }
}

pub struct Lexer {
    code: Vec<char>,
    tokens: Vec<Token>,
    look: usize,
}

impl Lexer {
    pub fn new(code: &str) -> Self {
        let code = code.chars().collect::<Vec<char>>();
        Self {
            code,
            tokens: Vec::new(),
            look: 0,
        }
    }

    pub fn tokenize(&mut self) {
        self.rec_tokenize();
    }

    fn rec_tokenize(&mut self) {
        if !self.at_end() {
            let looked = self.look_at();
            if looked.is_ascii_digit() {
                // Number
                self.tokenize_integer();
            } else if looked.is_ascii_alphabetic() {
                // Reserved
                self.tokenize_reserved();
            } else if DelimiterKind::contains(looked) {
                // Delimiter
                self.tokenize_delimiter();
            } else if ParenKind::contains(looked) {
                // Paren
                self.tokenize_paren();
            } else if OperatorKind::contains(looked) {
                // Operator
                self.tokenize_operator();
            } else {
                self.forward();
            }
            self.rec_tokenize()
        }
    }

    fn tokenize_integer(&mut self) {
        let start = self.look;
        let mut nums = vec![];
        while !self.at_end() {
            let c = self.look_at();
            if c.is_ascii_digit() {
                nums.push(c);
                self.forward();
            } else {
                break;
            }
        }
        let s = nums.iter().collect::<String>();
        let n = s.parse::<u32>().unwrap();
        let kind = TokenKind::Integer(n);
        let token = Token::new(kind, nums, (start, self.look - start));
        self.tokens.push(token);
    }

    fn tokenize_reserved(&mut self) {
        let start = self.look;
        let mut words = vec![];
        while !self.at_end() {
            let c = self.look_at();
            if c.is_ascii_alphanumeric() {
                words.push(c);
                self.forward();
            } else {
                break;
            }
        }
        let s = words.iter().collect::<String>();
        let kind = if ReservedLiteral::contains(s.as_str()) {
            TokenKind::Reserved(ReservedLiteral::from_literal(&s))
        } else {
            unimplemented!()
        };
        let token = Token::new(kind, words, (start, self.look - start));
        self.tokens.push(token);
    }

    fn tokenize_delimiter(&mut self) {
        let start = self.look;
        let looked = self.look_and_forward();
        let kind = TokenKind::Delimiter(DelimiterKind::from_literal(looked));
        let token = Token::new(kind, vec![looked], (start, 1));
        self.tokens.push(token);
    }

    fn tokenize_paren(&mut self) {
        let start = self.look;
        let looked = self.look_and_forward();
        let kind = TokenKind::Paren(ParenKind::from_literal(looked));
        let token = Token::new(kind, vec![looked], (start, 1));
        self.tokens.push(token);
    }

    fn tokenize_operator(&mut self) {
        let start = self.look;
        let looked = self.look_and_forward();
        let kind = TokenKind::Operator(OperatorKind::from_literal(looked));
        let token = Token::new(kind, vec![looked], (start, 1));
        self.tokens.push(token);
    }
}

impl Inspector for Lexer {
    type Yielded = char;

    fn forward(&mut self) {
        self.look += 1;
    }

    fn backward(&mut self) {
        self.look -= 1;
    }

    fn at_end(&self) -> bool {
        self.look >= self.code.len()
    }

    fn look_at(&self) -> char {
        self.code[self.look]
    }

    fn look_prev(&self) -> char {
        self.code[self.look - 1]
    }

    fn look_next(&self) -> char {
        self.code[self.look + 1]
    }

    fn look_and_forward(&mut self) -> char {
        self.forward();
        self.look_prev()
    }
}

#[derive(Debug, Clone)]
pub enum ASTKind {
    Return(Box<AST>),
    Binary(Box<AST>, Box<AST>, OperatorKind),
    Unary(Box<AST>, OperatorKind),
    Integer(u32),
}

#[derive(Debug, Clone)]
pub struct AST {
    kind: ASTKind,
    pos: Position,
}

impl AST {
    pub fn new(kind: ASTKind, pos: Position) -> Self {
        Self { kind, pos }
    }
}

impl std::fmt::Display for AST {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self.kind.clone() {
            ASTKind::Return(ast) => write!(f, "return [{}]", *ast),
            ASTKind::Binary(l, r, ope) => write!(f, "{} {} {}", *l, *r, ope.to_literal()),
            ASTKind::Unary(factor, ope) => write!(f, "0 {} {}", *factor, ope.to_literal()),
            ASTKind::Integer(n) => write!(f, "{}", n),
        }
    }
}

fn visualize_ast(ast: AST) {
    rec_visualize_ast(ast, 0);
}

fn rec_visualize_ast(ast: AST, i: usize) {
    print!("{}", "  ".repeat(i));
    match ast.kind.clone() {
        ASTKind::Return(ast) => {
            println!("return:");
            rec_visualize_ast(*ast, i + 1)
        }
        ASTKind::Binary(l, r, ope) => {
            println!("{}:", ope.to_literal());
            rec_visualize_ast(*l, i + 1);
            rec_visualize_ast(*r, i + 1);
        }
        ASTKind::Unary(factor, ope) => {
            println!("{}:", ope.to_literal());
            rec_visualize_ast(*factor, i + 1);
        }
        ASTKind::Integer(n) => println!("{}:", n),
    }
}

/*
    stmt   -> return
    return -> `return` expr `;`
    expr   -> term expr'
    expr'  -> (`+`|`-`) term expr' | epsilon
    term   -> unary term'
    term'  -> (`*`|`/`) unary term' | epsilon
    unary  -> (`+`|`-`) factor | factor
    factor -> `(` expr `)` | number
    number -> integer
*/

pub struct Parser {
    tokens: Vec<Token>,
    ast: Option<AST>,
    look: usize,
}

impl Inspector for Parser {
    type Yielded = Token;

    fn forward(&mut self) {
        self.look += 1;
    }

    fn backward(&mut self) {
        self.look -= 1;
    }

    fn at_end(&self) -> bool {
        self.look >= self.tokens.len()
    }

    fn look_at(&self) -> Token {
        self.tokens[self.look].clone()
    }

    fn look_prev(&self) -> Token {
        self.tokens[self.look - 1].clone()
    }

    fn look_next(&self) -> Token {
        self.tokens[self.look + 1].clone()
    }

    fn look_and_forward(&mut self) -> Token {
        self.forward();
        self.look_prev()
    }
}

impl Parser {
    fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            ast: None,
            look: 0,
        }
    }

    fn parse(&mut self) {
        self.ast = Some(self.parse_stmt());
    }

    fn parse_stmt(&mut self) -> AST {
        // stmt   -> return
        self.parse_return()
    }

    fn parse_return(&mut self) -> AST {
        // return -> `return` expr `;`
        let ret = self.look_and_forward();
        let expr = self.parse_expr();
        let semicolon = self.look_and_forward();
        let pos = ret.pos.extend_to(semicolon.pos);
        let kind = ASTKind::Return(Box::new(expr));
        AST::new(kind, pos)
    }

    fn parse_expr(&mut self) -> AST {
        // expr   -> term expr'
        let term_left = self.parse_term();
        match self.parse_expr_prime() {
            Some((ope, term_right)) => {
                let pos = term_left.pos.extend_to(term_right.pos);
                let kind = ASTKind::Binary(Box::new(term_left), Box::new(term_right), ope);
                AST::new(kind, pos)
            }
            None => term_left,
        }
    }

    fn parse_expr_prime(&mut self) -> Option<(OperatorKind, AST)> {
        // expr'  -> (`+`|`-`) term expr' | epsilon
        let looked = self.look_at();
        match looked.kind {
            TokenKind::Operator(ope_kind) => {
                self.forward();
                let term_left = self.parse_term();
                let ast = match self.parse_expr_prime() {
                    Some((ope, term_right)) => {
                        let pos = term_left.pos.extend_to(term_right.pos);
                        let kind = ASTKind::Binary(Box::new(term_left), Box::new(term_right), ope);
                        AST::new(kind, pos)
                    }
                    None => term_left,
                };
                Some((ope_kind, ast))
            }
            _ => None,
        }
    }

    fn parse_term(&mut self) -> AST {
        // term   -> unary term'
        let unary_left = self.parse_unary();
        match self.parse_term_prime() {
            Some((ope, unary_right)) => {
                let pos = unary_left.pos.extend_to(unary_right.pos);
                let kind = ASTKind::Binary(Box::new(unary_left), Box::new(unary_right), ope);
                AST::new(kind, pos)
            }
            None => unary_left,
        }
    }

    fn parse_term_prime(&mut self) -> Option<(OperatorKind, AST)> {
        // term'  -> (`*`|`/`) unary term' | epsilon
        let looked = self.look_at();
        match looked.kind {
            TokenKind::Operator(ope_kind) if ope_kind.priority().is_multiplication() => {
                self.forward();
                let unary_left = self.parse_unary();
                let ast = match self.parse_term_prime() {
                    Some((ope, unary_right)) => {
                        let pos = unary_left.pos.extend_to(unary_right.pos);
                        let kind = ASTKind::Binary(
                            Box::new(unary_left.clone()),
                            Box::new(unary_right.clone()),
                            ope,
                        );
                        AST::new(kind, pos)
                    }
                    None => unary_left,
                };
                Some((ope_kind, ast))
            }
            _ => None,
        }
    }

    fn parse_unary(&mut self) -> AST {
        // unary  -> (`+`|`-`) factor | factor
        let looked = self.look_at();
        let ope = match looked.kind {
            TokenKind::Operator(ope_kind) if ope_kind.priority().is_addition() => {
                self.forward();
                Some(ope_kind)
            }
            _ => None,
        };
        let factor = self.parse_factor();
        match ope {
            Some(OperatorKind::Minus) => {
                let pos = looked.pos.extend_to(factor.pos);
                let kind = ASTKind::Unary(Box::new(factor), OperatorKind::Minus);
                AST::new(kind, pos)
            }
            _ => factor,
        }
    }

    fn parse_factor(&mut self) -> AST {
        // factor -> `(` expr `)` | number
        let looked = self.look_at();
        match looked.kind {
            TokenKind::Paren(paren_kind) if paren_kind.is_literal('(') => {
                self.forward();
                let expr = self.parse_expr();
                let close_token = self.look_and_forward();
                match close_token.kind {
                    TokenKind::Paren(paren_kind) if paren_kind.is_literal(')') => expr,
                    _ => unreachable!(),
                }
            }
            TokenKind::Integer(_) => self.parse_number(),
            _ => unreachable!(),
        }
    }

    fn parse_number(&mut self) -> AST {
        // number -> integer
        let looked = self.look_and_forward();
        let kind = match looked.kind {
            TokenKind::Integer(n) => ASTKind::Integer(n),
            _ => unreachable!(),
        };
        let pos = looked.pos;
        AST::new(kind, pos)
    }
}

pub struct CodeGenerator {
    ast: AST,
    code: String,
}

impl CodeGenerator {
    fn new(ast: AST) -> Self {
        Self {
            ast,
            code: String::new(),
        }
    }

    fn gen_code(&mut self) {
        self.code = self.gen_stmt(self.ast.clone());
    }

    fn gen_stmt(&self, ast: AST) -> String {
        format!("define i32 @main() {{\n{}\n}}", self.gen_return(ast))
    }

    fn gen_return(&self, ast: AST) -> String {
        let expr_code = match ast.kind {
            ASTKind::Return(integer) => self.gen_expr(*integer),
            _ => unimplemented!(),
        };
        format!("  return i32 {}", expr_code)
    }

    fn gen_expr(&self, ast: AST) -> String {
        let n = match ast.kind {
            ASTKind::Integer(n) => n,
            _ => unimplemented!(),
        };
        format!("{}", n)
    }
}
