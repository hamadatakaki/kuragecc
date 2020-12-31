fn main() {
    let code = "return 42;";
    let mut lexer = Lexer::new(code);
    lexer.tokenize();
    let tokens = lexer.tokens;
    for token in tokens.clone() {
        println!("{}", token);
    }

    let mut parser = Parser::new(tokens);
    parser.parse();
    println!("{:?}", parser.ast.clone().unwrap());

    let mut generator = CodeGenerator::new(parser.ast.unwrap());
    generator.gen_code();
    println!("{}", generator.code);
}

type Position = (usize, usize);

#[derive(Debug, Clone)]
pub enum TokenKind {
    Reserved(ReservedWord),
    Integer(u32),
    Delimiter(DelimiterWord),
}

#[derive(Debug, Clone)]
pub enum ReservedWord {
    Return,
}

#[derive(Debug, Clone)]
pub enum DelimiterWord {
    Semicolon,
}

impl DelimiterWord {
    fn is_delimiter(c: char) -> bool {
        match c {
            ';' => true,
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

trait Inspector {
    type Yielded;

    fn forward(&mut self);
    fn backward(&mut self);
    fn look_at(&self) -> Self::Yielded;
    fn look_next(&self) -> Self::Yielded;
    fn look_prev(&self) -> Self::Yielded;
    fn look_and_forward(&mut self) -> Self::Yielded;
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

    fn at_end(&self) -> bool {
        self.look >= self.code.len()
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
            } else if DelimiterWord::is_delimiter(looked) {
                self.tokenize_delimiter();
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
        let kind = match s.as_str() {
            "return" => TokenKind::Reserved(ReservedWord::Return),
            _ => unimplemented!(),
        };
        let token = Token::new(kind, words, (start, self.look - start));
        self.tokens.push(token);
    }

    fn tokenize_delimiter(&mut self) {
        let start = self.look;
        let looked = self.look_and_forward();

        let kind = match looked {
            ';' => TokenKind::Delimiter(DelimiterWord::Semicolon),
            _ => unimplemented!(),
        };
        let token = Token::new(kind, vec![';'], (start, 1));
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
    Integer(u32),
}

#[derive(Debug, Clone)]
pub struct AST {
    expr: ASTKind,
    pos: Position,
}

impl AST {
    pub fn new(expr: ASTKind, pos: Position) -> Self {
        Self { expr, pos }
    }
}

/*
    stmt -> return
    return -> `return` expr `;`
    expr -> <integer>
*/

pub struct Parser {
    tokens: Vec<Token>,
    ast: Option<AST>,
    look: usize,
}

impl Parser {
    fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            ast: None,
            look: 0,
        }
    }

    fn look_at(&self) -> Token {
        self.tokens[self.look].clone()
    }

    fn look_and_forward(&mut self) -> Token {
        self.forward();
        self.look_prev()
    }

    fn look_prev(&self) -> Token {
        self.tokens[self.look - 1].clone()
    }

    fn look_next(&self) -> Token {
        self.tokens[self.look + 1].clone()
    }

    fn forward(&mut self) {
        self.look += 1;
    }

    fn at_end(&self) -> bool {
        self.look >= self.tokens.len()
    }

    fn parse(&mut self) {
        self.ast = Some(self.parse_stmt());
    }

    fn parse_stmt(&mut self) -> AST {
        self.parse_return()
    }

    fn parse_return(&mut self) -> AST {
        let ret = self.look_and_forward();
        let expr = self.parse_expr();
        let semicolon = self.look_and_forward();
        let pos = (ret.pos.0, semicolon.pos.0 - ret.pos.0 + semicolon.pos.1);
        let kind = ASTKind::Return(Box::new(expr));
        AST::new(kind, pos)
    }

    fn parse_expr(&mut self) -> AST {
        let token = self.look_and_forward();
        let pos = token.pos;
        let kind = match token.kind {
            TokenKind::Integer(n) => ASTKind::Integer(n),
            _ => unimplemented!(),
        };
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

    fn gen_stmt(&mut self, ast: AST) -> String {
        format!("define i32 @main() {{\n{}\n}}", self.gen_return(ast))
    }

    fn gen_return(&mut self, ast: AST) -> String {
        let expr_code = match ast.expr {
            ASTKind::Return(integer) => self.gen_expr(*integer),
            _ => unimplemented!(),
        };
        format!("  return i32 {}", expr_code)
    }

    fn gen_expr(&mut self, ast: AST) -> String {
        let n = match ast.expr {
            ASTKind::Integer(n) => n,
            _ => unimplemented!(),
        };
        format!("{}", n)
    }
}
