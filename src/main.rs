use kuragecc::ast::visualize_ast;
use kuragecc::codegen::CodeGenerator;
use kuragecc::lexer::Lexer;
use kuragecc::parser::Parser;

use std::fs;

fn main() {
    let code = fs::read_to_string("example/main1.tmpc").expect("File Input Error");

    let mut lexer = Lexer::new(&code);
    lexer.tokenize();
    let tokens = lexer.tokens;

    if std::env::var("RUST_BACKTRACE").is_ok() {
        for token in tokens.clone() {
            println!("{}", token);
        }
        println!();
    }

    let mut parser = Parser::new(tokens.clone());
    parser.parse();
    let ast = parser.ast.unwrap();

    if std::env::var("RUST_BACKTRACE").is_ok() {
        visualize_ast(ast.clone());
        println!();
    }

    let mut generator = CodeGenerator::new(ast);
    generator.gen_code();
    println!("{}", generator.code());
}
