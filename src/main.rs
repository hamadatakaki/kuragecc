use kuragecc::ast::visualize_ast;
use kuragecc::codegen::CodeGenerator;
use kuragecc::lexer::Lexer;
use kuragecc::parser::Parser;

use std::fs;

fn main() {
    let paths = vec![
        "example/main0.tmpc",
        "example/main1.tmpc",
        "example/lexer_error.tmpc",
        "example/parser_error0.tmpc",
    ];
    for path in paths {
        println!("=> {}\n", path);
        compile(path);
    }
}

fn compile(path: &str) {
    let code = fs::read_to_string(path).expect("File Input Error");

    println!("```\n{}```\n", code);

    let mut lexer = Lexer::new(&code);
    let tokens = match lexer.tokenize() {
        Ok(tokens) => tokens,
        Err(errors) => {
            println!("{:?}\n", errors);
            return;
        }
    };

    if std::env::var("RUST_BACKTRACE").is_ok() {
        for token in tokens.clone() {
            println!("{}", token);
        }
        println!();
    }

    let mut parser = Parser::new(tokens.clone());
    let ast = match parser.parse() {
        Ok(ast) => ast,
        Err(errors) => {
            println!("{:?}\n", errors);
            return;
        }
    };

    if std::env::var("RUST_BACKTRACE").is_ok() {
        visualize_ast(ast.clone());
        println!();
    }

    let mut generator = CodeGenerator::new(ast);
    generator.gen_code();
    println!("{}", generator.code());
}
