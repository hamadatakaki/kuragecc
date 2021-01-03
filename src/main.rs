use kuragecc::ast::visualize_ast;
use kuragecc::codegen::CodeGenerator;
use kuragecc::error::visualize_parser_error;
use kuragecc::lexer::Lexer;
use kuragecc::parser::Parser;

use std::fs;

fn main() {
    let paths = vec![
        "example/main0.tmpc",
        "example/main1.tmpc",
        "example/lexer_error.tmpc",
        "example/parser_error0.tmpc",
        "example/parser_error1.tmpc",
        "example/parser_error2.tmpc",
        "example/parser_error3.tmpc",
        "example/parser_error4.tmpc",
        "example/parser_error5.tmpc",
        "example/parser_error6.tmpc",
        "example/parser_error7.tmpc",
        "example/parser_error8.tmpc",
        "example/parser_error9.tmpc",
        "example/parser_error10.tmpc",
        "example/parser_error11.tmpc",
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
        Ok(tokens) => {
            if !tokens.is_empty() {
                tokens
            } else {
                return;
            }
        }
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
        Err(error) => {
            visualize_parser_error(error, code.as_str());
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
