use kuragecc::ast::visualize_ast;
use kuragecc::codegen::CodeGenerator;
use kuragecc::error::VisualizeError;
use kuragecc::lexer::Lexer;
use kuragecc::parser::Parser;
use kuragecc::semantics::SemanticAnalyzer;

use std::fs;

fn main() {
    let paths = vec![
        // "example/main0.tmpc",
        // "example/main1.tmpc",
        // "example/main2.tmpc",
        // "example/main3.tmpc",
        // "example/main4.tmpc",
        // "example/main5.tmpc",
        // "example/main6.tmpc",
        // "example/main7.tmpc",
        // "example/main8.tmpc",
        // "example/main9.tmpc",
        // "example/main10.tmpc",
        "example/main11.tmpc",
        // "example/lexer_error.tmpc",
        // "example/parser_error0.tmpc",
        // "example/parser_error1.tmpc",
        // "example/parser_error2.tmpc",
        // "example/parser_error3.tmpc",
        // "example/parser_error4.tmpc",
        // "example/parser_error5.tmpc",
        // "example/parser_error6.tmpc",
        // "example/parser_error7.tmpc",
        // "example/parser_error8.tmpc",
        // "example/parser_error9.tmpc",
        // "example/parser_error10.tmpc",
        // "example/parser_error11.tmpc",
        // "example/parser_error12.tmpc",
        // "example/parser_error13.tmpc",
        // "example/parser_error14.tmpc",
        // "example/parser_error15.tmpc",
        // "example/parser_error16.tmpc",
        // "example/parser_error17.tmpc",
        // "example/semantics_error0.tmpc",
        // "example/semantics_error1.tmpc",
        // "example/semantics_error2.tmpc",
        // "example/semantics_error3.tmpc",
    ];
    for path in paths {
        println!("=> {}\n", path);
        compile(path);
    }
}

fn compile(path: &str) {
    let code = fs::read_to_string(path).expect("File Input Error");

    println!("```\n{}```\n", code);

    // Lexer
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
            for e in errors {
                e.visualize_error(code.as_str());
            }
            return;
        }
    };

    if std::env::var("RUST_BACKTRACE").is_ok() {
        for token in tokens.clone() {
            println!("{}", token);
        }
        println!();
    }

    // Parser
    let mut parser = Parser::new(tokens.clone());
    let ast = match parser.parse() {
        Ok(ast) => ast,
        Err(error) => {
            error.visualize_error(code.as_str());
            return;
        }
    };

    if std::env::var("RUST_BACKTRACE").is_ok() {
        println!("=== After parse ===\n");
        visualize_ast(ast.clone());
        println!();
    }

    // Semantic Analyzer
    let mut analyzer = SemanticAnalyzer::new();
    let ast = match analyzer.semantic_analyze(ast.clone()) {
        Ok(new_ast) => new_ast,
        Err(errors) => {
            for e in errors {
                e.visualize_error(code.as_str());
            }
            return;
        }
    };

    if std::env::var("RUST_BACKTRACE").is_ok() {
        println!("=== After semantic-analyze ===\n");
        visualize_ast(ast.clone());
        println!();
    }

    // Code Generator
    let mut generator = CodeGenerator::new(ast);
    let asm = generator.gen_assembly();
    if std::env::var("RUST_BACKTRACE").is_ok() {
        println!("{}", asm);
    }
    println!("{}", asm.to_assembly());
}
