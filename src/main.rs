use kuragecc::ast::visualize_ast;
use kuragecc::codegen::CodeGenerator;
use kuragecc::lexer::Lexer;
use kuragecc::parser::Parser;

// TODO: 一度動くものを書いた後に、look_at系をOptionalに書き直す.

fn main() {
    let code = "return 0;";
    let mut lexer = Lexer::new(code);
    lexer.tokenize();
    let tokens = lexer.tokens;

    let mut parser = Parser::new(tokens.clone());
    parser.parse();
    let ast = parser.ast.unwrap();

    if std::env::var("RUST_BACKTRACE").is_ok() {
        for token in tokens.clone() {
            println!("{}", token);
        }

        visualize_ast(ast.clone());
    }

    let mut generator = CodeGenerator::new(ast);
    generator.gen_code();
    println!("{}", generator.code());
}
