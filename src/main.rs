use kuragecc::lexer::Lexer;
use kuragecc::parser::Parser;

// TODO: 一度動くものを書いた後に、look_at系をOptionalに書き直す.

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
