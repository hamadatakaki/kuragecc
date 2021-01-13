# kuragecc

Cコンパイラを作ろう！

## srcの構造

```
─── src
    ├── ast.rs
    ├── codegen
    │   ├── code.rs
    │   └── symbol.rs
    ├── codegen.rs
    ├── error
    │   ├── lexer.rs
    │   ├── parser.rs
    │   └── semantics.rs
    ├── error.rs
    ├── lexer.rs
    ├── lib.rs
    ├── main.rs
    ├── parser.rs
    ├── semantics
    │   └── identifier.rs
    ├── semantics.rs
    ├── token
    │   └── literal.rs
    └── token.rs
```
