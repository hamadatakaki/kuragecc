# kuragecc

Cコンパイラを作ろう！

## srcの構造

```
src
├── ast
│   └── types.rs
├── ast.rs
├── codegen
│   ├── code.rs
│   ├── expression.rs
│   └── symbol_table.rs
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

### `ast` および `ast.rs`

抽象構文木の構成要素が含まれる.
特に `types.rs` にはコンパイラ全体を通しての型の情報が含まれる.

### `codegen` および `codegen.rs`

意味解析まで済んだASTからコード生成を行う.
`code.rs` の `Code` という列挙型をLLVM用の中間表現として用い, 最後にアセンブリに変換する.

### `error` および `error.rs`

各パスにおけるエラーを含む. エラー表示についてもここに実装している.

### `lexer.rs`

字句解析を行う.

### `lib.rs`

`Location` などを定義している.

### `main.rs`

コンパイラの実行用コード. 現段階ではデバッグ用途で存在している.

### `parser.rs`

構文解析と `ASTExpr` の型チェックを行う.

### `semantics` および `semantics.rs`

引数の数や未定義関数・変数のチェックなど, 意味解析を行う. 

### `token` および `token.rs`

トークンの構成要素が含まれる.
