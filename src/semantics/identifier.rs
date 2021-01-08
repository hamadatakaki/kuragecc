use std::collections::HashMap;

#[derive(Debug, Clone)]
pub enum IdentifierKind {
    Function,
    Variable,
}

impl IdentifierKind {
    fn is_func(&self) -> bool {
        match self {
            IdentifierKind::Function => true,
            _ => false,
        }
    }

    fn is_var(&self) -> bool {
        match self {
            IdentifierKind::Variable => true,
            _ => false,
        }
    }
}

#[derive(Debug, Clone)]
pub struct IdentifierInformation {
    kind: IdentifierKind,
    scope: i32,
}

impl IdentifierInformation {
    pub fn new(kind: IdentifierKind, scope: i32) -> Self {
        Self { kind, scope }
    }
}

#[derive(Debug, Clone)]
pub struct IdentifierManager {
    count: usize,
    book: HashMap<String, IdentifierInformation>,
}

impl IdentifierManager {
    pub fn new() -> Self {
        Self {
            count: 1,
            book: HashMap::new(),
        }
    }

    pub fn get_name(&self, name: &String) -> Option<&IdentifierInformation> {
        self.book.get(name)
    }

    pub fn exist_variable(&self, name: &String) -> bool {
        match self.book.get(name) {
            Some(info) => info.kind.is_var(),
            _ => false,
        }
    }

    pub fn exist_function(&self, name: &String) -> bool {
        match self.book.get(name) {
            Some(info) => info.kind.is_func(),
            _ => false,
        }
    }

    pub fn set_name(&mut self, name: String, info: IdentifierInformation) {
        self.book.insert(name, info);
    }
}
