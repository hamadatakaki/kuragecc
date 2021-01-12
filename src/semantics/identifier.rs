use std::collections::HashMap;

// #[derive(Debug, Clone)]
// pub enum IdentifierKind {
//     Function(usize), // paramの数
//     Variable,
// }

// impl IdentifierKind {
//     pub fn is_func(&self) -> bool {
//         match self {
//             IdentifierKind::Function(_) => true,
//             _ => false,
//         }
//     }

//     fn is_var(&self) -> bool {
//         match self {
//             IdentifierKind::Variable => true,
//             _ => false,
//         }
//     }
// }

#[derive(Debug, Clone)]
pub struct IdentifierInformation {
    pub param_size: usize,
    scope: i32,
}

impl IdentifierInformation {
    pub fn new(param_size: usize, scope: i32) -> Self {
        Self { param_size, scope }
    }

    pub fn equal_param_size(&self, n: usize) -> bool {
        self.param_size == n
    }
}

#[derive(Debug, Clone)]
pub struct IdentifierManager {
    book: HashMap<String, IdentifierInformation>,
}

impl IdentifierManager {
    pub fn new() -> Self {
        Self {
            book: HashMap::new(),
        }
    }

    pub fn get_name(&self, name: &String) -> Option<&IdentifierInformation> {
        self.book.get(name)
    }

    pub fn exist(&self, name: &String) -> bool {
        self.book.get(name).is_some()
    }

    pub fn set_name(&mut self, name: String, info: IdentifierInformation) {
        self.book.insert(name, info);
    }
}
