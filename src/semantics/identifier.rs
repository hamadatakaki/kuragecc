// #[derive(Debug, Clone)]
// pub enum IdentifierKind {
//     Function,
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
    name: String,
    pub param_size: usize,
    pub scope: i32,
}

impl IdentifierInformation {
    pub fn new(name: String, param_size: usize, scope: i32) -> Self {
        Self {
            name,
            param_size,
            scope,
        }
    }

    pub fn equal_param_size(&self, n: usize) -> bool {
        self.param_size == n
    }

    pub fn is_same_name(&self, name: &String) -> bool {
        self.name == name.clone()
    }
}

#[derive(Debug, Clone)]
pub struct IdentifierManager {
    table: Vec<usize>,
    stack: Vec<IdentifierInformation>,
}

impl IdentifierManager {
    pub fn new() -> Self {
        Self {
            table: vec![0],
            stack: Vec::new(),
        }
    }

    pub fn search_name(&mut self, name: &String) -> Option<IdentifierInformation> {
        let l = self.stack.len() - 1;
        for k in 0..l + 1 {
            if let Some(info) = self.stack.get(l - k) {
                if info.is_same_name(name) {
                    return Some(info.clone());
                }
            }
        }
        None
    }

    pub fn push_info(&mut self, info: IdentifierInformation) {
        self.stack.push(info);
    }

    pub fn deepen_scope(&mut self) {
        self.table.push(self.stack.len())
    }

    pub fn shallow_scope(&mut self) {
        let height = self.table.pop().unwrap();
        let length = self.stack.len();
        for _ in height..length {
            self.stack.pop();
        }
    }
}
