use super::super::ast::types::Type;
use super::super::ast::ASTIdentifier;

#[derive(Debug, Clone)]
pub struct IdentifierInformation {
    id: ASTIdentifier,
}

impl IdentifierInformation {
    pub fn new(id: ASTIdentifier) -> Self {
        Self { id }
    }

    pub fn has_same_name(&self, name: &String) -> bool {
        self.id.get_name() == name.clone()
    }

    pub fn get_type(&self) -> Type {
        self.id.get_type()
    }
}

#[derive(Debug, Clone)]
pub struct IdentifierManager {
    scope_table: Vec<usize>,
    info_stack: Vec<IdentifierInformation>,
}

impl IdentifierManager {
    pub fn new() -> Self {
        Self {
            scope_table: vec![0],
            info_stack: Vec::new(),
        }
    }

    pub fn search_name(&mut self, id: &ASTIdentifier) -> Option<IdentifierInformation> {
        let l = self.info_stack.len() - 1;
        for k in 0..l + 1 {
            if let Some(info) = self.info_stack.get(l - k) {
                if info.has_same_name(&id.get_name()) {
                    return Some(info.clone());
                }
            }
        }
        None
    }

    pub fn push_info(&mut self, info: IdentifierInformation) {
        self.info_stack.push(info);

        match std::env::var("RUST_BACKTRACE") {
            Ok(s) if s.as_str() == "SCOPE" => {
                print!("info: ");
                let s = self
                    .info_stack
                    .iter()
                    .map(|info| info.id.get_name())
                    .collect::<Vec<String>>()
                    .join(", ");
                println!("{}", s);
            }
            _ => {}
        }
    }

    pub fn deepen_scope(&mut self) {
        self.scope_table.push(self.info_stack.len())
    }

    pub fn shallow_scope(&mut self) {
        let height = self.scope_table.pop().unwrap();
        let length = self.info_stack.len();
        for _ in height..length {
            self.info_stack.pop();
        }
    }
}
