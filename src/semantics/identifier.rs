use super::super::ast::{ASTIdentifier, AsSyntaxExpression};
use super::super::types::Type;
use super::super::Location;

#[derive(Debug, Clone)]
pub struct IdentifierInformation {
    name: String,
    cnt: usize,
    id_type: Type,
    location: Location,
}

// name: String, id_type: Type, loc: Location

impl IdentifierInformation {
    pub fn from_id(id: ASTIdentifier, cnt: usize) -> Self {
        let name = id.get_name();
        let id_type = id.get_type();
        let location = id.get_loc();
        Self {
            name,
            cnt,
            id_type,
            location,
        }
    }

    pub fn verbose_name(&self) -> String {
        format!("{}_{}", self.name, self.cnt)
    }

    pub fn has_same_name(&self, name: &String) -> bool {
        &self.name == name
    }

    pub fn get_type(&self) -> Type {
        self.id_type.clone()
    }
}

#[derive(Debug, Clone)]
pub struct IdentifierManager {
    scope_table: Vec<usize>,
    info_stack: Vec<IdentifierInformation>,
    var_count: usize,
}

impl IdentifierManager {
    pub fn new() -> Self {
        Self {
            scope_table: vec![0],
            info_stack: Vec::new(),
            var_count: 0,
        }
    }

    pub fn new_info(&mut self, id: ASTIdentifier) -> IdentifierInformation {
        let info = IdentifierInformation::from_id(id.clone(), self.var_count);
        self.var_count += 1;
        info
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
                    .map(|info| info.name.clone())
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
