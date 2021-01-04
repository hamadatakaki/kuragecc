use std::collections::HashMap;

pub struct IdentifierManager {
    count: usize,
    book: HashMap<String, String>,
}

impl IdentifierManager {
    pub fn new() -> Self {
        Self {
            count: 1,
            book: HashMap::new(),
        }
    }

    fn new_id(&mut self, name: &str) -> String {
        let c = self.count;
        self.count += 1;
        format!("%val_{}_{}", c, name)
    }

    pub fn get_name(&self, name: &String) -> Option<&String> {
        self.book.get(name)
    }

    pub fn set_name(&mut self, name: String) -> String {
        let id = self.new_id(name.as_str());
        self.book.insert(name, id.clone());
        id
    }
}
